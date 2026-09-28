#' @name api_v1_forecast
NULL

CANDIDATE_MODELS <- c("arima", "prophet", "ets")

#' modeltime_accuracy() renvoie mape/smape en points de pourcentage
#' (yardstick::mape()/smape()) ; le contrat attend une fraction (0.08 = 8 %).
#' @keywords internal
.api_v1_pct_to_fraction <- function(x) x / 100

#' @keywords internal
.api_v1_expand_models <- function(models) {
  if ("auto" %in% models) {
    extra <- setdiff(models, c("auto", CANDIDATE_MODELS))
    unique(c(CANDIDATE_MODELS, extra))
  } else {
    unique(models)
  }
}

#' @keywords internal
.api_v1_baseline_name <- function(frequency) {
  if (api_v1_seasonal_period(frequency) > 1L) "snaive" else "naive"
}

#' Prevision + backtest pour une seule serie (deja regularisee/triee)
#'
#' @param df data.frame avec colonnes `date`,`value`
#' @param request requete normalisee (voir `api_v1_validate_request()`)
#' @return liste representant une entree de `series[]` (voir docs/API.md), ou
#'   `list(error = api_v1_error(...))` en cas d'echec d'ajustement de tout modele.
#' @export
api_v1_forecast_one_series <- function(df, request) {
  horizon <- request$horizon
  n <- nrow(df)
  holdout <- max(2L, min(horizon, floor(n * 0.2)))
  holdout <- min(holdout, n - 3L)
  if (holdout < 2L) holdout <- 2L
  train_n <- n - holdout

  splits <- timetk::time_series_split(df, date_var = date, initial = train_n, assess = holdout, cumulative = TRUE)
  train_df <- rsample::training(splits)
  test_df <- rsample::testing(splits)

  seasonal_period <- api_v1_seasonal_period(request$frequency)
  requested_models <- .api_v1_expand_models(request$models)
  baseline_name <- .api_v1_baseline_name(request$frequency)
  models_to_fit <- unique(c(requested_models, baseline_name))

  fits <- stats::setNames(
    lapply(models_to_fit, .api_v1_fit_one_model, train_df = train_df, seasonal_period = seasonal_period),
    models_to_fit
  )
  fits <- Filter(Negate(is.null), fits)

  if (length(fits) == 0) {
    return(list(error = api_v1_error("models", "Aucun modèle n'a pu être entraîné sur cette série.")))
  }

  calib_tbl <- do.call(modeltime::modeltime_table, unname(fits))
  calib_tbl$.model_desc <- names(fits)
  calib_tbl <- modeltime::modeltime_calibrate(calib_tbl, new_data = test_df, quiet = TRUE)

  accuracy_tbl <- tryCatch(
    modeltime::modeltime_accuracy(calib_tbl),
    error = function(e) NULL
  )

  warnings_out <- character(0)

  metric_row <- function(model_name) {
    if (is.null(accuracy_tbl)) return(NULL)
    row <- accuracy_tbl[accuracy_tbl$.model_desc == model_name, , drop = FALSE]
    if (nrow(row) == 0) return(NULL)
    row
  }

  baseline_row <- metric_row(baseline_name)
  baseline_mase <- if (!is.null(baseline_row)) baseline_row$mase[1] else NA_real_

  candidates <- intersect(requested_models, names(fits))
  if (length(candidates) == 0) candidates <- names(fits)

  best_model <- NULL
  best_rmse <- Inf
  for (m in candidates) {
    row <- metric_row(m)
    if (is.null(row) || is.na(row$rmse[1])) next
    if (row$rmse[1] < best_rmse) {
      best_rmse <- row$rmse[1]
      best_model <- m
    }
  }
  if (is.null(best_model)) best_model <- candidates[1]

  winner_row <- metric_row(best_model)
  model_mase <- if (!is.null(winner_row)) winner_row$mase[1] else NA_real_
  holdout_points <- nrow(test_df)

  winner_id <- calib_tbl$.model_id[calib_tbl$.model_desc == best_model][1]

  # Le modele retenu est re-entraine sur toute la serie avant de prevoir : arima
  # et ets prevoient a partir de la fin de leurs donnees d'entrainement, sans
  # regarder les dates demandees. Sans refit, leur prevision serait celle du
  # holdout, datee comme le futur. Les intervalles restent calibres sur les
  # residus du holdout (conformal_split).
  refit_tbl <- tryCatch(
    modeltime::modeltime_refit(calib_tbl[calib_tbl$.model_id == winner_id, ], data = df),
    error = function(e) NULL
  )
  if (is.null(refit_tbl)) {
    return(list(error = api_v1_error("models", sprintf(
      "Impossible de ré-entraîner le modèle '%s' sur toute la série.", best_model
    ))))
  }

  forecast_rows <- NULL
  for (level in request$confidence_levels) {
    fc <- tryCatch(
      modeltime::modeltime_forecast(
        refit_tbl,
        h = horizon,
        actual_data = df,
        conf_interval = level,
        conf_method = "conformal_split",
        keep_data = FALSE
      ),
      error = function(e) NULL
    )
    if (is.null(fc)) {
      warnings_out <- c(warnings_out, sprintf(
        "Intervalle de confiance au niveau %.2f indisponible pour le modèle '%s'.", level, best_model
      ))
      next
    }
    fc <- fc[is.na(fc$.model_id) | fc$.model_id == winner_id, , drop = FALSE]
    lvl_label <- sprintf("%02d", as.integer(round(level * 100)))
    fc <- fc[c(".key", ".index", ".value", ".conf_lo", ".conf_hi")]
    names(fc)[names(fc) == ".conf_lo"] <- paste0("lower_", lvl_label)
    names(fc)[names(fc) == ".conf_hi"] <- paste0("upper_", lvl_label)

    if (is.null(forecast_rows)) {
      forecast_rows <- fc
    } else {
      forecast_rows <- merge(
        forecast_rows, fc[c(".key", ".index", paste0("lower_", lvl_label), paste0("upper_", lvl_label))],
        by = c(".key", ".index"), all.x = TRUE
      )
    }
  }

  if (is.null(forecast_rows)) {
    return(list(error = api_v1_error("models", sprintf(
      "Impossible de produire une prévision pour le modèle '%s'.", best_model
    ))))
  }

  forecast_rows <- forecast_rows[order(forecast_rows$.index), ]
  history_rows <- forecast_rows[forecast_rows$.key == "actual", ]
  future_rows <- forecast_rows[forecast_rows$.key == "prediction", ]

  round_target <- all(df$value == round(df$value), na.rm = TRUE)
  fmt_num <- function(x) if (round_target) round(x) else round(x, 6)

  history <- lapply(seq_len(nrow(history_rows)), function(i) {
    list(date = format(as.Date(history_rows$.index[i]), "%Y-%m-%d"), value = fmt_num(history_rows$.value[i]))
  })

  interval_cols <- setdiff(names(future_rows), c(".key", ".index", ".value"))
  forecast <- lapply(seq_len(nrow(future_rows)), function(i) {
    row <- list(date = format(as.Date(future_rows$.index[i]), "%Y-%m-%d"), value = fmt_num(future_rows$.value[i]))
    for (col in interval_cols) row[[col]] <- fmt_num(future_rows[[col]][i])
    row
  })

  metrics <- if (!is.null(winner_row)) {
    list(
      mape = .api_v1_pct_to_fraction(winner_row$mape[1]), smape = .api_v1_pct_to_fraction(winner_row$smape[1]),
      mase = winner_row$mase[1], rmse = winner_row$rmse[1],
      holdout_points = holdout_points
    )
  } else {
    list(mape = NA, smape = NA, mase = NA, rmse = NA, holdout_points = holdout_points)
  }

  baseline <- list(
    model = baseline_name,
    metrics = if (!is.null(baseline_row)) list(
      mape = .api_v1_pct_to_fraction(baseline_row$mape[1]), smape = .api_v1_pct_to_fraction(baseline_row$smape[1]),
      mase = baseline_row$mase[1], rmse = baseline_row$rmse[1]
    ) else list(mape = NA, smape = NA, mase = NA, rmse = NA)
  )

  beats_baseline <- !is.na(model_mase) && !is.na(baseline_mase) && model_mase < baseline_mase
  reliability <- api_v1_reliability(model_mase, baseline_mase, holdout_points)

  list(
    model = best_model,
    history = history,
    forecast = forecast,
    metrics = metrics,
    baseline = baseline,
    beats_baseline = beats_baseline,
    reliability = reliability,
    warnings = as.list(warnings_out)
  )
}

#' Point d'entree complet de `/v1/forecast` (fonction pure, sans I/O HTTP)
#'
#' @param body corps JSON deserialise
#' @param limits `list(max_rows=,max_series=,max_horizon=)`
#' @export
api_v1_run_forecast <- function(body, limits) {
  t0 <- Sys.time()
  validated <- api_v1_validate_request(body, limits)
  if (!isTRUE(validated$ok)) {
    return(list(
      status_code = validated$status,
      body = api_v1_error_body(validated$errors)
    ))
  }

  req <- validated$request
  df <- req$df
  warnings_out <- character(0)

  frequency <- req$frequency
  if (is.null(frequency)) {
    frequency <- api_v1_detect_frequency(df[[req$date_var]])
    warnings_out <- c(warnings_out, sprintf("Fréquence détectée automatiquement : %s.", frequency))
  }
  req$frequency <- frequency

  group_values <- if (!is.null(req$group_var)) as.character(df[[req$group_var]]) else rep(NA_character_, nrow(df))
  groups <- unique(group_values)

  series_out <- list()
  for (g in groups) {
    idx <- if (is.na(g)) which(is.na(group_values)) else which(group_values == g)
    sub <- data.frame(date = df[[req$date_var]][idx], value = df[[req$target_var]][idx])
    sub <- sub[order(sub$date), ]

    reg <- api_v1_regularize_series(sub, frequency)
    series_warnings <- character(0)
    if (reg$n_padded > 0) {
      series_warnings <- c(series_warnings, sprintf(
        "%d point(s) manquant(s) au pas '%s' comblé(s) par interpolation linéaire.", reg$n_padded, frequency
      ))
    }

    result <- api_v1_forecast_one_series(reg$df, req)
    if (!is.null(result$error)) {
      series_warnings <- c(series_warnings, result$error$message)
      series_out[[length(series_out) + 1]] <- list(
        group = if (is.na(g)) NULL else g,
        model = NA, history = list(), forecast = list(),
        metrics = list(mape = NA, smape = NA, mase = NA, rmse = NA, holdout_points = 0),
        baseline = list(model = NA, metrics = list(mape = NA, smape = NA, mase = NA, rmse = NA)),
        beats_baseline = FALSE, reliability = "unknown",
        warnings = as.list(series_warnings)
      )
      next
    }
    result$group <- if (is.na(g)) NULL else g
    result$warnings <- as.list(c(unlist(result$warnings), series_warnings))
    series_out[[length(series_out) + 1]] <- result
  }

  duration_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000

  list(
    status_code = 200L,
    body = list(
      status = "success",
      api_version = "1",
      frequency = frequency,
      duration_ms = round(duration_ms),
      warnings = as.list(warnings_out),
      series = series_out
    )
  )
}

#' Reponse de `GET /health`
#' @export
api_v1_health <- function() {
  version <- tryCatch(as.character(utils::packageVersion("th2forecast")), error = function(e) "0.0.0")
  list(status = "UP", version = version)
}
