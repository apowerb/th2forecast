#' @name api_v1_models
NULL

#' Modele naive / snaive pour th2forecast (baseline API v1)
#'
#' @param input_data data.frame d'entrainement avec colonnes `var_date`/`var_target`
#' @param var_target nom de la colonne cible
#' @param var_date nom de la colonne date
#' @param engine "naive" ou "snaive"
#' @param seasonal_period periode saisonniere (utilisee par snaive uniquement)
#' @export
th2_naive_engine <- function(input_data, var_target, var_date, engine = "naive", seasonal_period = "auto") {
  formula <- stats::as.formula(paste(var_target, "~", var_date))
  spec <- parsnip::set_engine(
    modeltime::naive_reg(seasonal_period = seasonal_period),
    engine = engine
  )
  parsnip::fit(spec, formula, data = input_data)
}

API_V1_MAX_ETS_PERIOD <- 24L
API_V1_MIN_CYCLE_CONSISTENCY <- 0.5

#' `forecast::ets()` ignores any seasonality above period 24, so a weekly series
#' (period 52) is fitted without seasonal component. The seasonal part is then
#' taken out by STL and handled apart (`stlm_ets`), but only if the seasonal
#' profile repeats from one cycle to the next: the correlation between the mean
#' profile of the first and of the second half of the cycles must reach
#' `API_V1_MIN_CYCLE_CONSISTENCY` (white noise stays below ~0.45). Without it the
#' forecast stays non seasonal, which is the right answer on noise.
#'
#' @param values numeric vector of the training series
#' @param seasonal_period seasonal period of the frequency
#' @keywords internal
.api_v1_stl_ets_applicable <- function(values, seasonal_period) {
  n <- length(values)
  if (seasonal_period <= API_V1_MAX_ETS_PERIOD || n < 2L * seasonal_period) return(FALSE)
  decomposition <- tryCatch(
    stats::stl(stats::ts(values, frequency = seasonal_period), s.window = "periodic", robust = TRUE),
    error = function(e) NULL
  )
  if (is.null(decomposition)) return(FALSE)
  detrended <- as.numeric(decomposition$time.series[, "seasonal"] + decomposition$time.series[, "remainder"])
  k <- n %/% seasonal_period
  cycles <- matrix(detrended[(n - k * seasonal_period + 1L):n], nrow = k, byrow = TRUE)
  first_half <- colMeans(cycles[seq_len(k %/% 2L), , drop = FALSE])
  second_half <- colMeans(cycles[(k %/% 2L + 1L):k, , drop = FALSE])
  if (stats::sd(first_half) == 0 || stats::sd(second_half) == 0) return(FALSE)
  stats::cor(first_half, second_half) >= API_V1_MIN_CYCLE_CONSISTENCY
}

#' Prophet only switches its yearly seasonality on after 2 years (730 days) of
#' history. For weekly data it is forced from 1.5 years (78 weeks), as the
#' Python engine does; otherwise Prophet's own `"auto"` rule applies.
#' @keywords internal
api_v1_prophet_yearly <- function(seasonal_period, n) {
  if (seasonal_period == 52L && n >= 78L) TRUE else "auto"
}

#' ETS, switched to STL + ETS when the series has a weekly-type seasonality
#' @keywords internal
.api_v1_fit_ets <- function(train_df, target_var, date_var, seasonal_period) {
  if (!.api_v1_stl_ets_applicable(train_df[[target_var]], seasonal_period)) {
    return(th2_ets_engine(train_df, date_var, target_var, fit_model = TRUE))
  }
  formula <- stats::as.formula(paste(target_var, "~", date_var))
  spec <- parsnip::set_engine(
    modeltime::seasonal_reg(seasonal_period_1 = seasonal_period),
    engine = "stlm_ets"
  )
  parsnip::fit(spec, formula, data = train_df)
}

#' @keywords internal
.api_v1_fit_one_model <- function(model_name, train_df, target_var = "value", date_var = "date", seasonal_period = 1L, holidays_calendar = NULL) {
  tryCatch({
    yearly <- api_v1_prophet_yearly(seasonal_period, nrow(train_df))
    fit <- switch(model_name,
      arima   = th2_arima_engine(train_df, target_var, date_var, fit_model = TRUE),
      prophet = if ("is_holiday" %in% names(train_df)) {
        .api_v1_fit_prophet_holidays(train_df, seasonal_yearly = yearly)
      } else {
        th2_prophet_engine(train_df, target_var, date_var, use_holidays = NULL, fit_model = TRUE, seasonal_yearly = yearly)
      },
      ets     = .api_v1_fit_ets(train_df, target_var, date_var, seasonal_period),
      naive   = th2_naive_engine(train_df, target_var, date_var, engine = "naive"),
      snaive  = th2_naive_engine(train_df, target_var, date_var, engine = "snaive", seasonal_period = seasonal_period),
      .api_v1_fit_ml_model(model_name, train_df, holidays_calendar)
    )
    if (is.null(fit) || inherits(fit, "warning")) return(NULL)
    fit
  }, error = function(e) NULL)
}

#' Regle de fiabilite "reliability" (documentee dans docs/API.md)
#'
#' - "unknown" si les metriques de backtest sont indisponibles.
#' - "poor" si le modele ne bat pas la baseline (MASE >= baseline MASE).
#' - "fair" si le modele bat la baseline mais avec peu de points de holdout (< 6)
#'   ou un ratio MASE/baseline superieur a 0.8.
#' - "good" si le modele bat la baseline avec au moins 6 points de holdout et
#'   un ratio MASE/baseline inferieur ou egal a 0.8.
#'
#' @param model_mase MASE du modele retenu (peut etre NA)
#' @param baseline_mase MASE de la baseline (peut etre NA)
#' @param holdout_points nombre de points de holdout utilises pour le backtest
#' @export
api_v1_reliability <- function(model_mase, baseline_mase, holdout_points) {
  if (is.na(model_mase) || is.na(baseline_mase) || is.na(holdout_points) || holdout_points < 2) {
    return("unknown")
  }
  beats <- model_mase < baseline_mase
  if (!beats) return("poor")
  ratio <- if (baseline_mase > 0) model_mase / baseline_mase else 0
  if (holdout_points >= 6 && ratio <= 0.8) "good" else "fair"
}
