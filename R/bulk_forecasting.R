#' effectue le processus de prévision pour plusieurs séries temporelles avec différents modèles.
#'
#' @param input_data un tableau de données avec une colonne au format date ou datetime
#' @param group_target colonne de regroupement
#' @param target_var variables target pour les prévisions
#' @param date_var nom de la colonne date
#' @param future_forecast temps futur pour les prévisions
#' @param models_list liste des modèles de prévision
#' @param train_split (optionnel) date de split (YYYY-MM-DD). Si fourni, ajuste future_forecast au nombre de dates >= split
#' @param external_data (optionnel; requis pour arimax) données exogènes (même colonne date)
#' @param exogenous_var (optionnel; requis pour arimax) nom(s) des colonnes exogènes
#' @param use_holidays (optionnel) NULL, "in_data" (avec country_column), ou code pays selon tes engines
#' @param country_column (optionnel) colonne pays si use_holidays == "in_data"
#' @param lags (optionnel) booléen pour features retardées dans RF/XGBoost
#' @param path_driver (optionnel) chemin driver DB (si holidays via BDD)
#' @param use_meteo (optionnel) contrôle des features météo pour RF/XGBoost
#'
#' @return forecast_result - renvoie un tableau de données contenant des informations sur les prévisions
#' @export
#' @examples
th2_bulk_forecasting <- function(input_data,
                                 group_target,
                                 target_var,
                                 date_var,
                                 future_forecast,
                                 models_list,
                                 train_split    = NULL,
                                 external_data  = NULL,
                                 exogenous_var  = NULL,
                                 use_holidays   = NULL,
                                 country_column = NULL,
                                 lags           = FALSE,
                                 path_driver    = NULL,
                                 use_meteo      = NULL) {

  # --------------------------- utilitaires & garde-fous ---------------------------
  `%||%` <- function(x, y) if (is.null(x)) y else x

  # Crée des colonnes optionnelles si des bouts du pipeline les attendent (évite "Unknown column")
  .ensure_optional_cols <- function(df) {
    if (!"var_factors" %in% names(df)) df$var_factors <- NA_character_
    df
  }

  # --------------------------- préparation train/test ----------------------------
  if (!is.null(train_split)) {
    train_data <- input_data %>%
      dplyr::filter(input_data[[date_var]] < as.Date(train_split))

    max_date <- max(train_data[[date_var]], na.rm = TRUE)
    if (max_date < as.Date(train_split)) {
      test_data <- input_data
    } else {
      test_data <- input_data %>%
        dplyr::filter(input_data[[date_var]] >= as.Date(train_split)) %>%
        dplyr::group_by(input_data[[date_var]]) %>%
        dplyr::summarise(count = dplyr::n())
      future_forecast <- nrow(test_data)
    }
  } else {
    train_data <- input_data
  }

  # --------------------------- validations ARIMAX --------------------------------
  if ("arimax" %in% models_list) {
    if (is.null(external_data) || is.null(exogenous_var)) {
      return(warning("It is necessary to define the external data for training an ARIMAX model."))
    } else {
      max_date_input <- max(train_data[[date_var]], na.rm = TRUE)
      max_date_exter <- max(external_data[[date_var]], na.rm = TRUE)

      if (max_date_input >= max_date_exter) {
        return(warning("The dates of the external data must be greater than the training data."))
      } else {
        future_data <- external_data %>%
          dplyr::filter(external_data[[date_var]] > max_date_input) %>%
          nrow()
        if (future_data < future_forecast) {
          future_forecast <- future_data
        } else {
          external_data <- external_data[1:(nrow(train_data) + future_forecast), , drop = FALSE]
        }
      }
    }
  }

  # --------------------------- connexion holidays (si besoin) --------------------
  db_conn <- NULL
  if (!is.null(use_holidays)) {
    db_conn <- th2product::connect_to_database(path_driver = path_driver)
    on.exit({
      if (!is.null(db_conn)) DBI::dbDisconnect(db_conn)
    }, add = TRUE)
  }

  # --------------------------- reshape des données -------------------------------
  group_target_output <- group_target <- ifelse(length(group_target) == 0, "all_columns", group_target)

  if (group_target == "all_columns") {
    data_tbl <- train_data %>%
      dplyr::select(!!target_var, !!date_var) %>%
      tidyr::pivot_longer(cols = !!target_var, names_to = "id", values_to = "value") %>%
      dplyr::rename(date := !!date_var)
    group_target <- "id"
    target_var <- "value"
  } else {
    if (!is.null(country_column) && (use_holidays == "in_data")) {
      select_vars <- c(group_target, date_var, target_var, country_column)
    } else {
      select_vars <- c(group_target, date_var, target_var)
    }

    data_tbl <- train_data %>%
      dplyr::select(dplyr::all_of(select_vars)) %>%
      dplyr::rename(id := !!group_target, date := !!date_var)
    group_target <- "id"
  }

  if (!is.null(country_column) && use_holidays == "in_data") {
    data_tbl <- data_tbl %>%
      dplyr::mutate(!!group_target := paste(data_tbl[[group_target]], "_", data_tbl[[country_column]], sep = "")) %>%
      dplyr::group_by_at(dplyr::vars("date", group_target, country_column)) %>%
      dplyr::summarise_at(dplyr::vars(target_var), sum)
  } else {
    data_tbl <- data_tbl %>%
      dplyr::group_by_at(dplyr::vars("date", group_target)) %>%
      dplyr::summarise_at(dplyr::vars(target_var), sum)
  }

  # --------------------------- split imbriqué ------------------------------------
  count_data   <- table(data_tbl[[group_target]])
  column_value <- as.numeric(count_data[1])
  num_ids      <- length(names(count_data[count_data > 1]))

  train_size <- round((nrow(data_tbl) / (num_ids %||% 1)) * 0.8)
  test_size  <- column_value - train_size
  if (is.na(test_size) || test_size < 1) test_size <- 1L  # éviter un test vide

  nested_data_tbl <- data_tbl %>%
    modeltime::extend_timeseries(
      .id_var        = id,
      .date_var      = date,
      .length_future = future_forecast
    ) %>%
    modeltime::nest_timeseries(
      .id_var        = id,
      .length_future = future_forecast
    ) %>%
    modeltime::split_nested_timeseries(
      .length_test = test_size
    )

  # --------------------------- prétraitement série par série ---------------------
  for (i in seq_len(nrow(nested_data_tbl))) {
    list_nestede_data <- nested_data_tbl[i, ]$.actual_data

    if (!is.null(country_column) && use_holidays == "in_data") {
      data_output_t <- preprocessing_data(
        list_nestede_data[[1]] %>% dplyr::select(-!!country_column)
      )[["dataset_clean"]]
      data_output_t <- cbind(data_output_t, list_nestede_data[[1]] %>% dplyr::select(country_column))
    } else {
      data_output_t <- preprocessing_data(list_nestede_data)[["dataset_clean"]]
    }

    # Colonnes optionnelles (évite warnings)
    data_output_t <- .ensure_optional_cols(data_output_t)

    data_output_t[["name_id"]] <- nested_data_tbl[i, 1][[1]]
    nested_data_tbl[i, ]$.actual_data[[1]] <- data_output_t

    future_data <- nested_data_tbl[i, ]$.future_data[[1]]
    future_data <- .ensure_optional_cols(future_data)

    future_data[["name_id"]] <- nested_data_tbl[i, 1][[1]]
    nested_data_tbl[i, ]$.future_data[[1]] <- future_data

    if (!is.null(country_column) && use_holidays == "in_data") {
      nested_data_tbl[i, ]$.future_data[[1]] <- nested_data_tbl[i, ]$.future_data[[1]] %>%
        dplyr::mutate(!!country_column := (list_nestede_data[[1]] %>% dplyr::select(country_column))[[1]][[1]])
    }

    # Alignement ARIMAX (comportement original préservé : exogènes globaux)
    if ("arimax" %in% models_list) {
      list_nestede_data <- as.data.frame(list_nestede_data)
      list_nestede_data <- tibble::as_tibble(train_data)

      min_date <- min(list_nestede_data[[date_var]], na.rm = TRUE)
      max_date <- max(list_nestede_data[[date_var]], na.rm = TRUE)

      exogen_data <- external_data %>%
        dplyr::filter(external_data[[date_var]] >= min_date & external_data[[date_var]] <= max_date) %>%
        dplyr::select(dplyr::all_of(exogenous_var))

      data_output_t[exogenous_var] <- exogen_data
      nested_data_tbl[i, ]$.actual_data[[1]] <- data_output_t

      min_date_f <- min(future_data[[date_var]], na.rm = TRUE)
      max_date_f <- max(future_data[[date_var]], na.rm = TRUE)

      future_exogen_data <- external_data %>%
        dplyr::filter(external_data[[date_var]] >= min_date_f & external_data[[date_var]] <= max_date_f) %>%
        dplyr::select(dplyr::all_of(exogenous_var))

      future_data[exogenous_var] <- future_exogen_data
      nested_data_tbl[i, ]$.future_data[[1]] <- future_data
    }
  }

  nested_data <- modeltime::extract_nested_train_split(nested_data_tbl)

  # --------------------------- entraînement modèles ------------------------------
  target_var <- tolower(target_var) # (préserve le comportement original)
  allow_par  <- FALSE
  list_output_models <- list()

  # param. holidays dérivé si besoin
  res_bh <- if (!is.null(use_holidays)) {
    if (use_holidays != "in_data") use_holidays else country_column
  } else {
    NULL
  }

  for (model in models_list) {
    training_model <- NULL
    label_model    <- ""

    if (model == "arima") {
      training_model <- th2_arima_engine(nested_data, target_var, "date", fit_model = "bulk")$fit
      label_model <- "model_arima"

    } else if (model == "prophet") {
      training_model <- th2_prophet_engine(nested_data, target_var, "date",
                                           use_holidays = use_holidays,
                                           fit_model = "bulk", db_conn = db_conn)$fit
      label_model <- "model_prophet"

    } else if (model == "lr") {
      training_model <- th2_linear_engine(nested_data, target_var, "date", fit_model = "bulk")$fit
      label_model <- "model_lm"

    } else if (model == "mars") {
      training_model <- th2_mars_engine(nested_data, target_var, "date", fit_model = "bulk")$fit
      label_model <- "model_mars"

    } else if (model == "random_forest") {
      if (!is.null(use_holidays)) {
        res_bh <- ifelse(use_holidays != "in_data", use_holidays, country_column)
      } else {
        res_bh <- use_holidays
      }
      training_model <- th2_random_forest_engine(nested_data, target_var,
                                                 use_holidays = res_bh,
                                                 use_meteo    = use_meteo,
                                                 fit_model    = "bulk",
                                                 all_data     = nested_data_tbl,
                                                 lags         = lags,
                                                 db_conn      = db_conn)$fit
      label_model <- "model_random_forest"

    } else if (model == "xgboost") {
      if (!is.null(use_holidays)) {
        res_bh <- ifelse(use_holidays != "in_data", use_holidays, country_column)
      } else {
        res_bh <- use_holidays
      }
      training_model <- th2_xgboost_engine(nested_data, "date", target_var,
                                           use_holidays = res_bh,
                                           use_meteo    = use_meteo,
                                           fit_model    = "bulk",
                                           all_data     = nested_data_tbl,
                                           lags         = lags,
                                           db_conn      = db_conn)$fit
      label_model <- "model_xgboost"

    } else if (model == "arimax") {
      training_model <- th2_arimax_engine(nested_data, "date", target_var,
                                          use_holidays = res_bh,
                                          fit_model    = "bulk",
                                          external_data = external_data,
                                          exogenous_var = exogenous_var)$fit
      label_model <- "arimax"

    } else {
      # inconnu -> on ignore mais on ne casse pas
      next
    }

    list_output_models[[label_model]] <- training_model
  }

  # Fit imbriqué
  nested_modeltime_tbl <- do.call(
    modeltime::modeltime_nested_fit,
    c(
      list(nested_data = nested_data_tbl),
      model_list = list_output_models,
      list(control = modeltime::control_nested_fit(
        allow_par = allow_par, verbose = TRUE, cores = -1,
        packages = "tidymodels, parsnip, modeltime, dplyr, stats, lubridate, timetk"
      ))
    )
  )

  # --------------------------- sélection "best" métrique robuste ------------------
  acc_all <- modeltime::extract_nested_test_accuracy(nested_modeltime_tbl)

  metrics_pref  <- c("rmse","mae","mape","smape","mase","rmsle","rsq","rsq_trad")
  metric_main   <- NA_character_
  minimize_main <- TRUE

  if (nrow(acc_all) == 0) {
    # aucun test calculé -> on garde la table telle quelle
    best_nested_modeltime_tbl     <- nested_modeltime_tbl
    mae_best_nested_modeltime_tbl <- NULL
    rsq_nested_modeltime_tbl      <- NULL

  } else {
    if (".metric" %in% names(acc_all)) {
      metrics_avail <- unique(stats::na.omit(as.character(acc_all$.metric)))
    } else {
      metrics_avail <- intersect(metrics_pref, names(acc_all))
    }
    metrics_avail <- intersect(metrics_avail, metrics_pref)

    if (length(metrics_avail) == 0) {
      best_nested_modeltime_tbl     <- nested_modeltime_tbl
      mae_best_nested_modeltime_tbl <- NULL
      rsq_nested_modeltime_tbl      <- NULL
    } else {
      ix <- match(TRUE, metrics_pref %in% metrics_avail)
      metric_main   <- if (!is.na(ix)) metrics_pref[ix] else metrics_avail[1]
      minimize_main <- !(metric_main %in% c("rsq","rsq_trad"))

      best_nested_modeltime_tbl <- modeltime::modeltime_nested_select_best(
        nested_modeltime_tbl,
        metric                = metric_main,
        minimize              = minimize_main,
        filter_test_forecasts = TRUE
      )

      mae_best_nested_modeltime_tbl <- if ("mae" %in% metrics_avail)
        modeltime::modeltime_nested_select_best(nested_modeltime_tbl, metric = "mae", minimize = TRUE,  filter_test_forecasts = TRUE) else NULL

      rsq_metric <- if ("rsq" %in% metrics_avail) "rsq" else if ("rsq_trad" %in% metrics_avail) "rsq_trad" else NA_character_
      rsq_nested_modeltime_tbl <- if (!is.na(rsq_metric))
        modeltime::modeltime_nested_select_best(nested_modeltime_tbl, metric = rsq_metric, minimize = FALSE, filter_test_forecasts = TRUE) else NULL
    }
  }

  # Refit final
  nested_modeltime_refit_tbl <- nested_modeltime_tbl %>%
    modeltime::modeltime_nested_refit()

  # --------------------------- accuracy & outputs --------------------------------
  accuracy_test <- best_nested_modeltime_tbl %>%
    modeltime::extract_nested_test_accuracy()

  cols_keep <- intersect(
    c("id",".model_id",".model_desc",".type",
      "rmse","mae","mape","smape","mase","rmsle","rsq","rsq_trad",
      ".metric",".estimate"),
    names(accuracy_test)
  )
  if (length(cols_keep) > 0) {
    accuracy_test <- dplyr::select(accuracy_test, dplyr::all_of(cols_keep))
  }

forecast_result <- nested_modeltime_refit_tbl %>%
    modeltime::extract_nested_future_forecast(.include_actual = FALSE) %>%
    dplyr::mutate(as_of = Sys.Date()) %>%
    dplyr::mutate(start_date = min(input_data[[date_var]], na.rm = TRUE)) %>%
    dplyr::mutate(end_date   = max(input_data[[date_var]], na.rm = TRUE))
  
  # Vérifier si la colonne 'id' existe avant de renommer
  if ("id" %in% colnames(forecast_result)) {
    forecast_result <- forecast_result %>%
      dplyr::rename(!!group_target_output := id) %>%
      dplyr::mutate(accuracy = list(accuracy_test))
  } else {
    # Si tous les modèles ont échoué, retourner un dataframe vide avec la structure attendue
    warning("All models failed during training. No forecasts were generated.")
    forecast_result <- tibble::tibble(
      !!group_target_output := character(0),
      .model_id = integer(0),
      .model_desc = character(0),
      .key = character(0),
      .index = as.POSIXct(character(0)),
      .value = numeric(0),
      as_of = as.Date(character(0)),
      start_date = as.POSIXct(character(0)),
      end_date = as.POSIXct(character(0))
    )
  }

  # Arrondi si la cible est entière (garde-fou: après tolower)
  if (!is.null(input_data[[target_var]]) &&
      all(input_data[[target_var]] == as.integer(input_data[[target_var]]), na.rm = TRUE)) {
    if (".value"   %in% names(forecast_result)) forecast_result$.value   <- round(forecast_result$.value)
    if (".conf_lo" %in% names(forecast_result)) forecast_result$.conf_lo <- round(forecast_result$.conf_lo)
    if (".conf_hi" %in% names(forecast_result)) forecast_result$.conf_hi <- round(forecast_result$.conf_hi)
  }

  return(forecast_result)
}
