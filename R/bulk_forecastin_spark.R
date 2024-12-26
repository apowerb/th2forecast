#' effectue le processus de prévision pour plusieurs séries temporelles avec différents modèles.
#'
#' @param input_data un tableau de données avec une colonne au format date ou datetime
#' @param group_target colonne de regroupement
#' @param target_var variables target pour les prévisions
#' @param date_var nom de la colonne date
#' @param future_forecast temps futur pour les prévisions
#' @param models_list liste des modèles de prévision
#'
#' @return forecast_result - renvoie un tableau de données contenant des informations sur les prévisions
#' @export
#' @examples
th2_bulk_forecasting_spark <- function(input_data, group_target, target_var, date_var, future_forecast, models_list, train_split = NULL, external_data = NULL, exogenous_var = NULL, use_holidays = NULL, country_column = NULL, lags = FALSE, path_driver = NULL, use_meteo = NULL, spark_conection = NULL, tuning = FALSE) {
  # allow_par <- FALSE
  # if (!is.null(spark_conection)) {
  #   allow_par <- TRUE
  #   modeltime::parallel_start(spark_conection, .method = "spark")
  # }

  dataframe_input <- input_data

  if (!is.null(country_column) && use_holidays == "in_data") {
    dataframe_input <- dataframe_input %>%
      dplyr::mutate(!!group_target := paste(dataframe_input[[group_target]], "_", dataframe_input[[country_column]], sep = ""))

    dataframe_input <- dataframe_input %>%
      dplyr::group_by_at(dplyr::vars(date_var, group_target, country_column)) %>%
      dplyr::summarise_at(dplyr::vars(target_var), sum)
  } else {
    dataframe_input <- dataframe_input %>%
      dplyr::group_by_at(dplyr::vars(date_var, group_target)) %>%
      dplyr::summarise_at(dplyr::vars(target_var), sum)
  }

  data_clean <- preprocessing_data(dataframe_input %>% dplyr::select(target_var, date_var))$dataset_clean # [["dataset_clean"]]

  dataframe_input[[target_var]] <- data_clean[[target_var]]

  list_output_models <- list()

  if (!is.null(train_split)) {
    train_data <- dataframe_input %>%
      dplyr::filter(.data[[date_var]] < as.Date(train_split))

    test_data <- dataframe_input %>%
      dplyr::filter(.data[[date_var]] >= as.Date(train_split)) %>%
      dplyr::rename(date := !!date_var)

    # test_data <- test_data %>%
    #   dplyr::group_by(test_data[[date_var]]) %>%
    #   dplyr::summarise(count = dplyr::n())

    future_forecast <- nrow(test_data)
  } else {
    train_data <- dataframe_input
  }

  db_conn <- NULL
  if (!is.null(use_holidays)) {
    db_conn <- th2product::connect_to_database(
      path_driver = path_driver
    )
  }

  group_target_output <- group_target
  if (group_target == "all_columns") {
    train_data <- train_data %>%
      tidyr::pivot_longer(!date_var, names_to = "id", values_to = target_var) %>%
      dplyr::rename(date := !!date_var)
    group_target <- "id"
  } else {
    if (!is.null(country_column) && (use_holidays == "in_data")) {
      select_vars <- c(group_target, date_var, target_var, country_column)
    } else {
      select_vars <- c(group_target, date_var, target_var)
    }

    train_data <- train_data %>%
      dplyr::select(all_of(select_vars)) %>%
      dplyr::rename(id := !!group_target, date := !!date_var)
    group_target <- "id"
    # data_tbl <- data_tbl %>%
    #   mutate(id_group = paste(store_nbr, family, sep = "_"))
  }



  # train_data <- train_data %>%
  #   dplyr::group_by_at(dplyr::vars("date", group_target)) %>%
  #   dplyr::summarise_at(dplyr::vars(target_var), sum)


  if (tuning == TRUE) {
    resample_data <- th2forecast::th2_resamples(train_data, "date")
  }

  count_data <- table(train_data[[group_target]])
  column_value <- as.numeric(count_data[1])
  num_ids <- length(names(count_data[count_data > 1]))

  train_size <- round((nrow(train_data) / num_ids) * 0.8)
  test_size <- column_value - train_size

  dataset_train_test <- th2forecast::split_dataset(input_data = train_data, var_time = "date", var_target = target_var, assess = test_size)[["traintest"]]

  error_models <- list()

  for (model in models_list) {
    if (model == "arima") {
      # if (tuning == TRUE) {
      #   training_model <- th2_arima_engine(rsample::training(dataset_train_test), target_var, date_var, fit_model = FALSE)
      #   formula <- as.formula(paste(target_var, "~", date_var))
      #   # tuning_param <- list(non_seasonal_ar = seq(1, 2, 3), non_seasonal_differences = seq(0, 1, 2), non_seasonal_ma = seq(1, 2, 3))
      #   tuning_param <- dials::grid_regular(modeltime::non_seasonal_ar(), modeltime::non_seasonal_differences(), modeltime::non_seasonal_ma(), levels = 3)
      #   label_model <- "model_arima"
      # }else{
      model_arima <- th2forecast::th2_arima_engine(rsample::training(dataset_train_test), target_var, "date", fit_model = TRUE)
      list_output_models[["model_arima"]] <- model_arima
      # }
    } else if (model == "prophet") {
      if (tuning == TRUE) {
        training_model <- th2_prophet_engine(rsample::training(dataset_train_test), target_var, "date", engine = "prophet", use_holidays = use_holidays, db_conn = db_conn, fit_model = FALSE)
        formula <- as.formula(paste(target_var, "~", "date"))
        tuning_param <- dials::grid_regular(modeltime::changepoint_num(c(10L, 50L)), modeltime::changepoint_range(), levels = 3)
        label_model <- "model_prophet"
      } else {
        model_prophet <- th2forecast::th2_prophet_engine(rsample::training(dataset_train_test), target_var, "date", fit_model = TRUE, use_holidays = use_holidays, db_conn = db_conn)
        list_output_models[["model_prophet"]] <- model_prophet
      }
    } else if (model == "lr") {
      # if (tuning == TRUE) {
      #   training_model <- th2_linear_engine(rsample::training(dataset_train_test), target_var, date_var, fit_model = FALSE)
      #   # formula <- as.formula(paste(target_var, "~", "as.numeric(",date_var,") + factor(month(",date_var,", label = TRUE), ordered = FALSE)"))
      #   formula <- as.formula(paste(target_var, "~", date_var))
      #   tuning_param <- FALSE
      #   label_model <- "model_lm"
      # }else{
      model_lm <- th2forecast::th2_linear_engine(rsample::training(dataset_train_test), target_var, "date", fit_model = TRUE)
      list_output_models[["model_lm"]] <- model_lm
      # }
    } else if (model == "mars") {
      if (tuning == TRUE) {
        training_model <- th2_mars_engine(rsample::training(dataset_train_test), target_var, "date", fit_model = FALSE)
        formula <- as.formula(paste(target_var, "~", "date"))
        tuning_param <- dials::grid_regular(dials::num_terms(c(5L, 20L)), dials::prod_degree(c(2L, 10L)), levels = 3)
        label_model <- "model_mars"
      } else {
        model_mars <- th2forecast::th2_mars_engine(rsample::training(dataset_train_test), target_var, "date", fit_model = TRUE)
        list_output_models[["model_mars"]] <- model_mars
      }
    } else if (model == "random_forest") {
      if (tuning == TRUE) {
        training_model <- th2_random_forest_engine(rsample::training(dataset_train_test) %>% dplyr::select(-group_target), target_var, use_holidays = use_holidays, db_conn = db_conn, use_meteo = use_meteo, lags = lags, fit_model = FALSE, all_data = train_data)
        formula <- as.formula(paste(target_var, "~ ."))
        tuning_param <- dials::grid_regular(dials::trees(), dials::min_n(), levels = 3)
        label_model <- "model_random_forest"
      } else {
        model_random_forest <- th2forecast::th2_random_forest_engine(rsample::training(dataset_train_test) %>% dplyr::select(-group_target), target_var, use_holidays = use_holidays, db_conn = db_conn, use_meteo = use_meteo, lags = lags, fit_model = TRUE, all_data = train_data)
        list_output_models[["model_random_forest"]] <- model_random_forest
      }
    } else if (model == "xgboost") {
      if (tuning == TRUE) {
        training_model <- th2_xgboost_engine(rsample::training(dataset_train_test) %>% dplyr::select(-group_target), "date", target_var, use_holidays = use_holidays, use_meteo = use_meteo, db_conn = db_conn, lags = lags, fit_model = FALSE, all_data = train_data)
        formula <- as.formula(paste(target_var, "~ ."))
        tuning_param <- dials::grid_regular(dials::trees(), dials::min_n(), dials::learn_rate(), levels = 3)
        label_model <- "model_xgboost"
      } else {
        model_xgboost <- th2forecast::th2_xgboost_engine(rsample::training(dataset_train_test) %>% dplyr::select(-group_target), "date", target_var, use_holidays = use_holidays, use_meteo = use_meteo, db_conn = db_conn, lags = lags, fit_model = TRUE, all_data = train_data)
        list_output_models[["model_xgboost"]] <- model_xgboost
      }
    } else if (model == "ets") {
      model_ets <- th2forecast::th2_ets_engine(rsample::training(dataset_train_test), "date", target_var, fit_model = TRUE)
      list_output_models[["model_ets"]] <- model_ets
    } else {
      error_models <- c(error_models, model)
    }

    if (tuning == TRUE & !(model == "arima" || model == "lr" || model == "ets")) {
      best_params <- th2forecast::th2_tune_model(resample_data, training_model$fit, tuning_param)

      fitted_model <- training_model$fit %>%
        tune::finalize_workflow(best_params) %>%
        parsnip::fit(data = rsample::training(dataset_train_test) %>% dplyr::select(-group_target))

      list_output_models[[label_model]] <- fitted_model
    }
  }

  if (length(error_models) > 0) {
    return(warning(paste("Models not found:", error_models)))
  }

  models_trained <- do.call(modeltime_table, list_output_models)

  models_evaluated <- model_evaluation(dataset_train_test, models_trained)
  table_performance <- models_evaluated$accuracy_models
  models_results <- models_evaluated[["model_calibrated"]]

  if (length(models_list) > 1) {
    ensemble_fit <- models_trained %>%
      modeltime.ensemble::ensemble_average(type = "mean")

    ensemble_calibration_tbl <- modeltime_table(ensemble_fit) %>%
      modeltime::modeltime_calibrate(rsample::testing(dataset_train_test), quiet = FALSE) %>%
      dplyr::mutate(.model_id = length(models_list) + 1)

    ensemble_accuracy <- ensemble_calibration_tbl %>% modeltime::modeltime_accuracy(metric_set = yardstick::metric_set(yardstick::mae, yardstick::rmse, yardstick::rsq))

    models_results <- bind_rows(models_results, ensemble_calibration_tbl)
    table_performance <- bind_rows(table_performance, ensemble_accuracy)
  }

  df_models_evaluated <- models_results %>%
    modeltime::modeltime_forecast(
      new_data = rsample::testing(dataset_train_test)
    )

  model_refit <- models_results %>%
    modeltime::modeltime_refit(data = train_data)

  models_predictions <- model_refit %>%
    modeltime::modeltime_forecast(
      # actual_data = input_data,
      new_data = test_data
    )

  models_predictions$.model_desc <- ifelse(grepl("ARIMA", models_predictions$.model_desc), "ARIMA", models_predictions$.model_desc)
  models_predictions$.model_desc <- ifelse(grepl("ETS", models_predictions$.model_desc), "ETS", models_predictions$.model_desc)
  models_predictions$.model_desc <- ifelse(grepl("ENSEMBLE", models_predictions$.model_desc), "TH2ENSEMBLE", models_predictions$.model_desc)


  table_performance$.model_desc <- ifelse(grepl("ARIMA", table_performance$.model_desc), "ARIMA", table_performance$.model_desc)
  table_performance$.model_desc <- ifelse(grepl("ETS", table_performance$.model_desc), "ETS", table_performance$.model_desc)
  table_performance$.model_desc <- ifelse(grepl("ENSEMBLE", table_performance$.model_desc), "TH2ENSEMBLE", table_performance$.model_desc)

  models_predictions <- models_predictions %>%
    dplyr::mutate(as_of = Sys.Date()) %>%
    dplyr::mutate(start_date = min(input_data[[date_var]])) %>%
    dplyr::mutate(end_date = max(input_data[[date_var]])) %>%
    dplyr::mutate(accuracy = list(table_performance))

  if (!is.null(use_holidays)) DBI::dbDisconnect(db_conn)

  return(models_predictions)
}
