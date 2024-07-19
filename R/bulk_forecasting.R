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
th2_bulk_forecasting <- function(input_data, group_target, target_var, date_var, future_forecast, models_list, train_split = NULL, external_data = NULL, exogenous_var = NULL, use_holidays = NULL, country_column = NULL, lags = FALSE, path_driver = NULL, use_meteo = NULL, spark_conection = NULL) {
  allow_par <- FALSE
  if (!is.null(spark_conection)) {
    allow_par <- TRUE
    modeltime::parallel_start(spark_conection, .method = "spark")
  }

  list_output_models <- list()

  if (!is.null(train_split)) {
    train_data <- input_data %>%
      dplyr::filter(input_data[[date_var]] < as.Date(train_split))

    test_data <- input_data %>%
      dplyr::filter(input_data[[date_var]] >= as.Date(train_split))

    test_data <- test_data %>%
      dplyr::group_by(test_data[[date_var]]) %>%
      dplyr::summarise(count = dplyr::n())

    future_forecast <- nrow(test_data)
  } else {
    train_data <- input_data
  }

  if ("arimax" %in% models_list) {
    if (is.null(external_data) || is.null(exogenous_var)) {
      return(warning("It is necessary to define the external data for training an ARIMAX model."))
    } else {
      max_date_input <- max(train_data[[date_var]])
      max_date_exter <- max(external_data[[date_var]])

      if (max_date_input >= max_date_exter) {
        return(warning("The dates of the external data must be greater than the training data."))
      } else {
        future_data <- external_data %>%
          filter(external_data[[date_var]] > max_date_input) %>%
          nrow()
        if (future_data < future_forecast) {
          future_forecast <- future_data
        } else {
          external_data <- external_data[1:(nrow(train_data) + future_forecast), ]
        }
      }
    }
  }

  db_conn <- NULL
  if (!is.null(use_holidays)) {
    db_conn <- th2product::connect_to_database(
      path_driver = path_driver
    )
  }

  group_target_output <- group_target
  if (group_target == "all_columns") {
    data_tbl <- train_data %>%
      tidyr::pivot_longer(!date_var, names_to = "id", values_to = target_var) %>%
      rename(date := !!date_var)
    group_target <- "id"
  } else {
    if (!is.null(country_column) && (use_holidays == "in_data")) {
      select_vars <- c(group_target, date_var, target_var, country_column)
    } else {
      select_vars <- c(group_target, date_var, target_var)
    }

    data_tbl <- train_data %>%
      dplyr::select(all_of(select_vars)) %>%
      rename(id := !!group_target, date := !!date_var)
    group_target <- "id"
    # data_tbl <- data_tbl %>%
    #   mutate(id_group = paste(store_nbr, family, sep = "_"))
  }

  if (!is.null(country_column) && use_holidays == "in_data") {
    data_tbl <- data_tbl %>%
      dplyr::mutate(!!group_target := paste(data_tbl[[group_target]], "_", data_tbl[[country_column]], sep = ""))


    data_tbl <- data_tbl %>%
      dplyr::group_by_at(vars("date", group_target, country_column)) %>%
      dplyr::summarise_at(vars(target_var), sum)
  } else {
    data_tbl <- data_tbl %>%
      dplyr::group_by_at(vars("date", group_target)) %>%
      dplyr::summarise_at(vars(target_var), sum)
  }

  count_data <- table(data_tbl[[group_target]])
  column_value <- as.numeric(count_data[1])
  num_ids <- length(names(count_data[count_data > 1]))

  train_size <- round((nrow(data_tbl) / num_ids) * 0.8)
  test_size <- column_value - train_size

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

  print(nested_data_tbl)

  for (i in 1:nrow(nested_data_tbl)) {
    list_nestede_data <- nested_data_tbl[i, ]$.actual_data
    if (!is.null(country_column) && use_holidays == "in_data") {
      data_output_t <- preprocessing_data(list_nestede_data[[1]] %>% dplyr::select(-!!country_column))[["dataset_clean"]]
      data_output_t <- cbind(data_output_t, list_nestede_data[[1]] %>% dplyr::select(country_column))
    } else {
      data_output_t <- preprocessing_data(list_nestede_data)[["dataset_clean"]]
    }

    data_output_t["name_id"] <- nested_data_tbl[i, 1]
    nested_data_tbl[i, ]$.actual_data[[1]] <- data_output_t

    future_data <- nested_data_tbl[i, ]$.future_data[[1]]

    future_data["name_id"] <- nested_data_tbl[i, 1]
    nested_data_tbl[i, ]$.future_data[[1]] <- future_data

    if (!is.null(country_column) && use_holidays == "in_data") {
      nested_data_tbl[i, ]$.future_data[[1]] <- nested_data_tbl[i, ]$.future_data[[1]] %>%
        dplyr::mutate(!!country_column := (list_nestede_data[[1]] %>% dplyr::select(country_column))[[1]][[1]])
    }

    if ("arimax" %in% models_list) {
      list_nestede_data <- as.data.frame(list_nestede_data)
      list_nestede_data <- tibble::as_tibble(train_data)
      min_date <- min(list_nestede_data[[date_var]])
      max_date <- max(list_nestede_data[[date_var]])

      exogen_data <- external_data %>%
        filter(external_data[[date_var]] >= min_date & external_data[[date_var]] <= max_date) %>%
        select(exogenous_var)

      data_output_t[exogenous_var] <- exogen_data
      nested_data_tbl[i, ]$.actual_data[[1]] <- data_output_t

      min_date_f <- min(future_data[[date_var]])
      max_date_f <- max(future_data[[date_var]])

      future_exogen_data <- external_data %>%
        filter(external_data[[date_var]] >= min_date_f & external_data[[date_var]] <= max_date_f) %>%
        select(exogenous_var)

      future_data[exogenous_var] <- future_exogen_data
      nested_data_tbl[i, ]$.future_data[[1]] <- future_data
    }
  }

  nested_data <- modeltime::extract_nested_train_split(nested_data_tbl)

  target_var <- tolower(target_var)
  # browser()

  for (model in models_list) {
    # tune -- for
    tuning_param <- ""
    training_model <- NULL
    label_model <- ""

    if (model == "arima") {
      training_model <- th2_arima_engine(nested_data, target_var, "date", fit_model = "bulk")$fit
      label_model <- "model_arima"
    } else if (model == "prophet") {
      training_model <- th2_prophet_engine(nested_data, target_var, "date", use_holidays = use_holidays, fit_model = "bulk", db_conn = db_conn)$fit
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

      training_model <- th2_random_forest_engine(nested_data, target_var, use_holidays = res_bh, use_meteo = use_meteo, fit_model = "bulk", all_data = nested_data_tbl, lags = lags, db_conn = db_conn)$fit
      label_model <- "model_random_forest"
    } else if (model == "xgboost") {
      if (!is.null(use_holidays)) {
        res_bh <- ifelse(use_holidays != "in_data", use_holidays, country_column)
      } else {
        res_bh <- use_holidays
      }

      training_model <- th2_xgboost_engine(nested_data, "date", target_var, use_holidays = res_bh, use_meteo = use_meteo, fit_model = "bulk", all_data = nested_data_tbl, lags = lags, db_conn = db_conn)$fit
      label_model <- "model_xgboost"
    } else if (model == "arimax") {
      training_model <- th2_arimax_engine(nested_data, "date", target_var, use_holidays = res_bh, fit_model = "bulk", external_data = external_data, exogenous_var = exogenous_var)$fit
      label_model <- "arimax"
    } else {
      error_models <- c(NULL, model)
    }

    list_output_models[[label_model]] <- training_model
  }



  nested_modeltime_tbl <- do.call(
    modeltime::modeltime_nested_fit,
    c(
      list(nested_data = nested_data_tbl),
      list_output_models,
      list(control = modeltime::control_nested_fit(allow_par = allow_par, verbose = TRUE, cores = -1, packages = "tidymodels, parsnip, modeltime, dplyr, stats, lubridate, timetk"))
    )
  )


  # nested_modeltime_tbl <- modeltime::modeltime_nested_fit

  best_nested_modeltime_tbl <- nested_modeltime_tbl %>%
    modeltime::modeltime_nested_select_best(
      metric                = "rmse",
      minimize              = TRUE,
      filter_test_forecasts = TRUE
    )

  # mae_best_nested_modeltime_tbl <- nested_modeltime_tbl %>%
  #   modeltime::modeltime_nested_select_best(
  #     metric                = "mae",
  #     minimize              = TRUE,
  #     filter_test_forecasts = TRUE
  #   )

  # rsq_nested_modeltime_tbl <- nested_modeltime_tbl %>%
  #   modeltime::modeltime_nested_select_best(
  #     metric                = "rsq",
  #     minimize              = FALSE,
  #     filter_test_forecasts = TRUE
  #   )


  nested_modeltime_refit_tbl <- nested_modeltime_tbl %>%
    modeltime::modeltime_nested_refit(
      control = modeltime::control_nested_refit(allow_par = allow_par, verbose = TRUE, cores = -1, packages = "tidymodels, parsnip, modeltime, dplyr, stats, lubridate, timetk")
    )

  accuracy_test <- best_nested_modeltime_tbl %>%
    modeltime::extract_nested_test_accuracy() %>%
    dplyr::select(id, .model_id, .model_desc, .type, mae, rmse, rsq)

  forecast_result <- nested_modeltime_refit_tbl %>%
    modeltime::extract_nested_future_forecast(
      .include_actual = FALSE
    ) %>%
    dplyr::mutate(as_of = Sys.Date()) %>%
    dplyr::mutate(start_date = min(input_data[[date_var]])) %>%
    dplyr::mutate(end_date = max(input_data[[date_var]])) %>%
    dplyr::rename(!!group_target_output := id)

  forecast_result <- forecast_result %>%
    dplyr::mutate(accuracy = list(accuracy_test)) %>%
    dplyr::mutate(best_rmse = list(best_nested_modeltime_tbl$.modeltime_tables[[1]]$.model_desc)) #%>%
    # dplyr::mutate(best_mae = list(mae_best_nested_modeltime_tbl$.modeltime_tables[[1]]$.model_desc)) %>%
    # dplyr::mutate(best_rsq = list(rsq_nested_modeltime_tbl$.modeltime_tables[[1]]$.model_desc))

  if (!is.null(use_holidays)) DBI::dbDisconnect(db_conn)

  if (all(input_data[[target_var]] == as.integer(input_data[[target_var]]))) {
    forecast_result$.value <- round(forecast_result$.value)
    forecast_result$.conf_lo <- round(forecast_result$.conf_lo)
    forecast_result$.conf_hi <- round(forecast_result$.conf_hi)
  }

  if (!is.null(spark_conection)) {
    # Unregisters the Spark Backend
    modeltime::parallel_stop()

    # Disconnects Spark
    # sparklyr::spark_disconnect_all()
  }

  return(forecast_result)
}
