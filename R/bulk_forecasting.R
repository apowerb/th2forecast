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
th2_bulk_forecasting <- function(input_data, group_target, target_var, date_var, future_forecast, models_list, use_holidays = TRUE, lags = FALSE) {
  list_output_models <- list()

  if (group_target == "all_columns") {
    data_tbl <- input_data %>%
      tidyr::pivot_longer(!date_var, names_to = "id", values_to = target_var) %>%
      rename(date = date_var)
    group_target <- "id"
  } else {
    select_vars <- c(group_target, date_var, target_var)

    data_tbl <- input_data %>%
      dplyr::select(all_of(select_vars)) %>%
      rename(id = group_target, date = date_var)
    group_target <- "id"
    # data_tbl <- data_tbl %>%
    #   mutate(id_group = paste(store_nbr, family, sep = "_"))
  }

  data_tbl <- data_tbl %>%
    dplyr::group_by_at(vars("date", group_target)) %>%
    dplyr::summarise_at(vars(target_var), sum)

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

  for (i in 1:nrow(nested_data_tbl)) {
    list_nestede_data <- nested_data_tbl[i, ]$.actual_data
    data_output_t <- preprocessing_data(list_nestede_data)[["dataset_clean"]]

    data_output_t["name_id"] <- nested_data_tbl[i, 1]
    nested_data_tbl[i, ]$.actual_data[[1]] <- data_output_t

    future_data <- nested_data_tbl[i, ]$.future_data[[1]]
    future_data["name_id"] <- nested_data_tbl[i, 1]

    nested_data_tbl[i, ]$.future_data[[1]] <- future_data
  }

  nested_data <- modeltime::extract_nested_train_split(nested_data_tbl)

  target_var <- tolower(target_var)

  for (model in models_list) {
    # tune -- for
    tuning_param <- ""
    training_model <- NULL
    label_model <- ""

    if (model == "arima") {
      training_model <- th2_arima_engine(nested_data, target_var, "date", fit_model = "bulk")$fit
      label_model <- "model_arima"
    } else if (model == "prophet") {
      training_model <- th2_prophet_engine(nested_data, target_var, "date", use_holidays = use_holidays, fit_model = "bulk")$fit
      label_model <- "model_prophet"
    } else if (model == "lr") {
      training_model <- th2_linear_engine(nested_data, target_var, "date", fit_model = "bulk")$fit
      label_model <- "model_lm"
    } else if (model == "mars") {
      training_model <- th2_mars_engine(nested_data, target_var, "date", fit_model = "bulk")$fit
      label_model <- "model_mars"
    } else if (model == "random_forest") {
      training_model <- th2_random_forest_engine(nested_data, target_var, use_holidays = use_holidays, fit_model = "bulk", all_data = nested_data_tbl, lags = lags)$fit
      label_model <- "model_random_forest"
    } else if (model == "xgboost") {
      training_model <- th2_xgboost_engine(nested_data, "date", target_var, use_holidays = use_holidays, fit_model = "bulk", all_data = nested_data_tbl, lags = lags)$fit
      print(training_model)
      label_model <- "model_xgboost"
    } else {
      error_models <- c(NULL, model)
    }

    list_output_models[[label_model]] <- training_model
  }

  nested_modeltime_tbl <- do.call(modeltime::modeltime_nested_fit, c(list(nested_data = nested_data_tbl), list_output_models))

  best_nested_modeltime_tbl <- nested_modeltime_tbl %>%
    modeltime::modeltime_nested_select_best(
      metric                = "rmse",
      minimize              = TRUE,
      filter_test_forecasts = TRUE
    )


  nested_modeltime_refit_tbl <- nested_modeltime_tbl %>%
    modeltime::modeltime_nested_refit(
      control = modeltime::control_nested_refit(verbose = TRUE)
    )

  forecast_result <- nested_modeltime_refit_tbl %>%
    modeltime::extract_nested_future_forecast(
      .include_actual = FALSE
    ) %>%
    dplyr::mutate(as_of = Sys.Date()) %>%
    dplyr::mutate(start_date = min(input_data[[date_var]])) %>%
    dplyr::mutate(end_date = max(input_data[[date_var]]))


  return(forecast_result)
}
