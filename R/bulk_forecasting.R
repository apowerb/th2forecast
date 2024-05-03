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
#'
#' @examples
th2_bulk_forecasting <- function(input_data, group_target, target_var, date_var, future_forecast, models_list ){

  list_output_models <- list()

  if (group_target == "c"){
    data_tbl <- input_data %>%
      tidyr::pivot_longer(!date_var , names_to = "id")
    group_target <- "id"
    target_var <- "value"

  }else{

    select_vars <- c(group_target, date_var, target_var)

    data_tbl <- input_data %>%
      dplyr::select(all_of(select_vars))
    # data_tbl <- data_tbl %>%
    #   mutate(id_group = paste(store_nbr, family, sep = "_"))
  }

  count <- table(data_tbl[[group_target]])
  num_ids <- length(names(count[count > 1]))

  train_size <- round((nrow(data_tbl)/ num_ids) * 0.8)
  test_size <- (nrow(data_tbl)/ num_ids) - train_size

  nested_data_tbl <- data_tbl %>%
    modeltime::extend_timeseries(
      .id_var        = !!group_target,
      .date_var      = !!date_var,
      .length_future = future_forecast
    ) %>%
    modeltime::nest_timeseries(
      .id_var        = !!group_target,
      .length_future = future_forecast
    ) %>%
    modeltime::split_nested_timeseries(
      .length_test = test_size
    )

  nested_data <- modeltime::extract_nested_train_split(nested_data_tbl)

  for (model in models_list) {
    # tune -- for
    tuning_param <- ""
    training_model <- NULL
    label_model <- ""

    if(model == "arima"){
      training_model <- th2_arima_engine(nested_data, target_var, date_var, fit_model = "bulk")$fit
      label_model <- "model_arima"

    } else if(model == "prophet"){
      training_model <- th2_prophet_engine(nested_data, target_var, date_var, fit_model = "bulk")$fit
      label_model <- "model_prophet"

    } else  if(model == "lr"){
      training_model <- th2_linear_engine(nested_data, target_var, date_var, fit_model = "bulk")$fit
      label_model <- "model_lm"

    } else if(model == "mars"){
      training_model <- th2_mars_engine(nested_data, target_var, date_var, fit_model = "bulk")$fit
      label_model <- "model_mars"

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


  nested_modeltime_refit_tbl <- best_nested_modeltime_tbl %>%
    modeltime::modeltime_nested_refit(
      control = control_nested_refit(verbose = TRUE)
    )

  forecast_result <- nested_modeltime_refit_tbl %>%
    modeltime::extract_nested_future_forecast(
    .include_actual = FALSE) %>%
    mutate(as_of = Sys.Date())

  return(forecast_result)
}

