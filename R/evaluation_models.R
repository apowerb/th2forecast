#' Calibre et évalue une liste de modèles
#'
#' Une fonction qui prend une liste de modèles entraînés à calibrer avec les
#' données de test et qui calcule ensuite les différentes mesures d'évaluation.
#'
#' @param input_data ensemble de données de test
#' @param model_table liste des modèles entraînés
#'
#' @return une liste avec 2 éléments : modèles calibrés - métriques d'évaluation
#' @export
#'
#' @examples model_evaluation(input_data, model_tbl)
model_evaluation <- function(input_data, model_table) {
  calib_tbl <- model_table %>%
    modeltime::modeltime_calibrate(testing(input_data), quiet = FALSE)

  accuracy_model <- calib_tbl %>%
    modeltime::modeltime_accuracy(metric_set = yardstick::metric_set(yardstick::mae, yardstick::rmse, yardstick::rsq))

  list("model_calibrated" = calib_tbl, "accuracy_models" = accuracy_model)
}


#' @export
th2_benchmarking <- function(test_data, forecasting_data, group_target = NULL, group_value = NULL, target_var = NULL, as_of = NULL ){

  test_data <- test_data[order(test_data[["_date"]]), ]

  forecasting_data <- forecasting_data[order(forecasting_data[["_date"]]), ]

  list_models <- unique(forecasting_data$`_model_desc`)

  if (!is.null(group_value)) {
    forecasting_data <- forecasting_data %>%
      dplyr::filter(forecasting_data[[group_target]] == !!group_value)
  }

  df_accuracy_test <- tibble(
    id = numeric(),
    .model_desc = character(),
    .type = character(),
    mae = numeric(),
    rmse = numeric()
  )

  y = as.double(unlist(test_data[[target_var]]))

  for(model in list_models){
    y_hat = as.double( forecasting_data %>%
                         dplyr::filter(forecasting_data$`_model_desc` == model & forecasting_data$execution_date == as_of) %>%
                         dplyr::pull(!!target_var))

    add_model <- tibble(
      id = group_value,
      .model_desc = model,
      .type = "Test",
      mae = yardstick::mae_vec( y, y_hat),
      rsq = yardstick::rmse_vec( y, y_hat)
    )

    df_accuracy_test <- rbind(df_accuracy_test, add_model)
  }

  return(df_accuracy_test)
}


#' @export
th2_rolling_forecast_stablizer<- function(data, model,months_test = 3, previsions = 3){

  max_date <- max(data[["X_date"]])

  size_data <- (31 * months_test) + (previsions * 7)

  date_filter <- max_date - size_data

  data <- data %>%
    filter(X_date >= date_filter)

  year_data <- unique(lubridate::year(data$X_date))

  months_list <- unique(lubridate::month(data$X_date))

  start_date <- as.Date(paste0(year_data,"-",months_list[2],"-01"), format = "%Y-%m-%d")
  end_date <- as.Date(paste0(year_data,"-",months_list[2+months_test],"-01"), format = "%Y-%m-%d")

  data <- data %>%
    dplyr::filter(X_date >= start_date & X_date < end_date )

  num_days = 0
  list_forecast <- list()
  last_date <- ""

  accuracy_df <- data.frame()

  for (i in c(1:previsions)) {

    # num_days = num_days + 7
    # cat("\nForecast for dates between", as.character(data$X_date[1] %m+% months(1)), " and ", as.character(data$X_date[1] %m+% months(2) + num_days), "\n")

    data_prevision <- data %>% dplyr::filter(X_date >= data$X_date[1] %m+% months(1) &  X_date < data$X_date[1] %m+% months(2) + num_days )

    output_forecast <- th2_bulk_forecasting( data_prevision, "family", "sales", "X_date", 30, c(model))
    # print(output_forecast)
    accuracy_df <- rbind(
      accuracy_df,
      as.data.frame(output_forecast$accuracy[1]) %>%
        dplyr::mutate(period = paste0(as.character(data$X_date[1] %m+% months(1)), " - ", as.character(data$X_date[1] %m+% months(2) + num_days)) )
      )

    list_forecast[i] <- output_forecast %>%
      dplyr::select(`.index`, `.value`) %>%
      list()

    last_date <- data$X_date[1] %m+% months(2) + num_days
    num_days = num_days + 7

  }

  accuracy_df <- accuracy_df %>%
    dplyr::mutate(mae = round(mae, 2)) %>%
    dplyr::mutate(rmse = round(rmse, 2)) %>%
    dplyr::mutate(rsq = round(rsq, 2)) %>%
    dplyr::select(- c(`.model_id`, `.type`))

  data <- data %>%
    dplyr::group_by_at(vars("X_date")) %>%
    dplyr::summarise_at(vars("sales"), sum) %>%
    dplyr::filter(X_date <= as.Date(last_date))

  time_series_plot <- plotly::plot_ly() %>%
    plotly::add_trace(
      data = data, type = "scatter", mode = "lines",
      x = ~ get("X_date"), y = ~ get("sales"), name = "Historical Values", color = I("#2C3E50")
    ) %>%
    plotly::add_trace(
      data = list_forecast[[1]], type = "scatter", mode = "lines",
      x = ~ get(".index"), y = ~ get(".value"), name = paste(model, "1"), color = I("#B0226B")
    ) %>%
    plotly::add_trace(
      data = list_forecast[[2]], type = "scatter", mode = "lines",
      x = ~ get(".index"), y = ~ get(".value"), name = paste(model, "2"), color = I("#6DB539")
    ) %>%
    plotly::add_trace(
      data = list_forecast[[3]], type = "scatter", mode = "lines",
      x = ~ get(".index"), y = ~ get(".value"), name = paste(model, "3"),  color = I("#7AC6EA")
    ) %>%
    plotly::layout(
      autosize = FALSE,
      width = 950,
      title = "Time Series Test",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Value"),
      showlegend = TRUE
    )


  return(list(time_series_plot = time_series_plot, accuracy_df = accuracy_df))

}
