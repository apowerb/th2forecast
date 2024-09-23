#' @export
generate_report_forecast <- function(dataset_input, var_date, var_target, kpi, models, column_kpi, split_date = NULL, horizon = NULL, use_holidays = NULL, country_column = NULL, lags = FALSE, path_driver = NULL, use_meteo = NULL, spark_conection = NULL) {
  path_rmarkdown <- system.file("rmarkdown", package = "th2forecast")

  rmd_report_file <- paste0(path_rmarkdown, "/models_stability_test.Rmd")

  # data_filter <- dataset_input %>%
  #   dplyr::filter(dataset_input[[column_kpi]] %in% kpi)
  #
  # result_test <- th2forecast::th2_rolling_forecast_stablizer(data_filter, var_date, var_target, column_kpi, models, split_date, horizon, use_holidays = use_holidays, country_column = country_column, lags = lags, path_driver = path_driver, use_meteo = use_meteo, spark_conection = spark_conection)

  for (kpi_plot in kpi) {
    result_test_kpi <- list()

    data_filter <- dataset_input %>%
      dplyr::filter(dataset_input[[column_kpi]] == kpi_plot)

    result_test <- th2forecast::th2_report_results(data_filter, var_date, var_target, column_kpi, models, split_date, horizon, use_holidays = use_holidays, country_column = country_column, lags = lags, path_driver = path_driver, use_meteo = use_meteo, spark_conection = spark_conection)

    result_test_kpi$time_series_plot <- result_test$time_series_plots[[kpi_plot]]
    result_test_kpi$accuracy_df <- result_test$accuracy_df %>%
      dplyr::filter(kpi_data == kpi_plot)


    report_params <- list(data = result_test_kpi, var_date = var_date, var_target = var_target, kpi = kpi, models = c(models, "timegpt", "TH2ENSEMBLE"), column_kpi = column_kpi, split_date = split_date, horizon = horizon)
    rmarkdown::render(input = rmd_report_file, output_file = paste0("models_stability_test_", ifelse(kpi_plot == "BREAD/BAKERY_1", "BREAD_BAKERY_1", kpi_plot)), params = report_params, output_dir = "./reports")
  }
}
