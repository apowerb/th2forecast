#' @export
generate_report_forecast <- function(dataset_input, var_date, var_target, kpi, models, column_kpi, split_date = NULL, horizon = NULL, spark_conection = NULL) {
  path_rmarkdown <- system.file("rmarkdown", package = "th2forecast")

  rmd_report_file <- paste0(path_rmarkdown, "/models_stability_test.Rmd")

  data_filter <- dataset_input %>%
    dplyr::filter(dataset_input[[column_kpi]] %in% kpi)

  result_test <- th2forecast::th2_rolling_forecast_stablizer(data_filter, var_date, var_target, column_kpi, models, split_date, horizon, spark_conection = spark_conection)

  for (kpi_plot in kpi) {
    result_test_kpi <- list()

    result_test_kpi$time_series_plot <- result_test$time_series_plots[[kpi_plot]]
    result_test_kpi$accuracy_df <- result_test$accuracy_df %>%
      dplyr::filter(kpi_data == kpi_plot)


    report_params <- list(data = result_test_kpi, var_date = var_date, var_target = var_target, kpi = kpi, models = c(models, "TH2ENSEMBLE"), column_kpi = column_kpi, split_date = split_date, horizon = horizon)
    rmarkdown::render(input = rmd_report_file, output_file = paste0("models_stability_test_", kpi_plot), params = report_params, output_dir = "./reports")
  }
}
