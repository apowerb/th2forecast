#' @export
generate_report_forecast <- function(dataset_input, var_date, var_target, kpi, models, column_kpi, split_date = NULL, horizon = NULL, spark_conection = NULL) {
  path_rmarkdown <- system.file("rmarkdown", package = "th2forecast")

  rmd_report_file <- paste0(path_rmarkdown, "/models_stability_test.Rmd")

  # data_filter <- dataset_input %>%
  #   dplyr::filter(dataset_input[[column_kpi]] %in% kpi)

  # result_test <- th2forecast::th2_rolling_forecast_stablizer(data_filter, var_date, var_target, column_kpi, models, split_date, horizon, spark_conection = spark_conection)
  #
  # for (kpi_plot in kpi) {
  #   result_test_kpi <- list()
  #
  #   result_test_kpi$time_series_plot <- result_test$time_series_plots[[kpi_plot]]
  #   result_test_kpi$accuracy_df <- result_test$accuracy_df %>%
  #     dplyr::filter(kpi_data == kpi_plot)


  report_params <- list(data = dataset_input, var_date = var_date, var_target = var_target, kpi = kpi, models = c(models, "TH2ENSEMBLE"), column_kpi = column_kpi, split_date = split_date, horizon = horizon)
  rmarkdown::render(input = rmd_report_file, output_file = paste0("models_stability_test_", kpi_plot), params = report_params, output_dir = "./reports")
  # }
}


#' @export
generate_rmd_skeleton <- function(dataset_input, var_date, var_target, target_kpis, models, column_kpi, split_date = NULL, horizon = NULL, spark_conection = NULL, draft_file = "/stability_template.Rmd", export_file = "/multiplekpis_report.Rmd" ){

  path_rmarkdown <- system.file("rmarkdown", package = "th2forecast")

  draft_file <- paste0(path_rmarkdown, draft_file)
  rmd_draft <- heddlr::import_draft(draft_file)
  rmd_header <- heddlr::import_draft( paste0(path_rmarkdown, "/report_header.Rmd"))
  rmd_heatmap <- heddlr::import_draft( paste0(path_rmarkdown, "/heatmap_performance.Rmd"))

  report_rmd_chunks <- target_kpis %>% purrr::map(~ heddlr::heddle(data = .x,pattern = rmd_draft, "TARGET_KPI"))
  report_skeleton <- c(rmd_header, report_rmd_chunks, rmd_heatmap)

  export_file <- paste0(path_rmarkdown, export_file)

  report_skeleton %>%
    heddlr::export_template(filename = export_file)

  report_params <- list(data = dataset_input, var_date = var_date, var_target = var_target, kpi = target_kpis, models = models, column_kpi = column_kpi, split_date = split_date, horizon = horizon)
  rmarkdown::render(input = export_file, output_file = paste0("models_stability_test_multiple_kpis"), params = report_params, output_dir = "./reports")

}


#' @export
generate_rmd_performance_spark <- function(dataset_input, column_kpi, var_target, var_date, horizon, models, train_split = NULL, spark_conection = NULL, test_models = FALSE){

  path_rmarkdown <- system.file("rmarkdown", package = "th2forecast")

  draft_file <- paste0(path_rmarkdown, "/report_performance_spark.Rmd")

  data_performance <- th2forecast::test_forecast_spark_perfomance(dataset_input, column_kpi, var_target, var_date, horizon, models, train_split = train_split, test_models = test_models)

  report_params <- list(data = data_performance, models = models)

  rmarkdown::render(input = draft_file, output_file = "models_performance_spark", params = report_params, output_dir = "./default_repo/reports")

}
