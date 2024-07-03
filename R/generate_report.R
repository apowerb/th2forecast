#' @export
generate_report_forecast <- function(dataset_input, kpi, models, column_kpi){
  rmd_report_file <- "./inst/test_models.Rmd"
  report_params <- list(data = dataset_input, kpi = kpi, models = models, column_kpi = column_kpi)

  rmarkdown::render(input = rmd_report_file, params = report_params, output_dir = "./reports" )
}
