#' forecast_transform_data
#'
#' @param fc_data data loaded
#' @param fc_meta_data metadata to use
#'
#' @export
forecast_transform_data <- function(
    fc_data = NULL,
    fc_meta_data = NULL) {
  fc_meta_data$target_var <- tolower(fc_meta_data$target_var)
  fc_meta_data$group_target <- tolower(fc_meta_data$group_target)
  fc_meta_data$date_var <- tolower(fc_meta_data$date_var)
  fc_meta_data$business_days <- tolower(fc_meta_data$business_days)
  fc_meta_data$use_spark <- tolower(fc_meta_data$use_spark)

  colnames(fc_data) <- tolower(colnames(fc_data))
  date_var_input <- fc_meta_data$date_var
  date_var <- date_var_input
  if (startsWith(date_var, "_")) {
    date_var <- paste0("X", date_var)
  }

  date_var <- tolower(date_var)
  date_value <- as.Date(fc_meta_data$split_train_test, origin = "1970-01-01")
  date_string <- format(date_value, "%Y-%m-%d")
  df <- as_tibble(fc_data)
  df[[date_var]] <- as.POSIXct(df[[date_var]])

  calendar_column <- fc_meta_data$calendar_column
  if ("" == calendar_column || is.null(calendar_column)) {
    calendar_column <- NULL
  }

  use_holidays <- fc_meta_data$calendar_country
  if ("" == use_holidays || is.null(use_holidays)) {
    use_holidays <- NULL
  }

  use_spark <- ifelse(length(fc_meta_data$use_spark) == 0, FALSE, fc_meta_data$use_spark)

  if (use_spark == "TRUE") {
    spark_connection <- "spark://spark-1723119839-master-0.spark-1723119839-headless.th2mage.svc.cluster.local:7077"
    result_bulk_f <- th2forecast::th2_forecast_spark(df, fc_meta_data$group_target, fc_meta_data$target_var, date_var, fc_meta_data$future_forecast, fc_meta_data$models_list, train_split = date_string, group_by_col = c("family"), master_spark = spark_connection)

    # names(result_bulk_f)[names(result_bulk_f) == '_value'] <- fc_meta_data$target_var
    names(result_bulk_f)[names(result_bulk_f) == "_index"] <- date_var_input
  } else {
    result_bulk_f <- th2forecast::th2_bulk_forecasting(df, fc_meta_data$group_target, fc_meta_data$target_var, date_var, fc_meta_data$future_forecast, fc_meta_data$models_list, train_split = date_string, use_holidays = use_holidays, country_column = calendar_column, path_driver = "/home/src/default_repo/utils/drivers")

    # names(result_bulk_f)[names(result_bulk_f) == '.value'] <- fc_meta_data$target_var
    names(result_bulk_f)[names(result_bulk_f) == ".index"] <- date_var_input
  }

  # Supprimer la colonne accuracy seulement si elle existe
  if ("accuracy" %in% colnames(result_bulk_f)) {
    result_bulk_f <- result_bulk_f %>%
      dplyr::select(-accuracy)
  }

  return(result_bulk_f)
}
