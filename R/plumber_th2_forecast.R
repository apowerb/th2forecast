#' @title Plumber API endpoint for th2_bulk_forecasting
#' @description This file contains the Plumber API endpoint for the th2_bulk_forecasting function.
#' @name forecast_endpoint
NULL

#' @apiTitle TH2 Forecasting API
#' @apiDescription API for th2Forecast package

#' Run a bulk forecast
#'
#' @post /forecast
#' @param input_data:string Base64-encoded and serialized R data frame.
#' @param group_target:string Column for grouping time series.
#' @param target_var:string Target variable for forecasting.
#' @param date_var:string Name of the date column.
#' @param future_forecast:int Number of future periods to forecast.
#' @param models_list:list A list of models to use for forecasting.
#' @param train_split:string Optional date string for splitting training data.
#' @param external_data:string Optional Base64-encoded and serialized external data frame.
#' @param exogenous_var:list Optional list of exogenous variables.
#' @param use_holidays:string Optional holiday specification.
#' @param country_column:string Optional column with country names for holidays.
#' @param lags:boolean Optional flag to use lags.
#' @param path_driver:string Optional path to a database driver.
#' @param use_meteo:string Optional meteo specification.
#' @serializer json
#' @export
forecast <- function(res, input_data, group_target, target_var, date_var, future_forecast, models_list, train_split = NULL, external_data = NULL, exogenous_var = NULL, use_holidays = NULL, country_column = NULL, lags = FALSE, path_driver = NULL, use_meteo = NULL) {
    # Decode the base64 input_data
  tryCatch({
    input_data_df <- unserialize(base64enc::base64decode(input_data))
  }, error = function(e) {
    res$status <- 400
    return(list(error = "Invalid base64 for input_data. Ensure it's a serialized and base64-encoded data frame."))
  })
  
  # Decode external_data if provided
  external_data_df <- NULL
  if (!is.null(external_data)) {
      tryCatch({
        external_data_df <- unserialize(base64enc::base64decode(external_data))
      }, error = function(e) {
        res$status <- 400
        return(list(error = "Invalid base64 for external_data. Ensure it's a serialized and base64-encoded data frame."))
      })
  }

  # Call the forecasting function
  forecast_result <- th2_bulk_forecasting(
    input_data = input_data_df,
    group_target = group_target,
    target_var = target_var,
    date_var = date_var,
    future_forecast = future_forecast,
    models_list = models_list,
    train_split = train_split,
    external_data = external_data_df,
    exogenous_var = exogenous_var,
    use_holidays = use_holidays,
    country_column = country_column,
    lags = lags,
    path_driver = path_driver,
    use_meteo = use_meteo
  )

  # Return the forecast result
  return(forecast_result)
}
