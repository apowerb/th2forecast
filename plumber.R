library(plumber2)
library(th2forecast)

#* TH2 Forecasting API
#* API for th2Forecast package

#* Run a bulk forecast
#* @post /forecast
#* @param input_data:string Base64 encoded input data
#* @param group_target:string Column name for grouping
#* @param target_var:string Target variable name
#* @param date_var:string Date column name
#* @param future_forecast:int Number of periods to forecast
#* @param models_list:list List of models
#* @serializer json
function(res, input_data, group_target, target_var, date_var, future_forecast, models_list) {
  th2forecast::forecast(res, input_data, group_target, target_var, date_var, future_forecast, models_list)
}

#* @get /health
#* @serializer json
function() {
  list(status = "UP")
}

#* @get /
#* @serializer json
function() {
  list(status = "UP")
}
