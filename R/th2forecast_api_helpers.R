#' th2forecast_forecast_api
#' @export
th2forecast_forecast_api <- function(input_data,group_target = NULL, fcast_horizon = 30, target_var,date_var ){

  token <- jose::jwt_claim(user= "test@thaink2.com", scope= "readonly")
  #
  encrypted_token <- th2marketplace::th2_api_token_generator(jwt_token = token, target_action= "encrypt")
  #
  # decrypted_token <- th2_api_token_generator(jwt_token = encrypted_token, target_action= "decrypt")

  base_url <- "http://127.0.0.1:3838/"
  base_url <- "https://apis-dev.thaink2.fr/"
  end_point <- "thaink2/forecasting"

  req_body <- list(
    "actuals"= input_data,
    "fcast_horizon" = fcast_horizon,
    "group_target"= group_target,
    "target_var" = target_var,
    "date_var" = date_var,
    "fcast_model" = "xgboost"
  )

  req_url <- glue::glue("{base_url}{end_point}")
  forecast_req <- httr2::request(base_url = req_url)%>%
    httr2::req_auth_bearer_token(token = encrypted_token)%>%
    httr2::req_body_json(req_body)%>%
    httr2::req_method("POST")

  forecast_resp <- forecast_req%>%
    httr2::req_perform(verbosity = 3)

  forecast_result <- forecast_resp%>%
    httr2::resp_body_json()
  return(forecast_result)
}
#
# fcast_res <- th2forecast_forecast_api(
#   input_data = main_dataset2,
#   fcast_horizon = 30,
#   target_var = "y",
#   date_var = "ds",
#   group_target = "unique_id"
# )
