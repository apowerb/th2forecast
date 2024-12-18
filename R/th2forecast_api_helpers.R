#' th2forecast_forecast_api
#' @export
th2forecast_forecast_api <- function(input_data,group_target = NULL,base_url ,fcast_horizon = 30, target_var,date_var , models_list = NULL){

  token <- jose::jwt_claim(user= "test@thaink2.com", scope= "readonly")
  #
  encrypted_token <- th2marketplace::th2_api_token_generator(jwt_token = token, target_action= "encrypt")
  #
  # decrypted_token <- th2_api_token_generator(jwt_token = encrypted_token, target_action= "decrypt")
  end_point <- "thaink2/forecasting"

  req_body <- list(
    "actuals"= input_data,
    "fcast_horizon" = fcast_horizon,
    "group_target"= group_target,
    "target_var" = target_var,
    "date_var" = date_var,
    "models_list" = models_list
  )

  req_url <- glue::glue("{base_url}{end_point}")
  forecast_req <- httr2::request(base_url = req_url)%>%
    httr2::req_auth_bearer_token(token = encrypted_token)%>%
    httr2::req_body_json(req_body)%>%
    httr2::req_method("POST")

  forecast_resp <- forecast_req%>%
    httr2::req_perform(verbosity = 2)

  forecast_result <- forecast_resp%>%
    httr2::resp_body_json()
  return(forecast_result)
}
