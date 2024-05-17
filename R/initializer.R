#' init_forecast
#' @description
#' Initiate the app with dependences
#' @noRd
init_forecast <- function() {
  Sys.setenv("CURRENT_DB" = "postgresql")
  Sys.setenv("SHINYPROXY_USERNAME" = "fayrouz.safa@thaink2.com")
  Sys.setenv("TH_DB_PASSWORD" = "thaink2MANAGER2024")
  Sys.setenv("CURRENT_DB" = "postgresql")
  Sys.setenv("ENCRYPT_PASS" = "eb7cea86ec70f9417124298f6db12173d04dc8b01f856997a526f648b844c017")
  print("initialisation.")

}
