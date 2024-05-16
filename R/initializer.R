#' init_product
#' @description
#' Initiate the app with dependences
#' @noRd
init_forecast <- function() {
  Sys.setenv("CURRENT_DB" = "postgresql")
  Sys.setenv("SHINYPROXY_USERNAME" = "fayrouz.safa@thaink2.com")
  Sys.setenv("TH_DB_PASSWORD" = "thaink2MANAGER2024")
  Sys.setenv("CURRENT_DB" = "postgresql")
  print("initialisation.")

}
