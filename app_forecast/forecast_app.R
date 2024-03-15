#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(th2Forecast)


ui <- fluidPage(

  mod_th2_forecasting_ui("id_forecast")
)




server <- function(input, output) {

  mod_th2_forecasting_server("id_forecast")
}


shinyApp(ui = ui, server = server , options = list(launch.browser = TRUE))
