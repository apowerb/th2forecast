#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(th2forecast)
library(modeltime)
library(shinyalert)
library(plotly)
library(shinyalert)
library(anomalize)



# Declaration and initialization of global variables
data_input <- data.frame()
data_clean <- data.frame()
data_train <- data.frame()
data_feature_train <- data.frame()

var_date_feature <- ""
var_target <- ""
start_date <- NULL
end_date <- NULL

dataset_train_test <- data.frame()
list_features <- list()

models_trained <- modeltime_table()
models_predictions <- modeltime_table()
df_prediction_test_forecas <- NULL
first_horizon <- 0


# Shiny application
ui <- fluidPage(

  mod_th2_forecasting_ui("id_forecast")
)


server <- function(input, output) {
  mod_th2_forecasting_server("id_forecast")
}


shinyApp(ui = ui, server = server , options = list(launch.browser = TRUE))
