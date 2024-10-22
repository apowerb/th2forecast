mod_basic_fcast_viewer_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("fcast_output_params"))
  # uiOutput(ns("fcast_plot"))
}

mod_basic_fcast_viewer_server <- function(id, fcast_inputs = list()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$fcast_output_params <- renderUI({
      fluidRow(
        column(width = 2, selectInput(inputId = ns("agg_by"), label = "By", choices = c("days","weeks")))
      )
    })

    output$fcast_plot <- renderUI({
      if(input$agg_by == "days") {
        create_time_series_plot(historical_data = fcast_inputs$historical_data_aggregated,
                                prediction_data = fcast_inputs$prediction_data_aggregated,
                                x_var = selected_info()$date_var,
                                y_var = selected_info()$target_var)
      } else {
        create_weekly_bar_chart(historical_data = historical_data_aggregated, prediction_data = prediction_data_aggregated, x_var = selected_info()$date_var, y_var = selected_info()$target_var, agg_type = input$agg_type)
      }
    })
  })
}
