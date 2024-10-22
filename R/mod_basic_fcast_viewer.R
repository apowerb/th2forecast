mod_basic_fcast_viewer_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("fcast_output_params")),
    uiOutput(ns("fcast_plot"))
  )
}

mod_basic_fcast_viewer_server <- function(id, fcast_inputs = list()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$fcast_output_params <- renderUI({
      fluidRow(
        column(width = 2, selectInput(inputId = ns("agg_by"), label = "By", choices = c("days","weeks"))),
        column(width = 2, uiOutput(ns("fcast_model")))
      )
    })

    output$fcast_model <- renderUI({
      fcast_inputs2 <<- fcast_inputs
      list_of_models <- unique(fcast_inputs$prediction_data_aggregated$`_model_desc`)
      selectInput(inputId = ns("fcast_model"), label = "Model", choices = list_of_models)
    })
    output$fcast_plot <- renderUI({
      req(input$agg_by)
      req(input$fcast_model)
      historical_data_aggregated <- tail(fcast_inputs$historical_data_aggregated, 120)
      prediction_data_aggregated <<- fcast_inputs$prediction_data_aggregated%>%
        dplyr::filter(`_model_desc` == "XGBOOST")
      if(input$agg_by == "days") {
        create_time_series_plot(historical_data = historical_data_aggregated,
                                prediction_data = prediction_data_aggregated,
                                x_var = fcast_inputs$date_var,
                                y_var = fcast_inputs$target_var)
      } else {
        create_weekly_bar_chart(historical_data = historical_data_aggregated,
                                prediction_data = prediction_data_aggregated,
                                x_var = fcast_inputs$date_var,
                                y_var = fcast_inputs$target_var,
                                agg_type = input$agg_type)
      }
    })
  })
}
