mod_basic_fcast_viewer_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("fcast_box"))
}

mod_basic_fcast_viewer_server <- function(id, fcast_inputs2 = list()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    fcast_inputs <- fcast_inputs2
    output$fcast_output_params <- renderUI({
      fluidRow(
        column(width = 3, selectInput(inputId = ns("agg_by"), label = "By", choices = c("days","weeks"))),
        column(width = 3, uiOutput(ns("fcast_model")))
      )
    })

    output$fcast_model <- renderUI({
      list_of_models <- unique(fcast_inputs$prediction_data_aggregated$`_model_desc`)
      selectInput(inputId = ns("fcast_model"), label = "Model", choices = list_of_models)
    })
    fcast_plot <- reactive({
      req(input$agg_by)
      req(input$fcast_model)
      print(paste("target var is", fcast_inputs$target_var))


      prediction_data_aggregated <- fcast_inputs$prediction_data_aggregated%>%
        dplyr::filter(all_columns == !!fcast_inputs$target_var)%>%
        dplyr::filter(`_model_desc` == !!input$fcast_model)
      historical_data_aggregated <- fcast_inputs$historical_data_aggregated%>%
        dplyr::select(!!fcast_inputs$target_var, !!fcast_inputs$date_var)%>%
        tail(200)

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

    output$fcast_plot <- echarts4r::renderEcharts4r({
      fcast_plot()
    })

    output$fcast_table <- DT::renderDataTable({
      fcast_inputs$prediction_data_aggregated%>%
        dplyr::filter(all_columns == !!fcast_inputs$target_var)
    })

    output$fcast_box <- renderUI({
      bs4Dash::tabBox(title = fcast_inputs$target_var, width = 12, status = "primary", solidHeader = TRUE,
                      tabPanel(icon = icon("chart-line"), title = "",
                        fluidPage(
                          uiOutput(ns("fcast_output_params")),
                          echarts4r::echarts4rOutput(ns("fcast_plot"))
                        )
                      ),
                      tabPanel(icon = icon("table"),title = "",
                                 DT::dataTableOutput(ns("fcast_table"))
                               )
                      )

    })
  })
}
