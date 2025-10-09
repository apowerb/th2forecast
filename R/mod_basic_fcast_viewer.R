mod_basic_fcast_viewer_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("fcast_box"))
}

mod_basic_fcast_viewer_server <- function(id, fcast_inputs = list()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$fcast_output_params <- renderUI({
      fluidRow(
        column(width = 3, uiOutput(ns("agg_by"))),
        column(width = 3, uiOutput(ns("fcast_model"))),
        column(width = 3, uiOutput(ns("fcast_horizon"))),
        column(width = 3, br(), mod_expand_graph_ui(ns("expand_graph")))
      )
    })


    mod_expand_graph_server("expand_graph", interactive_graph = fcast_plot)

    date_freqs <- reactive({
      date_freq <- fcast_inputs$historical_data_aggregated %>%
        dplyr::pull(!!fcast_inputs$date_var) %>%
        th2reporting::possible_units_for_summary(time_vect = .)
      return(date_freq)
    })
    output$agg_by <- renderUI({
      req(date_freqs())
      shinyWidgets::pickerInput(inputId = ns("agg_by"), label = "By", choices = date_freqs())
    })
    output$fcast_model <- renderUI({
      list_of_models <- unique(fcast_inputs$prediction_data_aggregated$`_model_desc`)
      shinyWidgets::pickerInput(inputId = ns("fcast_model"), label = "Model", choices = list_of_models)
    })

    output$fcast_horizon <- renderUI({
      req(input$agg_by)
      fcast_horizon <- 60
      if (input$agg_by == "days") {
        fcast_horizon <- 30
      } else if (input$agg_by == "weeks") {
        fcast_horizon <- 8
      } else if (input$agg_by == "months") {
        fcast_horizon <- 4
      } else if (input$agg_by == "years") {
        fcast_horizon <- 2
      }
      numericInput(
        inputId = ns("fcast_horizon"),
        label = "Horizon",
        value = fcast_horizon,
        min = 5,
        max = 100
      )
    })

    fcast_plot <- reactive({
      req(input$agg_by)
      req(date_freqs())
      req(input$fcast_model)
      req(input$fcast_horizon)
      prediction_data_aggregated <- fcast_inputs$prediction_data_aggregated %>%
        dplyr::filter(`_model_desc` == !!input$fcast_model)
      if (input$agg_by == date_freqs()[1]) {
        create_time_series_plot(
          historical_data = fcast_inputs$historical_data_aggregated,
          prediction_data = prediction_data_aggregated,
          x_var = fcast_inputs$date_var,
          y_var = fcast_inputs$target_var,
          fcast_horizon = input$fcast_horizon
        )
      } else {
        create_weekly_bar_chart(
          historical_data = fcast_inputs$historical_data_aggregated,
          prediction_data = prediction_data_aggregated,
          x_var = fcast_inputs$date_var,
          y_var = fcast_inputs$target_var,
          agg_freq = input$agg_by,
          fcast_horizon = input$fcast_horizon,
          agg_type = "sum"
        )
      }
    })

    output$fcast_plot <- echarts4r::renderEcharts4r({
      fcast_plot()
    })

    output$fcast_table <- DT::renderDataTable({
      fcast_inputs$prediction_data_aggregated %>%
        dplyr::select(-`_model_id`, -start_date, -end_date) %>%
        DT::datatable(
          data = .,
          extensions = c("Scroller"),
          options = list(
            dom = "Bfrtip",
            deferRender = TRUE,
            scrollY = 300,
            scroller = TRUE
          )
        )
    })


    output$fcast_box <- renderUI({
      exporter_mod_id <- th2product::generateID("exporter")
      th2reporting:::save_datatable_server(exporter_mod_id, export_name = fcast_inputs$target_var, data_table = reactive({
        fcast_inputs$prediction_data_aggregated %>%
          dplyr::select(-`_model_id`, -start_date, -end_date)
      }))

      bs4Dash::tabBox(
        title = fcast_inputs$target_var, width = 12, status = "primary", solidHeader = TRUE,
        tabPanel(
          icon = icon("chart-line"), title = "",
          fluidPage(
            uiOutput(ns("fcast_output_params")),
            echarts4r::echarts4rOutput(ns("fcast_plot"))
          )
        ),
        tabPanel(
          icon = icon("table"), title = "",
          th2reporting:::save_datatable_ui(id = ns(exporter_mod_id)),
          DT::dataTableOutput(ns("fcast_table"))
        )
      )
    })
  })
}
