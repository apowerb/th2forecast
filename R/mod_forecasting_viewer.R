


#' mod_forecasting_viewer_ui
#' @export


mod_forecasting_viewer_ui <- function(id) {
  ns <- NS(id)

  bs4Dash::tabBox(
    id = ns("forecastViz_tabbox"), width = 12, selected = "Forecasting Pipelines",
    tabPanel(
      title = "Forecasting Pipelines", icon = icon("database"),
  fluidPage(
      DT::dataTableOutput(ns("pipelines_table"))
    )
  ),
  tabPanel(
    title = "Forecasting Viewer", icon = icon("chart-line"),
    fluidPage(
      fluidRow(
      column(width = 2, uiOutput(ns("as_of"))),
      column(width = 2, uiOutput(ns("kpi_value"))),
      column(width = 2, uiOutput(ns("model"))),
      column(width = 2, uiOutput(ns("target_variable"))),
      column(width = 2, uiOutput(ns("aggregation"))),
      column(width = 2, uiOutput(ns("run")))
      ),
    uiOutput(ns("graph_output"))
    ))
  )
}



#'mod_forecasting_viewer_server
#' @export
mod_forecasting_viewer_server <- function(id) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_info <- reactiveVal()
    pipelines_metadata <- reactiveVal()
    input_data_result <- reactiveVal()
    output_data_result <- reactiveVal()



##=============connect to Db and return pipelines metadata ======
  pipelines_metadata <- reactiveVal({
      user = Sys.getenv("SHINYPROXY_USERNAME")
      sql <- glue::glue(
        "select tb1.*,
          tb_in.param1 as input_meta_connection,tb_in.data_source as input_datasource_type,
          tb_out.param1 as output_meta_connection,tb_out.data_source as output_datasource_type
          from th2_forecast_project tb1
          join th2_wf_permissions  tb2 on tb1.pipeline_uuid = tb2.object_id
          join data_connection_params tb_in on tb1.input_id = tb_in.table_id
          join data_connection_params tb_out on tb1.output_id = tb_out.table_id
          where tb2.permitted_users = '{user}'"
      )

      pipelines_metadata <- th2product::fetch_data_from_db_by_sql(sql)
      pipelines_metadata
    })


    output$pipelines_table <-  DT::renderDataTable({
      user_permissions <- th2blender::get_user_data_permissions(target_table = "th2_wf_permissions", object_type = "fc")
      pipelines_metadata <- th2product::fetch_data_from_db(table = "th2_forecast_project")

      if (nrow(pipelines_metadata) == 0) {
        return(NULL)
      }
      pipelines_metadata <- user_permissions %>%
        dplyr::rename(pipeline_uuid = OBJECT_ID) %>%
        dplyr::select(pipeline_uuid) %>%
        dplyr::inner_join(pipelines_metadata, by = c("pipeline_uuid"))
      if (nrow(pipelines_metadata) == 0) {
        return(NULL)
      }
      pipelines_metadata
      DT::datatable(pipelines_metadata(), selection = list(mode = "single"))
    })


  observeEvent(input$pipelines_table_rows_selected, {
    selected_row <- NULL
    selected_row <- input$pipelines_table_rows_selected

    if (length(selected_row) > 0) {
      selected_info(pipelines_metadata()[selected_row, ])

      output_connection <- th2product::decrypt_column(selected_info()['output_meta_connection'])

      decrypted_output_connection <- jsonlite::fromJSON(output_connection)

      db_conn <- db_conn_function(dbms = "postgresql",
                                  user = decrypted_output_connection$username,
                                  password = decrypted_output_connection$password,
                                  port = decrypted_output_connection$port ,
                                  host = decrypted_output_connection$host,
                                  db_name = decrypted_output_connection$database)

      output_data_result(output_data_fetch(db_conn = db_conn,
                                           target_table = decrypted_output_connection$target_table,
                                           schema = decrypted_output_connection$schema,
                                           target_var = selected_info()$target_var,
                                           group_target_var = selected_info()$group_target_var,
                                           date_var = selected_info()$date_var))

    updateTabsetPanel(session, "forecastViz_tabbox", selected = "Forecasting Viewer")
    }
  })

#=====Forecating Viewer ==================

  output$as_of <- renderUI({
  req(output_data_result())
  execution_dates <- base::unique(output_data_result()$as_of)
  selectInput(inputId = ns("as_of"), label = "As_Of", choices = c("",format(execution_dates, "%Y-%m-%d")))
  })

  observeEvent(input$as_of,{

      input_connection <- th2product::decrypt_column(selected_info()['input_meta_connection'])
      print(input_connection)

      decrypted_input_connection <- jsonlite::fromJSON(input_connection)
      print(decrypted_input_connection)

      db_conn <- db_conn_function(dbms = "postgresql",
                                  user = decrypted_input_connection$username,
                                  password = decrypted_input_connection$password,
                                  port = decrypted_input_connection$port ,
                                  host = decrypted_input_connection$host,
                                  db_name = decrypted_input_connection$database)

    input_data_result(input_data_fetch(prediction_data = output_data_result(),
                                       db_conn = db_conn,
                                       target_table = decrypted_input_connection$target_table,
                                       target_var = selected_info()$target_var,
                                       group_target_var = selected_info()$group_target_var,
                                       date_var = selected_info()$date_var,
                                       as_of = input$as_of))

  })

    output$kpi_value <- renderUI({
      req(input$as_of)
      req(selected_info())
      req(input_data_result())

      group_target_var <- selected_info()$group_target_var
      kpi_values <- base::unique(input_data_result()[group_target_var])
      selectInput(inputId = ns("kpi_value"), label = "KPIs", choices = kpi_values, multiple = FALSE)
    })

    output$model <- renderUI({
      req(input$as_of)
      req(output_data_result())
      model_names <- base::unique(output_data_result()$`_model_desc`)
      selectInput(inputId = ns("model"), label = "Model", choices = model_names, multiple = FALSE)
  })

    output$target_variable <- renderUI({
      req(input$as_of)
      # req(output_data_result())
      target_variables <- base::unique()
      selectInput(inputId = ns("target_variable"), label = "Target Variable", choices = target_variables, multiple = FALSE)
    })

    output$aggregation <- renderUI({
      req(input$as_of)
      selectInput(inputId = ns("agg_type"), "Aggregation", choices = c("", "Sum" = "sum", "Mean"="mean", "Count"="count"))

    })



    output$run <- renderUI({
     req(input_data_result())
     req(output_data_result())
     req(input$kpi_value, input$model)
     actionButton(inputId = ns("run"), label = "Run",style = "color: #ffffff; background-color: #007bff; border-color: #007bff;")
    })


    observeEvent(input$run,{

      prediction_data_filtred_result <- prediction_data_filtred(prediction_data = output_data_result(),
                                                                group_target_var = selected_info()$group_target_var,
                                                                 model = input$model,
                                                                 kpi_value = input$kpi_value)

      historical_data_filtred_result <- historical_data_filtred(historical_data = input_data_result(),
                                                                group_target_var = selected_info()$group_target_var,
                                                                kpi_value = input$kpi_value)

      if (!is.null(historical_data_filtred_result) && !is.null(prediction_data_filtred_result)) {

        output$graph_output <- renderUI({
          create_time_series_plot(historical_data = historical_data_filtred_result ,prediction_data =prediction_data_filtred_result )
        })
      } else {
       output$graph_output <- renderText("Aucune donnée disponible pour les filtres sélectionnés.")
      }
    })

  })
  }






