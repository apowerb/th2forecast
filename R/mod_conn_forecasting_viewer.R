


#' mod_forecasting_viewer_ui
#' @export


mod_conn_forecasting_viewer_ui <- function(id) {
  ns <- NS(id)

  bs4Dash::tabBox(
    id = ns("forecastViz_tabbox"), width = 12, selected = "Forecasting Pipelines",
    tabPanel(
      title = "Forecasting Pipelines", icon = icon("database"),
  fluidPage(
    fluidRow(
      column(width = 2, textInput(inputId = ns("host"), label = "Hostname")),
      column(width = 2, textInput(inputId = ns("username"), label = "Username")),
      column(width = 2, passwordInput(inputId = ns("password"), label = "Password")),
      column(width = 2, textInput(inputId = ns("port"), label = "Port", value = 5432)),
      column(width = 2, textInput(inputId = ns("db_name"), label = "Database Name")),
      uiOutput(ns("connect_btn"))
      ),
    fluidRow(
      DT::dataTableOutput(ns("pipelines_table"))
    )
  )
  ),
  tabPanel(
    title = "Forecasting Viewer", icon = icon("chart-line"),
    fluidPage(
      fluidRow(
      column(width = 2, uiOutput(ns("as_of"))),
      column(width = 2, uiOutput(ns("kpi_value"))),
      column(width = 2, uiOutput(ns("model"))),
      column(width = 2, uiOutput(ns("run")))
      ),
    uiOutput(ns("graph_output"))
    ))
  )
}



#'mod_forecasting_viewer_server
#' @export
mod_conn_forecasting_viewer_server <- function(id) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_row_info <- reactiveVal()

    output_data_result <- reactiveVal()
    input_data_result <- reactiveVal()
    merged_data_result <- reactiveVal()


  output$connect_btn <- renderUI({
    req(input$host, input$username, input$password, input$port, input$db_name)
    actionButton(inputId = ns("connect"), label = "Connect",style = "color: #ffffff; background-color: #007bff; border-color: #007bff;", icon = icon("nfc-symbol"))
  })

#==== db Connection and return valable tables

  available_data_tables <- eventReactive(input$connect, {
    connection_items <- th2blender::th2_connect_to_data_source(data_source_params = list(
      data_source= "postgresql",
      postgresql = list(
        host = input$host,
        database = input$db_name,
        password = input$password,
        username = input$username,
        port = input$port
      )
    ),
    target_action = "list_tables")
    showNotification(paste(input$db_name, "(postgresql)","Connection Successfull"), type = "message")
    return(connection_items)
  })

#===================================

  output$pipelines_table <-  DT::renderDataTable({
    req(available_data_tables())

    user_permissions <- th2blender::get_user_data_permissions()

    pipelines_metadata <- th2product::fetch_data_from_db(table = "th2_forecast_project")


  #   if (nrow(pipelines_metadata) == 0) {
  #     return(NULL)
  #   }
  #   pipelines_metadata <- user_permissions %>%
  #     dplyr::rename(project_name = OBJECT_ID) %>%
  #     dplyr::select(project_name) %>%
  #     dplyr::inner_join(pipelines_metadata, by = c("project_name"))
  #   if (nrow(pipelines_metadata) == 0) {
  #     return(NULL)
  #   }
  #   pipelines_metadata
  })


  observeEvent(input$pipelines_table_rows_selected, {
    selected_row <- input$pipelines_table_rows_selected
    #Récupérer les infos de la ligne sélectionnée
    if (length(selected_row) == 1) {
      pipelines_metadata <- th2product::fetch_data_from_db(table = "th2_forecast_project")
      selected_info <<- pipelines_metadata[selected_row, ]

      selected_row_info(selected_info)

      db_conn <- db_conn_function(dbms = "postgresql",
                                  user = input$username, password = input$password,
                                  port = input$port , host = input$host, db_name = input$db_name)

      available_tables <- DBI::dbListTables(conn = db_conn)

      print(selected_row_info()$output_id)

      output_data_result(output_data_function(selected_info = selected_row_info(),
                                              selected_table_output = selected_row_info()$output_id,
                                              db_conn = db_conn,
                                              available_tables = available_tables))


      print(output_data_result())


    updateTabsetPanel(session, "forecastViz_tabbox", selected = "Forecasting Viewer")
    }
  })





#=====Forecating Viewer ==================

  output$as_of <- renderUI({
  req(selected_row_info())

  execution_dates <- base::unique(output_data_result()$as_of)
  selectInput(inputId = ns("as_of"), label = "As_Of", choices = execution_dates, multiple = FALSE)
  })

  observeEvent(input$as_of,{
    db_conn <- db_conn_function(dbms = "postgresql",
                                user = input$username, password = input$password,
                                port = input$port , host = input$host, db_name = input$db_name)

    available_tables <- DBI::dbListTables(conn = db_conn)

    input_data_result(input_data_function(selected_table_input = input$input_table,
                                          prediction_data = output_data_result(),
                                          db_conn = db_conn,
                                          as_of = input$as_of,
                                          available_tables = available_tables))
  })

    output$kpi_value <- renderUI({
      req(input$as_of)
      req(input_data_result())
      kpi_values <- base::unique(input_data_result()$family)
      selectInput(inputId = ns("kpi_value"), label = "KPIs", choices = kpi_values, multiple = FALSE)
    })

    output$model <- renderUI({
      req(input$as_of)
      req(output_data_result())
      model_names <- base::unique(output_data_result()$`_model_desc`)
      selectInput(inputId = ns("model"), label = "Model", choices = model_names, multiple = FALSE)
  })
    output$run <- renderUI({
     req(input_data_result())
     req(output_data_result())
     req(input$kpi_value, input$model)
     actionButton(inputId = ns("run"), label = "Run",style = "color: #ffffff; background-color: #007bff; border-color: #007bff;")
    })


    observeEvent(input$run,{

      prediction_data_filtred_result <<- prediction_data_filtred(prediction_data = output_data_result(),
                                                                 model = input$model,
                                                                 kpi_value = input$kpi_value)

      historical_data_filtred_result <<- historical_data_filtred(historical_data = input_data_result(),
                                                                 kpi_value = input$kpi_value)

      if (!is.null(historical_data_filtred_result) && !is.null(prediction_data_filtred_result)) {

        output$graph_output <- renderUI({
          create_time_series_plot(historical_data = historical_data_filtred_result ,prediction_data =prediction_data_filtred_result )
        })
      } else {
       output$graph_output <- renderText("Aucune donnée disponible pour les filtres sélectionnés.")
      }
    })


    db_conn_function <- function(dbms = NULL ,server = NULL, user = NULL, password = NULL, port =NULL ,
                                 host = NULL, db_name = NULL)  {
       db_conn <- DatabaseConnector::connect(
        dbms = "postgresql",
        server = paste0(host, "/", db_name),
        user = user,
        password = password,
        port = as.numeric(port)
    )
       return(db_conn)
    }


  })
  }






