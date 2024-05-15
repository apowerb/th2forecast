


#' mod_forecasting_viewer_ui
#' @export


mod_forecasting_viewer_ui <- function(id) {
  ns <- NS(id)

  bs4Dash::tabBox(
    id = ns("forecastViz_tabbox"), width = 12, selected = "DB Connection",
    tabPanel(
      title = "DB Connection", icon = icon("database"),
  fluidPage(
    fluidRow(
      column(width = 2, textInput(inputId = ns("host"), label = "Hostname", value = "thaink2-db.cbqdqfe0vbqr.eu-west-3.rds.amazonaws.com")),
      column(width = 2, textInput(inputId = ns("username"), label = "Username", value = "farid")),
      column(width = 2, passwordInput(inputId = ns("password"), label = "Password", value = "thaink2MANAGER2024")),
      column(width = 2, textInput(inputId = ns("port"), label = "Port", value = 5432)),
      column(width = 2, textInput(inputId = ns("db_name"), label = "Database Name", value = "postgres")),
      uiOutput(ns("connect_btn"))
      ),
    fluidRow(
      column(width = 2, uiOutput(ns("input_table"))),
      column(width = 2, uiOutput(ns("output_table"))),
      column(width = 2, uiOutput(ns("first_run")))
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
mod_forecasting_viewer_server <- function(id) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

  output$input_table <- renderUI({
    req(available_data_tables())
      selectInput(inputId = ns("input_table"), label = "Input", choices = available_data_tables(), multiple = F)
  })

  output$output_table <- renderUI({
    req(available_data_tables())
    selectInput(inputId = ns("output_table"), label = "Output", choices = available_data_tables(), multiple = F)
  })


  output$first_run <- renderUI({
    req(available_data_tables(), input$output_table)
    actionButton(inputId = ns("first_run"), label = "Run",style = "color: #ffffff; background-color: #007bff; border-color: #007bff;")
  })



  observeEvent(input$first_run,{
    db_conn <- db_conn_function(dbms = "postgresql",
                                user = input$username, password = input$password,
                                port = input$port , host = input$host, db_name = input$db_name)

    available_tables <- DBI::dbListTables(conn = db_conn)

    output_data_result(output_data_function(selected_table_output = input$output_table,
                                            db_conn = db_conn,
                                            available_tables = available_tables))

    input_data_result(input_data_function(selected_table_input = input$input_table,
                                          db_conn = db_conn,
                                          available_tables = available_tables))

    merged_data_result(merged_data_function(historical_data = input_data_result(),
                                            prediction_data = output_data_result(),
                                            date_start = output_data_result()$date_start ,
                                            date_end = output_data_result()$date_end))





    updateTabsetPanel(session, "forecastViz_tabbox", selected = "Forecasting Viewer")

  })

#=====Forecating Viewer ==================

  output$as_of <- renderUI({
  req(output_data_result())
  execution_dates <- base::unique(output_data_result()$as_of)
  selectInput(inputId = ns("as_of"), label = "As_Of", choices = c(NA , execution_dates) , multiple = FALSE)
  })

    output$kpi_value <- renderUI({
      req(input$as_of)
      req(merged_data_result())
      kpi_values <- base::unique(merged_data_result()$family)
      selectInput(inputId = ns("kpi_value"), label = "KPIs", choices = kpi_values, multiple = FALSE)
    })

    output$model <- renderUI({
      req(input$as_of)
      req(merged_data_result())
    model_names <- base::unique(merged_data_result()$`_model_desc`)
    selectInput(inputId = ns("model"), label = "Model", choices = model_names, multiple = FALSE)
  })



    output$run <- renderUI({
     req(merged_data_result())
     req(input$kpi_value, input$model)
     actionButton(inputId = ns("run"), label = "Run",style = "color: #ffffff; background-color: #007bff; border-color: #007bff;")
    })

    observeEvent(input$run,{
      merged_data_filtred_result <<- merged_data_filtred(merged_data = merged_data_result(),
                                                        kpi_value = input$kpi_value ,
                                                        model = input$model)

      if (!is.null(merged_data_filtred_result)) {
        output$graph_output <- renderUI({
          create_time_series_plot(data = merged_data_filtred_result)
        })
      } else {
       output$graph_output <- renderText("Aucune donnée disponible pour les filtres sélectionnés.")
      }
    })


    db_conn_function <- function(dbms = "postgresql" ,server = NULL, user = NULL, password = NULL, port = NULL , host = NULL, db_name = NULL)  {
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






