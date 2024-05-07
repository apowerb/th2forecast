


#' mod_forecasting_viewer_ui
#' @export


mod_forecasting_viewer_ui <- function(id) {
  ns <- NS(id)

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
      column(width = 2, uiOutput(ns("input_table"))),
      column(width = 2, uiOutput(ns("output_table"))),
      column(width = 2, uiOutput(ns("kpi_value"))),
      column(width = 2, uiOutput(ns("run")))

      ),
    uiOutput(ns("graph_output"))
    )
}



#'mod_forecasting_viewer_server
#' @export
mod_forecasting_viewer_server <- function(id) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns



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


    output$kpi_value <- renderUI({
      req(available_data_tables(), input$output_table)
      selected_table_output <- input$output_table

     db_conn <- db_conn_function(dbms = "postgresql",
                      user = input$username, password = input$password,
                      port = input$port , host = input$host, db_name = input$db_name)

      available_tables <- DBI::dbListTables(conn = db_conn)

       if (!selected_table_output %in% available_tables) {
        DBI::dbDisconnect(db_conn)
        stop(paste("La table ",selected_table_output, "n'existe pas dans la base de données."))
      }else{
        query_statement <- glue::glue(
          paste("SELECT distinct(family) FROM " , selected_table_output)
        )}
      query_res <- DBI::dbSendQuery(db_conn, statement = query_statement)
      #Récupération des résultats de la requête
      kpi_values <- DBI::dbFetch(query_res)
      DBI::dbDisconnect(db_conn)
      selectInput(inputId = ns("kpi_value"), label = "KPIs", choices = kpi_values, multiple = FALSE)

    })


    output$run <- renderUI({
      req(available_data_tables(), input$output_table, input$kpi_value)
      actionButton(inputId = ns("run"), label = "Run",style = "color: #ffffff; background-color: #007bff; border-color: #007bff;")
    })

#========= forecesting viewer
    observeEvent(input$run,{
      db_conn <- db_conn_function(dbms = "postgresql",
                                  user = input$username, password = input$password,
                                  port = input$port , host = input$host, db_name = input$db_name)

      available_tables <- DBI::dbListTables(conn = db_conn)

      merged_data_result <- merged_data(selected_table_input = input$input_table,
                                        selected_table_output = input$output_table,
                                        db_conn = db_conn,
                                        available_tables = available_tables)

      print(head(merged_data_result))

      # output$graph_output <-

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




