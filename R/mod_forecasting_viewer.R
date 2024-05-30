#' mod_forecasting_viewer_ui
#' @export


mod_forecasting_viewer_ui <- function(id) {
  ns <- NS(id)

  bs4Dash::tabBox(
    id = ns("forecastViz_tabbox"), width = 12, selected = "Forecasting Pipelines",
    tabPanel(
      title = "Forecasting Pipelines", icon = icon("database"),
      fluidPage(
        DT::dataTableOutput(ns("pipelines_table")),
        uiOutput(ns("forecast_pipeline_boxes"))
      )
    ),
    tabPanel(
      title = "Forecasting Viewer", icon = icon("chart-line"),
      fluidPage(
        fluidRow(
          column(width = 2, uiOutput(ns("as_of"))),
          column(width = 2, uiOutput(ns("kpi_value"))),
          column(width = 2, uiOutput(ns("model"))),
          column(width = 2, uiOutput(ns("aggregation"))),
          column(width = 2, uiOutput(ns("run")))
        ),
        uiOutput(ns("graph_output"))
      )
    )
  )
}



#' mod_forecasting_viewer_server
#' @export
mod_forecasting_viewer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_info <- reactiveVal()
    pipelines_metadata <- reactiveVal()
    input_data_result <- reactiveVal()
    output_data_result <- reactiveVal()

    ## =============connect to Db and return pipelines metadata ======
    pipelines_metadata <- reactiveVal({
      user <- Sys.getenv("SHINYPROXY_USERNAME")
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


    # output$pipelines_table <-  DT::renderDataTable({
    #   user_permissions <- th2blender::get_user_data_permissions(target_table = "th2_wf_permissions", object_type = "fc")
    #   if (nrow(pipelines_metadata()) == 0) {
    #     return(NULL)
    #   }
    #   pipelines_metadata <- user_permissions %>%
    #     dplyr::rename(pipeline_uuid = OBJECT_ID) %>%
    #     dplyr::select(pipeline_uuid) %>%
    #     dplyr::inner_join(pipelines_metadata(), by = c("pipeline_uuid"))
    #   if (nrow(pipelines_metadata) == 0) {
    #     return(NULL)
    #   }
    #   DT::datatable(dplyr::select(pipelines_metadata,-id, -input_meta_connection, -output_meta_connection), selection = list(mode = "single"))
    # })

    output$forecast_pipeline_boxes <- renderUI({
      user_permissions <- th2blender::get_user_data_permissions(target_table = "th2_wf_permissions", object_type = "fc")

      if (nrow(pipelines_metadata()) == 0) {
        return(NULL)
      }
      pipelines_metadata <- user_permissions %>%
        dplyr::rename(pipeline_uuid = OBJECT_ID) %>%
        dplyr::select(pipeline_uuid) %>%
        dplyr::inner_join(pipelines_metadata(), by = c("pipeline_uuid"))
      if (nrow(pipelines_metadata) == 0) {
        return(NULL)
      }
      pipelines_list <- pipelines_metadata
      print(pipelines_list)
      temp <- seq_len(nrow(pipelines_metadata))
      # print(list_of_workflows()$pipelines)

      all_boxes <- fluidRow(lapply(temp, function(x) {
        created_at_value <- as.numeric(pipelines_list[x, "created_at"])
        created_at_human_readable <- format(as.POSIXct(created_at_value, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S")
        pipeline_content <- HTML(as.character(
          glue::glue(
            "<b>Name</b> : {pipelines_list[x, 'project_name']}<br>
         <b>Input datasource type</b> : {pipelines_list[x, 'input_datasource_type']}<br>
         <b>Output datasource type</b> : {pipelines_list[x, 'output_datasource_type']}<br>
         <b>Groupe target</b> : {pipelines_list[x, 'group_target_var']}<br>
         <b>Target</b> : {pipelines_list[x, 'target_var']}<br>
         <b>Forecast duration</b> : {pipelines_list[x, 'forecast_duration']}<br>
         <b>Input id</b> : {pipelines_list[x, 'input_id']}<br>
         <b>Output id</b> : {pipelines_list[x, 'output_id']}<br>
         <b>Created At</b> : {created_at_human_readable}"
          )
        ))

        # print(workflows_list)
        mod_fc_boxes_server(
          id = pipelines_list[x, "pipeline_uuid"],
          perm_table = "th2_wf_permissions",
          box_uuid = pipelines_list[x, "pipeline_uuid"],
          box_title = pipelines_list[x, "project_name"],
          box_color = "info",
          box_bg_color = "white",
          box_icon = "timeline",
          box_body = pipeline_content,
          data = pipelines_list,
          selected_info = selected_info,
          index = x,
          output_data_result = output_data_result,
          parent_session = session
        )

        column(width = 4, mod_fc_boxes_ui(id = ns(pipelines_list[x, "pipeline_uuid"])))
      }))
      return(all_boxes)
    })

    observeEvent(input$pipelines_table_rows_selected, {
      selected_row <- NULL
      selected_row <- input$pipelines_table_rows_selected
      print(selected_row)
      if (length(selected_row) > 0) {
        selected_info(pipelines_metadata()[selected_row, ])

        output_connection <- th2product::decrypt_column(selected_info()["output_meta_connection"])

        decrypted_output_connection <- jsonlite::fromJSON(output_connection)

        db_conn <- db_conn_function(
          dbms = "postgresql",
          user = decrypted_output_connection$username,
          password = decrypted_output_connection$password,
          port = decrypted_output_connection$port,
          host = decrypted_output_connection$host,
          db_name = decrypted_output_connection$database
        )

        output_data_result(output_data_fetch(
          db_conn = db_conn,
          target_table = decrypted_output_connection$target_table,
          schema = decrypted_output_connection$schema,
          target_var = selected_info()$target_var,
          group_target_var = selected_info()$group_target_var,
          date_var = selected_info()$date_var
        ))

        updateTabsetPanel(session, "forecastViz_tabbox", selected = "Forecasting Viewer")
      }
    })

    # =====Forecating Viewer ==================

    output$as_of <- renderUI({
      req(output_data_result())
      execution_dates <- base::unique(output_data_result()$as_of)
      selectInput(inputId = ns("as_of"), label = "As_Of", choices = c("", format(execution_dates, "%Y-%m-%d")))
    })

    observeEvent(input$as_of, {
      input_connection <- th2product::decrypt_column(selected_info()["input_meta_connection"])
      print(input_connection)

      decrypted_input_connection <- jsonlite::fromJSON(input_connection)
      print(decrypted_input_connection)

      db_conn <- db_conn_function(
        dbms = "postgresql",
        user = decrypted_input_connection$username,
        password = decrypted_input_connection$password,
        port = decrypted_input_connection$port,
        host = decrypted_input_connection$host,
        db_name = decrypted_input_connection$database
      )

      input_data_result(input_data_fetch(
        prediction_data = output_data_result(),
        db_conn = db_conn,
        target_table = decrypted_input_connection$target_table,
        target_var = selected_info()$target_var,
        group_target_var = selected_info()$group_target_var,
        date_var = selected_info()$date_var,
        as_of = input$as_of
      ))
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

    output$aggregation <- renderUI({
      req(input$as_of)
      selectInput(inputId = ns("agg_type"), "Aggregation", choices = c("", "Sum" = "sum"))
    })



    output$run <- renderUI({
      req(input_data_result())
      req(output_data_result())
      req(input$kpi_value, input$model)
      actionButton(inputId = ns("run"), label = "Run", style = "color: #ffffff; background-color: #007bff; border-color: #007bff;")
    })

    data_to_visualize <- eventReactive(input$run, {
      prediction_data_filtred_result <- prediction_data_filtred(
        prediction_data = output_data_result(),
        group_target_var = selected_info()$group_target_var,
        model = input$model,
        kpi_value = input$kpi_value
      )

      historical_data_filtred_result <- historical_data_filtred(
        historical_data = input_data_result(),
        group_target_var = selected_info()$group_target_var,
        kpi_value = input$kpi_value
      )


      if (input$agg_type == "sum") {
        prediction_data_aggregated <- prediction_data_filtred_result %>%
          dplyr::group_by_at(vars(selected_info()$date_var)) %>%
          dplyr::summarise(across(where(is.numeric), sum))

        historical_data_aggregated <- historical_data_filtred_result %>%
          dplyr::group_by_at(vars(selected_info()$date_var)) %>%
          dplyr::summarise_at(vars(selected_info()$target_var), sum)
      }


      # Ajustement du nombre de lignes de historical_data_aggregated
      prediction_rows <- nrow(prediction_data_aggregated)
      historical_rows <- prediction_rows * 3

      if (nrow(historical_data_aggregated) >= historical_rows) {
        historical_data_aggregated <- tail(historical_data_aggregated, historical_rows)
      } else {
        # Si historical_data_aggregated contient moins de lignes que nécessaire
        historical_data_aggregated <- historical_data_aggregated
      }


      if (!is.null(historical_data_aggregated) && !is.null(prediction_data_aggregated)) {
        create_time_series_plot(historical_data = historical_data_aggregated, prediction_data = prediction_data_aggregated, x_var = selected_info()$date_var, y_var = selected_info()$target_var)
      } else {
        renderText("Aucune donnée disponible pour les filtres sélectionnés.")
      }
    })

    # ====================== Graph output
    output$graph_output <- renderUI({
      data_to_visualize()
    })
  })
}
