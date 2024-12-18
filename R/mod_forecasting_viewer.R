#' mod_forecasting_viewer_ui
#' @export


mod_forecasting_viewer_ui <- function(id) {
  ns <- NS(id)

  bs4Dash::tabBox(
    id = ns("forecastViz_tabbox"), width = 12,
    tabPanel(
      title = "Forecasting Pipelines",
      fluidPage(
        DT::dataTableOutput(ns("pipelines_table")),
        uiOutput(ns("forecast_pipeline_boxes"))
      )
    ),
    tabPanel(
      title = "Forecasting Viewer",
      fluidPage(
        fluidRow(
          column(width = 2, uiOutput(ns("as_of"))),
          column(width = 2, uiOutput(ns("kpi_value"))),
          column(width = 2, uiOutput(ns("model"))),
          column(width = 2, uiOutput(ns("aggregate_by"))),
          column(width = 2, uiOutput(ns("aggregation"))),
          column(width = 1, uiOutput(ns("run"))),
          column(width = 1, uiOutput(ns("accuracy")))
        ),
        uiOutput(ns("graph_output")),
        uiOutput(ns("accuracy_output"))
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

    # help and infos
    output$infos_and_help <- renderUI({
      fluidRow(
        column(width = 10, uiOutput(ns("void_object"))),
        column(width = 1, th2utils::mod_th2_infos_and_help_ui(ns("info_forecasting")))
      )
    })
    rmd_help_file <- system.file("infos_and_help/th2_forecasting.md", package = "th2utils")
    th2utils::mod_th2_infos_and_help_server(id = "info_forecasting", rmd_help_file = rmd_help_file)


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
          where tb2.permitted_users = '{user}' and tb2.object_type = 'fc'
          group by tb1.created_at, tb1.calendar_country, tb1.business_days, tb1.id, tb1.date_var, tb1.forecast_duration, tb1.group_target_var, tb1.input_id, tb1.output_id, tb1.pipeline_uuid, tb1.project_name, tb1.split_train_test, tb1.target_var, tb1.use_spark, tb1.group_by_columns,  tb_in.param1, tb_in.data_source, tb_out.param1, tb_out.data_source"
      )

      pipelines_metadata <- th2product::fetch_data_from_db_by_sql(sql)
      pipelines_metadata
    })

    output$forecast_pipeline_boxes <- renderUI({
      pipelines_list <- pipelines_metadata()
      temp <- seq_len(nrow(pipelines_list))

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
          box_color = "primary",
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

      if (length(selected_row) > 0) {
        selected_info(pipelines_metadata()[selected_row, ])

        output_connection <- th2product::decrypt_column(selected_info()["output_meta_connection"])

        decrypted_output_connection <- jsonlite::fromJSON(output_connection)

        tryCatch(
          {
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
          },
          error = function(err) {
            shinyalert::shinyalert("Error loading output data. Please check the output configuration.", type = "error")
          }
        )
      }
    })

    # =====Forecating Viewer ==================

    output$as_of <- renderUI({
      req(output_data_result())
      req(selected_info())
      execution_dates <- base::unique(output_data_result()$execution_date)
      selectInput(inputId = ns("as_of"), label = "As Of", choices = c("", format(execution_dates, "%Y-%m-%d %H:%M:%S")))
    })

    observeEvent(input$as_of, {
      input_connection <- th2product::decrypt_column(selected_info()["input_meta_connection"])
      decrypted_input_connection <- jsonlite::fromJSON(input_connection)

      tryCatch(
        {
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
        },
        error = function(err) {
          print(err)
          shinyalert::shinyalert("Error loading input data. Please check the input configuration.", type = "error")
        }
      )
    })


    output$kpi_value <- renderUI({
      req(input$as_of)
      req(selected_info())
      req(input_data_result())

      group_target_var <- selected_info()$group_target_var
      group_target_var <- tolower(group_target_var)
      kpi_values <- base::unique(input_data_result()[group_target_var])
      selectInput(inputId = ns("kpi_value"), label = "KPIs", choices = kpi_values, multiple = FALSE)
    })

    output$model <- renderUI({
      req(input$as_of)
      req(output_data_result())
      model_names <- base::unique(output_data_result()$`_model_desc`)
      selectInput(inputId = ns("model"), label = "Model", choices = model_names, multiple = FALSE)
    })


    output$aggregate_by <- renderUI({
      req(input$as_of)
      selectInput(inputId = ns("agg_by"), "Aggregate by", choices = c("days", "weeks"))
    })

    output$aggregation <- renderUI({
      req(input$as_of)
      req(input$agg_by)
      if (input$agg_by == "weeks") {
        selectInput(inputId = ns("agg_type"), "Aggregation", choices = c("Sum" = "sum", "Mean" = "mean", "Max" = "max", "Min" = "min"))
      } else {
        NULL
      }
    })

    output$run <- renderUI({
      req(input_data_result())
      req(output_data_result())
      req(input$kpi_value, input$model)
      actionButton(inputId = ns("run"), label = "Run", icon = icon("play"), style = "margin-top: 28px; color: #ffffff; background-color: #013DFF; border-color: #013DFF;")
    })

    output$accuracy <- renderUI({
      req(input_data_result())
      req(output_data_result())
      req(input$kpi_value, input$model)
      actionButton(inputId = ns("accuracy"), label = "Accuracy", icon = icon("search"), style = "margin-top: 28px; color: #ffffff; background-color: #013DFF; border-color: #013DFF;")
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

      prediction_data_aggregated <- prediction_data_filtred_result %>%
        dplyr::filter(execution_date == input$as_of)
      date_var <- tolower(selected_info()$date_var)
      target_var <- tolower(selected_info()$target_var)
      historical_data_aggregated <- historical_data_filtred_result %>%
        dplyr::group_by_at(dplyr::vars(date_var)) %>%
        dplyr::summarise_at(dplyr::vars(target_var), sum)



      # Ajustement du nombre de lignes de historical_data_aggregated
      prediction_rows <- nrow(prediction_data_aggregated)
      historical_rows <- prediction_rows * 3

      if (nrow(historical_data_aggregated) >= historical_rows) {
        historical_data_aggregated <- tail(historical_data_aggregated, historical_rows)
      } else {
        # Si historical_data_aggregated contient moins de lignes que nécessaire
        historical_data_aggregated <- historical_data_aggregated
      }

      historical_data_aggregated2 <<- historical_data_aggregated
      prediction_data_aggregated2 <<- prediction_data_aggregated

      fcast_meta <<- selected_info()
      if (!is.null(historical_data_aggregated) && !is.null(prediction_data_aggregated)) {
        if (input$agg_by == "days") {
          create_time_series_plot(historical_data = historical_data_aggregated,
                                  prediction_data = prediction_data_aggregated,
                                  x_var = selected_info()$date_var,
                                  y_var = selected_info()$target_var)
        } else {
          create_weekly_bar_chart(historical_data = historical_data_aggregated, prediction_data = prediction_data_aggregated, x_var = selected_info()$date_var, y_var = selected_info()$target_var, agg_type = input$agg_type)
        }
      } else {
        renderText("No data available for selected filters.")
      }
    })


    # =================== Accuracy

    accuracy_to_visualize <- eventReactive(input$accuracy, {
      prediction_data_filtred_result <- output_data_result() %>%
        dplyr::filter(output_data_result()[[selected_info()$group_target_var]] == input$kpi_value)

      historical_data_filtred_result <- historical_data_filtred(
        historical_data = input_data_result(),
        group_target_var = selected_info()$group_target_var,
        kpi_value = input$kpi_value
      )
      historical_data_filtred_result <- historical_data_filtred_result %>%
        dplyr::filter(historical_data_filtred_result[[selected_info()$date_var]] >= min(prediction_data_filtred_result[[selected_info()$date_var]]))

      # if (input$agg_type == "sum") {
      historical_data_aggregated <- historical_data_filtred_result %>%
        dplyr::group_by_at(dplyr::vars(selected_info()$date_var)) %>%
        dplyr::summarise_at(dplyr::vars(selected_info()$target_var), sum)
      # }

      benchmarking_models_test <- th2_benchmarking(historical_data_aggregated, prediction_data_filtred_result, group_target = NULL, group_value = NULL, target_var = selected_info()$target_var, as_of = input$as_of)

      showModal(modalDialog(
        title = "Benchmark",
        renderTable(benchmarking_models_test)
      ))
    })

    # ====================== Graphs output
    output$graph_output <- renderUI({
      data_to_visualize()
    })
    # ====================== Accuracy output
    output$accuracy_output <- renderUI({
      accuracy_to_visualize()
    })
  })
}
