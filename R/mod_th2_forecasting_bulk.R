#' forecast_train_mod_ui2
#' @description Saldae Dashboard module UI : time based aggregator
#' @author Farid Azouaou
#' @param id  server module ID
#' @param div_width dimension information about the framework(html object)
#' @param mod_title module title (default NULL)
#' @return UI module
#' @export

forecast_train_mod_ui2 <- function(id, mod_title = NULL, div_width = "col-xs-12 col-sm-6 col-md-8") {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("aggregator_board_box")),
    uiOutput(ns("graphs_ui"))
  )
}





#' forecast_train_mod_server2
#' @description tbd
#' @author Farid Azouaou
#' @param input  input shinydashboard elements containing information to use for output generation
#' @param output output shinydashboard element
#' @param session shiny session
#' @param tisefka reactive object containing data
#' @param div_width dimension information about the framework(html object)
#' @return output objects to be displayed in corresponding UI module
#' @export

forecast_train_mod_server2 <- function(id, div_width = "col-xs-6 col-sm-12 col-md-6") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ml_project_meta <- th2ml::th2_load_ml_project(ml_dir = ml_dir, action = "load")
    s3_bucket <- ifelse(Sys.getenv("WORKING_MODE") == "dev", paste0(ml_project_meta$company_name, "-dev"), ml_project_meta$company_name)
    s3_prefix <- ifelse(is.null(ml_project_meta$data_pool), "thaink2_data_pool/", paste0(ml_project_meta$data_pool, "/"))

    button_theme <- th2utils::add_button_theme()


    mod_refresh_file <- th2product::create_refresh_helper_file(mod_id = "th2forecasting")

    refresh_statement <- shiny::reactiveFileReader(intervalMillis = 1000, session = session, filePath = mod_refresh_file, readFunc = readRDS)

    observeEvent(input$refresh, {
      mod_refresh_file <- th2product::create_refresh_helper_file(mod_id = "th2forecasting")
    })



    object_creator <- Sys.getenv("SHINYPROXY_USERNAME")
    if (object_creator == "") object_creator <- "thaink2"

    available_data <- reactive({
      req(refresh_statement())

      saved_data_meta <- th2blender::get_data_meta(s3_bucket = s3_bucket, s3_prefix = s3_prefix)
      if (is.null(saved_data_meta) || nrow(saved_data_meta) == 0) {
        return(NULL)
      }
      saved_data_meta %>% dplyr::pull(name)
    })

    #### UI ####
    rmd_help_file <- system.file("infos_and_help/th2_insights.md", package = "th2utils")
    th2utils::mod_th2_infos_and_help_server(id = "info_forecasting_help", rmd_help_file = rmd_help_file)

    output$infos_and_help <- renderUI({
      fluidRow(
        column(width = 10, uiOutput(ns("void_object"))),
        column(width = 1, th2utils::mod_th2_infos_and_help_ui(ns("info_forecasting_help")))
      )
    })

    output$selected_bi_data <- renderUI({
      req(available_data())
      shinyWidgets::pickerInput(inputId = ns("selected_bi_data"), label = "Select Data", choices = available_data(), multiple = FALSE)
    })

    output$load_ml_data <- renderUI({
      req(available_data())
      req(input$selected_bi_data)
      actionButton(inputId = ns("load_ml_data"), label = "Load Data", style = button_theme, icon = icon("upload"), class = "btn-primary")
    })
    data_is_loaded <- reactiveVal(NULL)

    tisefka <- eventReactive(input$load_ml_data, {
      req(input$selected_bi_data)
      data <- th2ml::load_ml_models_for_api_s3(target_model_name = input$selected_bi_data, s3_bucket = s3_bucket, s3_prefix = s3_prefix) %>%
        janitor::clean_names()
      data_is_loaded(input$selected_bi_data)
      return(data)
    })


    # tisefka <- reactive({
    #   ggplot2::diamonds
    # })
    data_diag <- reactive({
      req(tisefka())
      req(input$load_ml_data)
      dt_diag <- th2reporting::data_diagnosis_f(tisefka())
      return(dt_diag)
    })

    categoricals_unique_values <- reactive({
      req(input$load_ml_data)
      req(data_diag())
      data_diag()$categoricals_unique_values
    })

    non_numeric_variables <- reactive({
      req(categoricals_unique_values())
      req(input$load_ml_data)
      var_granularity <- data_diag()$diagnosis %>%
        dplyr::filter(!types %in% c("numeric", "integer", "Date", "POSIXct")) %>%
        dplyr::pull(variables)
      return(var_granularity)
    })

    output$aggregator_board_box <- renderUI({
      current_data_meta <- available_data()
      if (is.null(current_data_meta) || length(current_data_meta) == 0) {
        tags$div(
          class = "alert alert-danger",
          style = "margin: 15px;",
          "No data available."
        )
      } else {
        fluidPage(
          fluidRow(
            column(width = 11, uiOutput(ns("infos_and_help"))),
            column(
              width = 1,
              actionButton(ns("refresh"), "", icon = icon("arrows-rotate"), style = th2utils::add_button_theme(), class = "btn-primary")
            )
          ),
          fluidRow(
            column(width = 4, uiOutput(ns("selected_bi_data"))),
            column(width = 3, br(), uiOutput(ns("load_ml_data")))
          ),
          fluidRow(
            column(width = 2, uiOutput(ns("fc_date_var"))),
            column(width = 2, uiOutput(ns("fc_target_var"))),
            column(width = 2, uiOutput(ns("var_granularity"))),
            column(width = 1, uiOutput(ns("input_time_freq"))),
            column(width = 1, br(), uiOutput(ns("submit"))),
            column(width = 2, uiOutput(ns("fc_models_list"))),
            column(width = 2, uiOutput(ns("fc_split_train_test"))),
            column(width = 2, br(), uiOutput(ns("fc_chk_business_days")))
          ),
          uiOutput(ns("non_numeric_variables_inputs"))
        )
      }
    })


    output$submit <- renderUI({
      req(input$fc_date_var)
      req(input$fc_target_var)
      req(input$load_ml_data)
      req(input$fc_models_list)
      bs4Dash::actionButton(inputId = ns("submit"), label = ("Start"), icon = icon("play"), style = th2utils::add_button_theme())
    })

    observeEvent(eventExpr = non_numeric_variables(), handlerExpr = {
      non_numeric_variables() %>% purrr::imap(~ {
        output_name_app <- paste0("non_numeric_variables_", .x)
        output[[output_name_app]] <- renderUI({
          shinyWidgets::pickerInput(
            inputId = ns(output_name_app),
            label = gsub("_", " ", .x),
            choices = categoricals_unique_values()[[.x]],
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = TRUE
          )
        })
      })
    })

    output$non_numeric_variables_inputs <- renderUI({
      req(non_numeric_variables())
      req(input$fc_target_var)
      req(input$fc_date_var)
      req(input$load_ml_data)
      fluidRow(
        purrr::map(non_numeric_variables(), ~ {
          column(width = 2, uiOutput(ns(paste0("non_numeric_variables_", .x))))
        })
      )
    })

    output$fc_models_list <- renderUI({
      req(input$fc_date_var)
      req(input$fc_target_var)
      req(input$load_ml_data)
      shinyWidgets::pickerInput(
        inputId = ns("fc_models_list"),
        label = "Models",
        options = list(`actions-box` = TRUE),
        choices = c(
          "XGBoost" = "xgboost",
          "ARIMA" = "arima",
          "Prophet" = "prophet",
          "Linear regression" = "lr",
          "Random Forest" = "random_forest",
          "Mars" = "mars",
          "ARIMAX" = "arimax"
        ),
        multiple = TRUE
      )
    })

    output$fc_date_var <- renderUI({
      req(input$load_ml_data)
      req(data_diag())
      fc_date_var <- data_diag()$diagnosis %>%
        dplyr::filter(types %in% c("Date", "POSIXct", "POSIXt")) %>%
        dplyr::pull(variables)

      shinyWidgets::pickerInput(
        inputId = ns("fc_date_var"),
        label = ("Date"),
        multiple = FALSE,
        choices = fc_date_var,
        selected = NULL
      )
    })

    output$fc_target_var <- renderUI({
      req(input$load_ml_data)
      req(input$fc_date_var)
      req(data_diag())
      num_vars <- data_diag()$diagnosis %>%
        dplyr::filter(types %in% c("numeric", "integer")) %>%
        dplyr::pull(variables)

      shinyWidgets::pickerInput(
        inputId = ns("fc_target_var"),
        label = ("Target variables"),
        multiple = TRUE,
        choices = num_vars,
        selected = NULL
      )
    })
    output$var_granularity <- renderUI({
      req(input$load_ml_data)
      req(input$fc_target_var)
      req(input$fc_date_var)
      req(data_diag())
      var_granularity <- data_diag()$diagnosis %>%
        dplyr::filter(!types %in% c("numeric", "integer", "Date", "POSIXct")) %>%
        dplyr::pull(variables)
      var_granularity <- var_granularity[var_granularity != input$fc_date_var]
      if (length(var_granularity) == 0) {
        return(NULL)
      }
      shinyWidgets::pickerInput(
        inputId = ns("var_granularity"),
        label = ("Granularity"),
        multiple = TRUE,
        choices = var_granularity,
        selected = NULL
      )
    })
    # aggregation metric
    output$aggregation_metric <- renderUI({
      req(input$load_ml_data)
      req(tisefka())
      aggregation_choices <- c("Sum", "Average", "Median", "Count")
      names(aggregation_choices) <- aggregation_choices
      shinyWidgets::pickerInput(
        inputId = ns("aggregation_metric"),
        label = ("Aggregation"),
        multiple = FALSE,
        selected = aggregation_choices[1],
        choices = aggregation_choices
      )
    })

    tisefka_iheggan <- eventReactive(input$submit, {
      req(tisefka())
      req(input$fc_target_var)
      req(input$fc_date_var)
      req(input$input_time_freq)
      tisefka_iheggan <- tisefka() %>%
        prepare_input_fcast_data(
          raw_data = .,
          non_numeric_variables = non_numeric_variables(),
          input_object = input,
          input_time_freq = input$input_time_freq,
          var_granularity = input$var_granularity,
          fc_target_var = input$fc_target_var,
          fc_date_var = input$fc_date_var
        )
      return(tisefka_iheggan)
    })


    ts_time_units <- reactive({
      req(tisefka())
      req(input$fc_date_var)
      date_freq <- tisefka() %>%
        dplyr::pull(!!input$fc_date_var) %>%
        th2reporting::possible_units_for_summary(time_vect = .)
      return(date_freq)
    })
    #---------------------------------------
    output$input_time_freq <- renderUI({
      req(ts_time_units())
      shinyWidgets::pickerInput(
        inputId = ns("input_time_freq"),
        label = ("Input freq"),
        choices = ts_time_units()
      )
    })
    #----------------
    output$fc_split_train_test <- renderUI({
      req(input$fc_date_var)
      req(input$fc_target_var)
      req(tisefka())
      # split_value <- tisefka()%>%dplyr::pull(!!input$fc_date_var)%>%max()
      split_value <- Sys.Date()
      dateInput(inputId = ns("fc_split_train_test"), label = "Split", value = split_value)
    })

    tisefka_aggregated_all <- eventReactive(input$submit, {
      req(input$fc_date_var)
      req(input$fc_target_var)
      req(tisefka_iheggan())
      calendar_country <- ""
      calendar_column <- NULL
      if (input$fc_chk_business_days == TRUE) {
        req(input$fc_calendar_list)
        if (input$fc_calendar_list == "in_data") {
          req(input$fc_calendar_column)
          calendar_country <- input$fc_calendar_list
          calendar_column <- input$fc_calendar_column
        } else {
          calendar_country <- input$fc_calendar_list
        }
      }
      fc_meta_data <- list(
        # group_target = input$fc_group_target,
        target_var = "actuals",
        date_var = input$fc_date_var,
        future_forecast = 120,
        models_list = input$fc_models_list,
        business_days = input$fc_chk_business_days,
        split_train_test = input$fc_split_train_test,
        calendar_country = calendar_country,
        calendar_column = calendar_column,
        use_spark = input$fc_chk_spark,
        group_target = "target_vars"
      )

      fc_result <- forecast_transform_data(
        fc_data = tisefka_iheggan(),
        fc_meta_data = fc_meta_data
      )
      return(fc_result)
    })

    output$fc_chk_business_days <- renderUI({
      req(input$fc_date_var)
      req(input$fc_target_var)
      req(input$load_ml_data)
      checkboxInput(
        inputId = ns("fc_chk_business_days"),
        label = "B.H.",
        value = FALSE
      )
    })
    tisefka_aggregated <- reactive({
      req(tisefka_aggregated_all())
      req(input$fc_date_var)
      req(input$fc_target_var)
      tisefka_aggregated <- tisefka_aggregated_all()
      colnames(tisefka_aggregated) <- gsub("[[:punct:]]| ", "_", colnames(tisefka_aggregated))
      return(tisefka_aggregated)
    })

    fc_target_var <- reactive({
      req(tisefka_aggregated())
      fc_target_var <- unique(tisefka_aggregated()$target_vars)
      fc_target_var <- fc_target_var[!fc_target_var %in% c(input$fc_date_var, input$var_granularity)]
      names(fc_target_var) <- fc_target_var
      fc_target_var <- head(fc_target_var, 15)
      return(fc_target_var)
    })

    #---------------------
    output$graphs_ui <- renderUI({
      req(fc_target_var())
      req(tisefka_aggregated_all())
      req(input$submit)
      hist_data <- tisefka_iheggan()
      plots_list <- purrr::map(fc_target_var(), ~ {
        mod_id <- th2product::generateID("fcast_view")
        fcast_inputs <- list(
          target_var = .x, date_var = input$fc_date_var,
          historical_data_aggregated = hist_data %>%
            dplyr::filter(target_vars == !!.x) %>%
            dplyr::select(!!input$fc_date_var, actuals),
          prediction_data_aggregated = tisefka_aggregated() %>%
            dplyr::filter(target_vars == !!.x)
        )
        mod_basic_fcast_viewer_server(mod_id, fcast_inputs = fcast_inputs)
        column(
          width = 6,
          mod_basic_fcast_viewer_ui(ns(mod_id))
        )
      })
      fluidRow(plots_list)
    })
  })
}
