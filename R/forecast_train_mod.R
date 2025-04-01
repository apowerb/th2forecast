#' FairML Dashboard Module UI (analytics)
#' @description FairML Dashboard module UI : forecasting
#' @author Farid Azouaou
#' @param id  server module ID
#' @param div_width dimension information about the framework(html object)
#' @param mod_title module title (default NULL)
#' @return UI module
#' @export

forecast_train_mod_ui <- function(id,div_width = "col-xs-12 col-sm-6 col-md-8") {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(width = 12, uiOutput(ns("features_engineering_box")))
    ),
    fluidRow(
      column(width = 12, uiOutput(ns("forecast_train_box")))
    )
  )
}

#' FairML Dashboard Module Server Analytics
#' @description FairML Dashboard module SERVER : render and generate multiple output objects for analytics
#' @author Farid Azouaou
#' @param input  input bs4Dash elements containing information to use for output generation
#' @param output output bs4Dash element
#' @param session shiny session
#' @param tisefka reactive object containing data
#' @param div_width dimension information about the framework(html object)
#' @return output objects to be displayed in corresponding UI module
#' @export

forecast_train_mod_server <- function(id, div_width = "col-xs-6 col-sm-12 col-md-6") {
  moduleServer(id, function(input, output, session) {
  ns <- session$ns

  ml_project_meta <- th2ml::th2_load_ml_project(ml_dir = ml_dir, action = "load")
  s3_bucket <-ifelse(Sys.getenv("WORKING_MODE") == "dev", paste0(ml_project_meta$company_name, "-dev"), ml_project_meta$company_name)
  s3_prefix <- ifelse(is.null(ml_project_meta$data_pool), "thaink2_data_pool/", paste0(ml_project_meta$data_pool, "/"))

  button_theme <- th2utils::add_button_theme()


  mod_refresh_file <- th2product::create_refresh_helper_file(mod_id = "feature_engineering")

  refresh_statement <- shiny::reactiveFileReader(intervalMillis = 1000, session = session, filePath = mod_refresh_file, readFunc = readRDS)

  observeEvent(input$refresh, {
    mod_refresh_file <- th2product::create_refresh_helper_file(mod_id = "feature_engineering")
  })



  object_creator <- Sys.getenv("SHINYPROXY_USERNAME")
  if (object_creator == "") object_creator <- "thaink2"


  list_of_users <- th2utils::get_list_of_users()
  list_of_users <- list_of_users[list_of_users != object_creator]

  user_permissions <- reactive({
    req(refresh_statement())
    # user_permissions <- grant_ml_permission(s3_bucket = s3_bucket, s3_prefix = s3_prefix, permission_action = "get", object_creator = object_creator)
    start_time <- Sys.time()
    user_permissions <- th2product::grant_ml_permission_db(
      target_table = "th2_data_permissions",
      meta = list(
        current_user = object_creator,
        CURRENT_OBJECT_TYPE = "db_table"
      ),
      permission_action = "get"
    ) %>%
      dplyr::distinct(OBJECT_ID, .keep_all = TRUE)
    print(paste("DB connected in ", Sys.time() - start_time, "seconds"))
    return(user_permissions)
  })



  available_data <- reactive({
    req(refresh_statement())
    saved_data_meta <- th2ml::get_ml_model_metadata(s3_bucket, s3_prefix, pin_object = "data")
    if (nrow(saved_data_meta) == 0) {
      return(NULL)
    }
    if (nrow(user_permissions()) == 0) {
      return(NULL)
    }
    saved_data_meta <- user_permissions() %>%
      dplyr::rename(name = OBJECT_ID) %>%
      dplyr::select(name) %>%
      dplyr::inner_join(saved_data_meta, by = c("name"))

    if (nrow(saved_data_meta) == 0) {
      return(NULL)
    }
    saved_data_meta <- saved_data_meta %>% dplyr::pull(name)
    return(saved_data_meta)
  })


  output$load_ml_data <- renderUI({
    req(available_data())
    req(input$selected_ml_data)
    actionButton(inputId = ns("load_ml_data"), label = "Load Data", style = button_theme, icon = icon("upload"), class = "btn-primary")
  })
  data_is_loaded <- reactiveVal(NULL)

  result_forecast <- reactiveVal(NULL)

  tisefka_with_features <- reactiveVal(NULL)

  missing_value_variable <- reactiveValues()

  tisefka_tizegzawin <- reactiveVal(NULL)
  data_to_visualize <- reactiveVal(NULL)

  observeEvent(input$load_ml_data, {
    print("tisefka_tizegzawin")

    data <- th2ml::load_ml_models_for_api_s3(target_model_name = input$selected_ml_data, s3_bucket = s3_bucket, s3_prefix = s3_prefix)

    df_diagnose <- data %>% dlookr::diagnose()

    for (i in 1:nrow(df_diagnose)) {
      variable_name <- df_diagnose$variables[i]
      variable_type <- df_diagnose$types[i]
      missing_count <- df_diagnose$missing_count[i]
      missing_percent <- df_diagnose$missing_percent[i]
      missing_value_variable[[variable_name]] <- list(
        type = variable_type,
        missing_count = missing_count,
        missing_percent = missing_percent
      )
    }

    result_forecast(NULL)
    data_to_visualize(NULL)
    data_is_loaded(input$selected_ml_data)
    tisefka_tizegzawin(data)
  })
  output$infos_and_help <- renderUI({
    fluidRow(
      column(width = 10, uiOutput(ns("void_object"))),
      column(width = 1, th2utils::mod_th2_infos_and_help_ui(ns("info_features_engi_upload")))
    )
  })

  rmd_help_file <- system.file("infos_and_help/th2_features_engineering.md", package = "th2utils")
  th2utils::mod_th2_infos_and_help_server(id = "info_features_engi_upload", rmd_help_file = rmd_help_file)
  output$features_engineering_box <- renderUI({
    req(available_data())
    tagList(
      fluidRow(
        column(width = 11, uiOutput(ns("infos_and_help"))),
        column(
          width = 1,
          actionButton(ns("refresh"), "", icon = icon("arrows-rotate"), style = th2utils::add_button_theme(), class = "btn-primary")
        )
      ),
      fluidRow(
        column(
          width = 4,
          uiOutput(ns("selected_ml_data"))
        ),
        column(
          width = 3, br(),
          uiOutput(ns("load_ml_data"))
        )
      ),
      uiOutput(ns("fc_variables"))
    )
  })

  output$fc_variables <- renderUI({
    req(data_is_loaded())
    if(is.null(result_forecast())){
      fluidRow(
        column(width = 3, uiOutput(ns(
          "fc_group_target"
        ))),
        column(width = 1, uiOutput(ns(
          "fc_chk_columns"
        ))),
        column(width = 3, uiOutput(ns(
          "fc_target_var"
        ))),
        column(width = 2, uiOutput(ns(
          "fc_date_var"
        ))),
        column(width = 2, uiOutput(ns(
          "fc_future_forecast"
        ))),
        column(width = 1, uiOutput(ns(
          "fc_chk_business_days"
        ))),
        column(width = 3, uiOutput(ns(
          "fc_models_list"
        ))),
        column(width = 3, uiOutput(ns(
          "fc_split_train_test"
        ))),
        column(width = 2, uiOutput(ns(
          "fc_calendar_list"
        ))),
        column(width = 2, uiOutput(ns(
          "fc_calendar_column"
        ))),
        column(width = 2, uiOutput(ns(
          "fc_chk_spark"
        ))),
        column(
          width = 2, br(),
          uiOutput(ns("generate_features"))),

      )
    }else{
      fluidRow(
        column(width = 2, uiOutput(ns("kpi_value"))),
        column(width = 2, uiOutput(ns("model"))),
        column(width = 2, uiOutput(ns("aggregate_by"))),
        column(width = 2, uiOutput(ns("aggregation"))),
        column(width = 1, uiOutput(ns("run")))
      )
    }
  })
  output$selected_ml_data <- renderUI({
    req(available_data())
    selectInput(inputId = ns("selected_ml_data"), label = "Select Data", choices = available_data(), multiple = FALSE)
  })

  observeEvent(input$selected_ml_data, {
    print("selected_ml_data")
    data_is_loaded(NULL)
  })

  output$fc_group_target <- renderUI({
    # req(input$data_source)
    list_group_target <- colnames(tisefka_tizegzawin())
    shinyWidgets::pickerInput(
      inputId = ns("fc_group_target"), label = "Group Target",
      choices = list_group_target,
      selected = list_group_target
    )

  })

  output$fc_chk_columns <- renderUI({
    checkboxInput(
      inputId = ns("fc_chk_columns"),
      label = tags$span(style = "font-size: 12px;", "Columns"),
      value = FALSE
    )
  })

  output$fc_target_var <- renderUI({
    # req(input$data_source)
    list_target_var <- colnames(tisefka_tizegzawin())
    shinyWidgets::pickerInput(
      inputId = ns("fc_target_var"), label = "Target variable",
      choices = list_target_var
    )
  })

  dates_yellan <- reactive({
    req(tisefka_tizegzawin())
    dates_yellan <- th2reporting::detect_possible_date_var(tisefka_tizegzawin())
    if (length(dates_yellan) == 0) {
      th2product::th_shinyalert("Data Upload",
                                text = "Date variable not available", type = "error",
                                closeOnClickOutside = TRUE, showConfirmButton = TRUE
      )
    }
    return(dates_yellan)
  })

  output$fc_date_var <- renderUI({
    req(dates_yellan())
    date_choices <- dates_yellan()
    shinyWidgets::pickerInput(
      inputId = ns("fc_date_var"),
      label = "Date variable",
      choices = date_choices
    )

  })


  output$fc_future_forecast <- renderUI({
    numericInput(
      inputId = ns("fc_future_forecast"),
      label = "Duration", value = 50
    )
  })

  output$fc_chk_business_days <- renderUI({
    checkboxInput(
      inputId = ns("fc_chk_business_days"),
      label = "B.H.",
      value = FALSE
    )
  })

  output$fc_models_list <- renderUI({
    # req(input$data_source)
    shinyWidgets::pickerInput(
      inputId = ns("fc_models_list"),
      label = "Models",
      options = list(`actions-box` = TRUE),
      choices = c("ARIMA" = "arima", "Prophet" = "prophet", "Mars" = "mars", "Linear regression" = "lr", "Random Forest" = "random_forest", "XGBoost" = "xgboost", "ARIMAX" = "arimax"),
      multiple = TRUE
    )
  })

  output$fc_split_train_test <- renderUI({
    dateInput(inputId = ns("fc_split_train_test"), "Split train:")
  })

  observeEvent(input$fc_chk_columns, {
    chk_columns <- input$fc_chk_columns
    if (chk_columns == TRUE) {
      updateTextInput(session, "fc_group_target", value = "all_columns")
      shinyjs::disable("fc_group_target")
    } else {
      updateTextInput(session, "fc_group_target", value = "")
      shinyjs::enable("fc_group_target")
    }
  })

  observeEvent(input$fc_chk_business_days, {
    chk_columns <- input$fc_chk_business_days

    if (chk_columns == TRUE) {
      output$fc_calendar_list <- renderUI({
        selectInput(
          inputId = ns("fc_calendar_list"),
          label = "Calendar",
          choices = c("Peru" = "PE", "France" = "FR", "Deutschland" = "DE", "Belgium" = "BE", "In data" = "in_data")
        )
      })
    } else {
      output$fc_calendar_list <- renderUI({
        ""
      })
      output$fc_calendar_column <- renderUI({
        ""
      })
    }
  })

  observeEvent(input$fc_calendar_list, {
    chk_columns <- input$fc_calendar_list

    if (chk_columns == "in_data") {
      output$fc_calendar_column <- renderUI({
        textInput(
          inputId = ns("fc_calendar_column"),
          label = "Calendar column",
          value = ""
        )
      })
    } else {
      output$fc_calendar_column <- renderUI({
        ""
      })
    }
  })

  output$fc_chk_spark <- renderUI({
    checkboxInput(
      inputId = ns("fc_chk_spark"),
      label = "spark execution",
      value = FALSE
    )
  })

  output$generate_features <- renderUI({
    req(data_is_loaded())
    req(input$fc_models_list)
    req(input$fc_future_forecast)
    req(input$fc_date_var)
    req(input$fc_target_var)
    req(input$fc_group_target)
    req(is.null(result_forecast()))

    actionButton(
      inputId = ns("generate_features"),
      label = "Generate",
      style = button_theme,
      icon = icon("cogs"),
      class = "btn-primary"
    )
  })

  observeEvent(input$generate_features,{
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
      group_target = input$fc_group_target,
      target_var = input$fc_target_var,
      date_var = input$fc_date_var,
      future_forecast = input$fc_future_forecast,
      models_list = input$fc_models_list,
      business_days = input$fc_chk_business_days,
      split_train_test = input$fc_split_train_test,
      calendar_country = calendar_country,
      calendar_column = calendar_column,
      use_spark = input$fc_chk_spark
    )

    fc_result <- forecast_transform_data(
      fc_data = tisefka_tizegzawin(),
      fc_meta_data = fc_meta_data
    )

    if (is.null(fc_result)) {
      return(NULL)
    }

    result_forecast(fc_result)

    shinyalert::shinyalert(
      title = "Data Training",
      text = "Data has been successfully trained",
      imageUrl = "https://raw.githubusercontent.com/thaink2/thaink2publicimages/main/thaink2_logo_circle.png",
      confirmButtonCol = "#013DFF",
      type = "success"
    )
  })

  output$kpi_value <- renderUI({
    req(tisefka_tizegzawin())

    group_target_var <- input$fc_group_target
    kpi_values <- base::unique(tisefka_tizegzawin()[group_target_var])
    selectInput(inputId = ns("kpi_value"), label = "KPIs", choices = kpi_values, multiple = FALSE)
  })

  output$model <- renderUI({
    req(result_forecast())
    model_names <- base::unique(result_forecast()$`.model_desc`)
    selectInput(inputId = ns("model"), label = "Model", choices = model_names, multiple = FALSE)
  })

  output$aggregate_by <- renderUI({
    selectInput(inputId = ns("agg_by"), "Aggregate by", choices = c("days", "weeks"))
  })

  output$aggregation <- renderUI({
    req(input$agg_by)
    if (input$agg_by == "weeks") {
      selectInput(inputId = ns("agg_type"), "Aggregation", choices = c("Sum" = "sum", "Mean" = "mean", "Max" = "max", "Min" = "min"))
    } else {
      NULL
    }
  })

  output$run <- renderUI({
    req(tisefka_tizegzawin())
    print(result_forecast())
    req(result_forecast())
    actionButton(inputId = ns("run"), label = "Run", icon = icon("play"), style = button_theme, class = "btn-primary")
  })

  observeEvent(input$run, {
    prediction_data_filtred_result <- prediction_data_filtred(
      prediction_data = result_forecast(),
      group_target_var = input$fc_group_target,
      model = input$fc_models_list,
      kpi_value = input$kpi_value
    )
    historical_data_filtred_result <- historical_data_filtred(
      historical_data = tisefka_tizegzawin(),
      group_target_var = input$fc_group_target,
      kpi_value = input$kpi_value
    )
    colnames(historical_data_filtred_result) <- lapply(colnames(historical_data_filtred_result), function(x) {
      x <- tolower(x)
    })

    date_var <- tolower(input$fc_date_var)
    target_var <- tolower(input$fc_target_var)
    prediction_data_aggregated <- prediction_data_filtred_result
    historical_data_aggregated <- historical_data_filtred_result %>%
      dplyr::group_by_at(dplyr::vars(date_var)) %>%
      dplyr::summarise_at(dplyr::vars(target_var), sum)



    # Ajustement du nombre de lignes de historical_data_aggregated
    prediction_rows <- nrow(prediction_data_filtred_result)
    historical_rows <- prediction_rows * 3

    if (nrow(historical_data_aggregated) >= historical_rows) {
      historical_data_aggregated <- tail(historical_data_aggregated, historical_rows)
    } else {
      # Si historical_data_aggregated contient moins de lignes que nécessaire
      historical_data_aggregated <- historical_data_aggregated
    }


    if (!is.null(historical_data_aggregated) && !is.null(prediction_data_aggregated)) {
      if (input$agg_by == "days") {
        data_to_visualize(create_time_series_plot(historical_data = historical_data_aggregated, prediction_data = prediction_data_aggregated, x_var = input$fc_date_var, y_var = input$fc_target_var))
      }
      else {
        data_to_visualize(create_weekly_bar_chart(historical_data = historical_data_aggregated, prediction_data = prediction_data_aggregated, x_var = input$fc_date_var, y_var = input$fc_target_var, agg_type = input$agg_type))
      }
    } else {
      data_to_visualize(renderText("No data available for selected filters."))
    }
  })

  output$forecast_train_box <- renderUI({
    req(data_is_loaded())
    data_to_visualize()
  })


  })

}
