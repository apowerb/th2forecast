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
  moduleServer(id,function(input, output, session){
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
      req(input$selected_bi_data)
      actionButton(inputId = ns("load_ml_data"), label = "Load Data", style = button_theme, icon = icon("upload"), class = "btn-primary")
    })
    data_is_loaded <- reactiveVal(NULL)

    tisefka <- eventReactive(input$load_ml_data,{
      req(input$selected_bi_data)
      data <- th2ml::load_ml_models_for_api_s3(target_model_name = input$selected_bi_data, s3_bucket = s3_bucket, s3_prefix = s3_prefix)%>%
        janitor::clean_names()
      data_is_loaded(input$selected_bi_data)
      return(data)
    })
    output$selected_bi_data <- renderUI({
      req(available_data())
      selectInput(inputId = ns("selected_bi_data"), label = "Select Data", choices = available_data(), multiple = FALSE)
    })


    # tisefka <- reactive({
    #   ggplot2::diamonds
    # })
    data_diag <- reactive({
      req(tisefka())
      req(input$load_ml_data)
      SaldaeDataExplorer::data_diagnosis_f(tisefka())
    })

    categoricals_unique_values <- reactive({
      req(input$load_ml_data)
      req(data_diag())

      data_diag()$categoricals_unique_values
    })

    non_numeric_variables <- reactive({
      req(categoricals_unique_values())
      req(input$load_ml_data)
      var_granularity <- data_diag()$diagnosis%>%
        dplyr::filter(!types %in% c("numeric", "integer","Date","POSIXct"))%>%
        dplyr::pull(variables)
      return(var_granularity)
    })

    output$aggregator_board_box <- renderUI({
      fluidPage(
        fluidRow(
          column(width = 3,  uiOutput(ns("selected_bi_data"))),
          column(width = 2, br(), uiOutput(ns("load_ml_data"))),
          column(width = 2, uiOutput(ns("fc_date_var"))),
          column(width = 2, uiOutput(ns("fc_target_var"))),
          column(width = 2, uiOutput(ns("var_granularity"))),
          column(width = 1, uiOutput(ns("aggregation_metric"))),
          column(width = 2, uiOutput(ns("fc_models_list"))),
          column(width = 2, uiOutput(ns("fc_future_forecast"))),
          column(width = 2, uiOutput(ns("fc_split_train_test"))),
          column(width = 2,br(), uiOutput(ns("fc_chk_business_days"))),
          column(width = 1, br(), uiOutput(ns("submit")))
        ),
        uiOutput(ns("non_numeric_variables_inputs"))
      )
    })


    output$submit <- renderUI({
      req(input$fc_date_var)
      req(input$fc_target_var)
      req(input$load_ml_data)
      req(input$fc_models_list)
      bs4Dash::actionButton(inputId = ns("submit"), label = ("Start"), icon = icon("play"), status = "primary")
    })

    observeEvent(eventExpr = non_numeric_variables(), handlerExpr = {
      non_numeric_variables() %>% purrr::imap(~ {
        output_name_app <- paste0("non_numeric_variables_", .x)
        output[[output_name_app]] <- renderUI({
          ml_choices <- tisefka()$var_factors[[.x]]
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
        choices = c("ARIMA" = "arima",
                    "Prophet" = "prophet",
                    "Mars" = "mars",
                    "Linear regression" = "lr",
                    "Random Forest" = "random_forest",
                    "XGBoost" = "xgboost",
                    "ARIMAX" = "arimax"),
        multiple = TRUE
      )
    })

    output$fc_date_var <- renderUI({
      req(input$load_ml_data)
      req(data_diag())
      fc_date_var <- data_diag()$diagnosis%>%
        dplyr::filter(types %in% c("Date","POSIXct","POSIXt"))%>%
        dplyr::pull(variables)

      shinyWidgets::pickerInput(
        inputId = ns("fc_date_var"),
        label = ("X Axis"),
        multiple = FALSE,
        choices = fc_date_var,
        selected = NULL
      )

    })

    output$fc_target_var <- renderUI({
      req(input$load_ml_data)
      req(input$fc_date_var)
      req(data_diag())
      num_vars <- data_diag()$diagnosis%>%
        dplyr::filter(types %in% c("numeric", "integer"))%>%
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
      var_granularity <- data_diag()$diagnosis%>%
        dplyr::filter(!types %in% c("numeric", "integer","Date","POSIXct"))%>%
        dplyr::pull(variables)
      var_granularity <- var_granularity[var_granularity != input$fc_date_var]
      if(length(var_granularity)==0)return(NULL)
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
      aggregation_choices <- c("Sum","Average", "Median", "Count")
      names(aggregation_choices) <- aggregation_choices
      shinyWidgets::pickerInput(
        inputId = ns("aggregation_metric"),
        label = ("Aggregation"),
        multiple = FALSE,
        selected = aggregation_choices[1],
        choices = aggregation_choices
      )
    })

    tisefka_iheggan <- reactive({
      req(tisefka())
      req(input$fc_target_var)
      req(input$fc_date_var)
      req(input$submit)
      tisefka_iheggan <- tisefka()
      if (length(non_numeric_variables()) > 0) {
        categ_input_filter <- non_numeric_variables() %>%
          purrr::map(~ input[[paste0("non_numeric_variables_", .x)]]) %>%
          stats::setNames(non_numeric_variables())
        categ_input_filter <- categ_input_filter[!unlist(lapply(categ_input_filter, is.null))]
        for (cat_input in names(categ_input_filter)) {
          if ("NA" %in% categ_input_filter[[cat_input]]) categ_input_filter[[cat_input]] <- c(categ_input_filter[[cat_input]], NA)
          tisefka_iheggan <- tisefka_iheggan %>% dplyr::filter(!!rlang::sym(cat_input) %in% categ_input_filter[[cat_input]])
        }
      }

      if(!is.null(input$var_granularity)){
        grouping_elements <- c( input$fc_date_var,input$var_granularity) %>%
          unique()
      }else{
        grouping_elements <- input$fc_date_var
      }
      tisefka_iheggan <- tisefka_iheggan %>%
        dplyr::select(!!c(grouping_elements, input$fc_target_var))%>%
        dplyr::group_by(dplyr::across(!!grouping_elements)) %>%
        dplyr::summarise_all(SaldaeModulesUI:::th2_agg_func, "sum")

      tisefka_iheggan <- tisefka_iheggan%>%
        janitor::clean_names()
      tisefka_iheggan2 <<- tisefka_iheggan
      return(tisefka_iheggan)
    })

    #---------------------------------------
    # output$time_unit_data <- renderUI({
    #   req(ts_time_units())
    #   shinyWidgets::radioGroupButtons(
    #     inputId = ns("time_unit_data"),
    #     label = ("Aggregate by"),
    #     choices = ts_time_units(),
    #     status = "primary",
    #     justified = FALSE,
    #     checkIcon = list(
    #       yes = shiny::icon("ok",
    #                         lib = "glyphicon"
    #       )
    #     )
    #   )
    # })
    #----------------
    output$fc_split_train_test <- renderUI({
      req(input$fc_date_var)
      req(input$fc_target_var)
      dateInput(inputId = ns("fc_split_train_test"),label = "Split", value = Sys.Date())
    })

    tisefka_aggregated_all <- eventReactive(input$submit,{
      req(input$fc_date_var)
      req(input$fc_target_var)
      req(tisefka_iheggan())
      calendar_country =  ""
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
        target_var = input$fc_target_var,
        date_var = input$fc_date_var,
        future_forecast = 120,
        models_list = input$fc_models_list,
        business_days = input$fc_chk_business_days,
        split_train_test = input$fc_split_train_test,
        calendar_country = calendar_country,
        calendar_column = calendar_column,
        use_spark = input$fc_chk_spark,
        group_target = input$var_granularity
      )

      fc_result <- forecast_transform_data(
        fc_data = tisefka_iheggan(),
        fc_meta_data = fc_meta_data
      )

      if(!is.null(input$var_granularity)){
        fc_result <- fc_result%>%
          dplyr::rename(all_columns = !!input$var_granularity)
      }
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
      fc_target_var <- unique(tisefka_aggregated()$all_columns)
      fc_target_var <- fc_target_var[!fc_target_var %in% c(input$fc_date_var,input$var_granularity)]
      names(fc_target_var) <- fc_target_var
      fc_target_var <- head(fc_target_var, 15)
      return(fc_target_var)
    })

    #---------------------
    output$graphs_ui <- renderUI({
      req(fc_target_var())
      req(tisefka_aggregated_all())

      if(!is.null(input$var_granularity)){
        hist_data <- tisefka_iheggan()%>%
          tidyr::pivot_wider(id_cols = input$fc_date_var,
                             names_from = input$var_granularity,
                             values_from = input$fc_target_var,
                             values_fill  = 0)
      }else{
        hist_data <- tisefka_iheggan()
      }
      plots_list <- purrr::map(fc_target_var(), ~ {
        mod_id <- th2product::generateID("fcast_view")
        fcast_inputs <-  list(target_var = .x, date_var = input$fc_date_var,
                            historical_data_aggregated = hist_data%>%
                              dplyr::select(!!input$fc_date_var, !!.x),
                            prediction_data_aggregated = tisefka_aggregated()%>%
                              dplyr::filter(all_columns == !!.x))
        mod_basic_fcast_viewer_server(mod_id, fcast_inputs = fcast_inputs)
        column(width = 4,
               mod_basic_fcast_viewer_ui(ns(mod_id)))
      })
      fluidRow(plots_list)
    })
  })
}
