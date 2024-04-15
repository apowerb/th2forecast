#' mod_th2_forecasting_ui
#' @export


mod_th2_forecasting_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(

      # Inputs
      column(width = 2,  fileInput(ns("dataset"), "Upload your data file:", accept = c(".csv"))),

      column(width = 2, uiOutput(ns("feature_target"))),

      column(width = 2, selectInput(ns("models"), "Select Model", choices = c("ARIMA" = "arima", "Prophet" = "prophet", "Mars" = "mars", "Linear regresion" = "lr", "Random Forest" = "random_forest", "XGBoost" = "xgboost") , multiple = TRUE)),

      column(width = 2, uiOutput(ns("conditional_features"))),

      column(width = 2, uiOutput(ns("cond_start_date"))),

      column(width = 2, uiOutput(ns("cond_end_date"))),

      column(width = 2, uiOutput(ns("cond_forecast_predic"))),

      # Buttons
      column(width = 2, actionButton(inputId = ns("clean_data") , label = "Clean Data" , class = "btn-primary")),
      # actionButton(inputId = ns("feature_engi") , label = "Feature engineering" , value = NULL),
      # actionButton(inputId = ns("train_model") , label = "Training" , value = NULL),
      column(width = 2, actionButton(inputId = ns("forecasting") , label = "Forecasting" , class = "btn-primary")),

    ),

    # Display interface
    fluidRow(
      column(width = 12, uiOutput(ns("plot_forecast")))
    )
  )

}


#' mod_th2_forecasting_server
#' @export

mod_th2_forecasting_server<- function(id) {

  models_trained <- reactiveVal(NULL)
  start_date <- reactiveVal(NULL)
  end_date <- reactiveVal(NULL)
  var_target <- reactiveVal(NULL)

  data_input <- reactiveVal(NULL)
  data_clean <- reactiveVal(NULL)
  data_train <- reactiveVal(NULL)
  data_feature_train <- reactiveVal(NULL)

  var_date_feature <- reactiveVal(NULL)
  df_prediction_test_forecas <- reactiveVal(NULL)
  first_horizon <- reactiveVal(NULL)
  # models_predictions <- reactiveVal(NULL)



  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Function to recover csv file
    input_csv  <- reactive({
      if (is.null(input$dataset)) {
        shinyalert("Warning", "You must select a data set.", type = "info" )
        return("")
      }
      readr::read_csv(file = input$dataset$datapath)
    })

    # Generation of inputs when loading datasets
    observeEvent(input$dataset, {

      # Read dataset
      data_input(input_csv())

      # Date feature
      column_date <- sapply(data_input(), function(x) inherits(x, "Date") || inherits(x, "POSIXct"))

      # Target feature
      var_date_feature(colnames(data_input()[, column_date]))
      list_features <- as.list(colnames(dplyr::select(data_input(), -var_date_feature())))

      # Minimum and maximum tadaset date
      min_date <- sort(data_input()[[var_date_feature()]])[1]
      max_date <- sort(data_input()[[var_date_feature()]], decreasing = TRUE)[1]

      # Horizon forecast calculation according to dataset size
      min_horizon <- round(nrow(data_input()) * 0.02)
      value_horizon <- min_horizon + 30
      max_horizon <- min_horizon + round(nrow(data_input()) * 0.25)

      first_horizon(round((min_horizon + round(nrow(data_input()) * 0.25)) / 2))

      # Update of inputs
      output$feature_target <- renderUI({
        selectInput(ns("features"), "Target variable", choices = list_features)
      })

      output$cond_start_date <- renderUI({
        dateInput(ns("start_date"), "Start", value = min_date, min = min_date, max = max_date)
      })

      output$cond_end_date <- renderUI({
        dateInput(ns("end_date"), "End", value = max_date, min = min_date, max = max_date)
      })

      output$cond_forecast_predic <- renderUI({
        sliderInput(ns("forecast_predic"), "Forecast horizon:", min = min_horizon, max = max_horizon, value = value_horizon)

      })

      output$conditional_features <- renderUI({
        if (any(input$models %in% c("xgboost", "random_forest")) && length(input$models) != 0  ) {
            tagList(
              selectInput(ns("features_variables"), "Features variables:", choices = list_features , multiple = TRUE),
              actionButton(inputId = ns("feature_engineering") , label = "Feature engineering" , value = NULL),
              hr()
            )
        }else if(is.null(input$models))
        {
          ""
        }
      })

      # Clear graphic and variables
      output$plot_forecast <- renderUI({return(NULL)})


    })

############################################################################################################
    #Data Clean
    observeEvent(input$clean_data, {

      # Dataset validation
      if (is.null(input$dataset)) {
        shinyalert("Warning", "You must select a data set.", type = "info" )
      }
      else
      {
        # Read dataset
        data <- input_csv()

        # Cleaning and feature selection
        data_clean(preprocessing_data(data)$dataset_clean)

        # Update of inputs according to clean dataset
        column_date <- sapply(data_clean(), function(x) inherits(x, "Date") || inherits(x, "POSIXct"))
        var_date_feature(colnames(data_clean()[, column_date]))

        min_date <- sort(data_clean()[[var_date_feature()]])[1]
        max_date <- sort(data_clean()[[var_date_feature()]], decreasing = TRUE)[1]

        # Horizon forecast calculation according to dataset size
        min_horizon <- round(nrow(data_clean()) * 0.02)
        value_horizon <- min_horizon + 30
        max_horizon <- min_horizon + round(nrow(data_clean()) * 0.25)

        first_horizon(round((min_horizon + round(nrow(data_clean()) * 0.25)) / 2))


        output$cond_start_date <- renderUI({
          dateInput(ns("start_date"), "Start", value = min_date, min = min_date, max = max_date)
        })

        output$cond_end_date <- renderUI({
          dateInput(ns("end_date"), "End", value = max_date, min = min_date, max = max_date)
        })

        output$cond_forecast_predic <- renderUI({
          sliderInput(ns("forecast_predic"), "Forecast horizon:", min = min_horizon, max = max_horizon, value = value_horizon)
        })


        # Clear graphic and variables
        output$plot_forecast <- renderUI({return(NULL)})

        models_trained(NULL)
        # models_predictions(NULL)
        shinyalert("Processed data!", "The data was cleaned and the variables were selected.", type = "success")
      }

    })

    observeEvent(input$feature_engineering, {

      # Dataset and inputs validation
      if (is.null(input$dataset)) {
        shinyalert("Warning", "You must select a data set.", type = "info" )
      }
      else if (length(input$models) == 0) {
        shinyalert("Warning", "You must select at least one model.", type = "info" )
      }
      else if (length(input$features) == 0)
      {
        shinyalert("Warning", "You must select at least one target.", type = "info" )
      }
      else
      {

        # Input recovery
        feature_target <- input$features
        features_variables <- input$features_variables


        # Validation for new training or to show predictions already made
        # if ( is.null(models_trained) || forecast_horizon > first_horizon ||
        #      (is.null(models_trained) || nrow(models_trained) != length(list_models))
        #      || start_date != input$start_date
        #      || end_date != input$end_date
        #      || var_target != input$features
        # )
        # {

        # Validation of dataset to use
        if (nrow(data_clean()) == 0 || is.null(data_clean())){
          data_features <- data_input()
        }else{
          data_features <- data_clean()
        }

        data_features <- feature_selection(data_features, feature_target, features_variables)

        data_features <- data_features[complete.cases(data_features), ] # %>% dplyr::select(- "date")

        data_feature_train(data_features)

        output$plot_forecast <- renderUI({return(NULL)})

        shinyalert("Processed data!", "Feature generation was correct.", type = "success")
        # }
      }
    })


##############################################################################################################

    #Forecasting
    observeEvent(input$forecasting, {
      # Dataset and inputs validation
      if (is.null(input$dataset)) {
        shinyalert("Warning", "You must select a data set.", type = "info" )
      }
      else if (length(input$models) == 0) {
        shinyalert("Warning", "You must select at least one model.", type = "info" )
      }
      else if (length(input$features) == 0)
      {
        shinyalert("Warning", "You must select at least one target.", type = "info" )
      }
      else
      {
        # Input recovery
        forecast_horizon <- input$forecast_predic
        list_models <- input$models

        if(is.null(data_feature_train()) && (any(list_models %in% c("random_forest", "xgboost")))){
          shinyalert("Warning", "You must do feature engineering.", type = "info" )
        }else{

          # Graphic forecast
          output$plot_forecast <- renderUI({
            tagList(
              plotly::plotlyOutput(ns("plot_result_forecasting"), height = "100%", fill = ),
              h3("Evaluation:"),
              tableOutput(ns("table_performance"))
            )
          })

          # Validation for new training or to show predictions already made
          if ( is.null(models_trained()) || forecast_horizon > first_horizon() ||
               (is.null(models_trained()) || nrow(models_trained()) != length(list_models))
               || start_date() != input$start_date
               || end_date() != input$end_date
               || var_target() != input$features
               )
          {

            # Validation of dataset to use
            if ((nrow(data_clean()) == 0 || is.null(data_clean())) && (nrow(data_feature_train()) == 0 || is.null(data_feature_train()))){
              data_train(data_input())
            }else if (nrow(data_feature_train()) > 0 && !is.null(data_feature_train())) {
              data_train(data_feature_train())
            }else{
              data_train(data_clean())
            }

            # Input recovery
            forecast_horizon <- input$forecast_predic

            # Horizon forecast validation
            if (forecast_horizon > first_horizon() )
            {first_horizon(input$forecast_predic)}

            # Sub dataset according to time period
            start_date(as.Date(input$start_date))
            end_date(as.Date(input$end_date))

            data_train(filter(data_train(), data_train()[[var_date_feature()]] >= start_date()))
            data_train(filter(data_train(), data_train()[[var_date_feature()]] <= end_date()))

            # Input recovery
            var_target(input$features)

            # Generation of training and validation dataset
            # split_data <- split_dataset(data_train, var_date_feature, var_target)
            split_data <- split_dataset(data_train(), var_date_feature(), var_target())
            dataset_train_test <- split_data$traintest

            models_trained(model_selection_train(dataset_train_test, list_models, var_target(), var_date_feature()))

            # Model evaluation
            models_evaluated <- model_evaluation(dataset_train_test, models_trained())
            table_performance <- models_evaluated$accuracy_models
            models_evaluated <- models_evaluated$model_calibrated

            df_models_evaluated <- models_evaluated %>%
              modeltime::modeltime_forecast(
                new_data = testing(dataset_train_test),
                actual_data = data_train()
              )

            # Model prediction
            models_predictions <- prediction_forecast(data_train(), models_evaluated, h=first_horizon())

            if (inherits(data_train()[[var_date_feature()]][1], "Date"))
            {
              limit_date <- sort(data_train()[[var_date_feature()]], decreasing = TRUE)[1] + forecast_horizon
            }else if(inherits(data_train()[[var_date_feature()]][1], "POSIXct"))
            {
              limit_date <- sort(data_train()[[var_date_feature()]], decreasing = TRUE)[1] + hours(forecast_horizon)
            }

            df_prediction_test_forecas(dplyr::bind_rows(df_models_evaluated, models_predictions[ nrow(data_train())+1 : nrow(data_train()), ]))

            plot_prediction_test_forecas <- df_prediction_test_forecas() %>%
              dplyr::filter(.index <= as.Date(limit_date))
            plot_predictions <- plot_prediction_test_forecas %>%
              modeltime::plot_modeltime_forecast(.legend_max_width = 15)

            # Show forecast graphs
            output$plot_result_forecasting <- renderPlotly({
              plotly::ggplotly(plot_predictions)
            })

            # Show forecast performance
            output$table_performance <- renderTable(table_performance)

          }else{

            # Show forecast graph according to the prediction already made
            forecast_horizon <- input$forecast_predic

            if (inherits(data_train()[[var_date_feature()]][1], "Date"))
            {
              limit_date_show <- sort(data_train()[[var_date_feature()]], decreasing = TRUE)[1] + forecast_horizon
            }else if(inherits(data_train()[[var_date_feature()]][1], "POSIXct"))
            {
              limit_date_show <- sort(data_train()[[var_date_feature()]], decreasing = TRUE)[1] + hours(forecast_horizon)
            }

            plot_prediction_test_forecas <- df_prediction_test_forecas() %>%
              dplyr::filter(.index <= as.Date(limit_date_show))
            plot_predictions <- plot_prediction_test_forecas %>%
              modeltime::plot_modeltime_forecast( .legend_max_width = 15)


            # Show forecast graphs
            output$plot_result_forecasting <- renderPlotly({
              plotly::ggplotly(plot_predictions)
            })

          }
        }
      }

    })
  }
  )}
