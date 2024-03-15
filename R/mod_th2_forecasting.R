#' mod_th2_forecasting_ui
#' @export


mod_th2_forecasting_ui <- function(id) {
  ns <- NS(id)


  fluidPage(
    fluidRow(
      tagList(
        fileInput(ns("dataset"), "Upload your data file", accept = c(".csv")),
        actionButton(inputId = ns("clean_data") , label = "Clean Data" , value = NULL),
        actionButton(inputId = ns("feature_engi") , label = "Feature engineering" , value = NULL),
        actionButton(inputId = ns("train_model") , label = "Training" , value = NULL),
        actionButton(inputId = ns("forecasting") , label = "Forecasting" , value = NULL),
        )
      ),
    mainPanel(
      DT::dataTableOutput(ns("table"))
    )

  )

}


#' mod_th2_forecasting_server
#' @export

mod_th2_forecasting_server<- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    input_csv  <- reactive({
      if (is.null(input$dataset)) {
        return("")
      }
      # actually read the file
      read.csv(file = input$dataset$datapath)
    })

############################################################################################################

    #Data Clean
    observeEvent(input$clean_data, {
      data <- input_csv()
      data <- preprocessing_data(data)$dataset_clean
      data <- feature_selection(data)

      output$table <- DT::renderDataTable({
        data
      })

    })
############################################################################################################

    #feauture engineering
    observeEvent(input$feature_engi, {
      showModal(modalDialog(
        title = "Feature Engineering", easyClose = TRUE,
        fluidRow(
          column(width = 5, offset = 1, selectInput(inputId = ns("target"), choices = c("temp","seasson","casual"), label = "Target")),
          uiOutput(ns("split"))
          )))
    })
    output$split <- renderUI({
      req(input$target)
      column(width = 5, offset = 1, actionButton(inputId = ns("split_data"), style = "btn-primary", label = "Split Data"))
    })

    observeEvent(input$split_data,{
      showModal(modalDialog(
        title = "Split Data", easyClose = TRUE,
        fluidRow(
          column(width = 5, offset = 1, textInput(inputId = ns("assess"), label = "Assess", value = NULL)),
          uiOutput(ns("split_data_btn"))
        )
      ))
    })

    output$split_data_btn <- renderUI({
      req(input$assess)
      column(width = 5, offset = 1, actionButton(inputId = ns("split"), style = "btn-primary", label = "Split Data"))
    })
    observeEvent(input$split,{
      #ici tu mets la fct de split + plot
    })

##################################################################################################################

    #Training Model
    observeEvent(input$train_model,{
      showModal(modalDialog(
        title = "Training Model", easyClose = TRUE,
        fluidRow(
          column(width = 5, offset = 1, selectInput(inputId = ns("model"), choices = c("ls","mars","prophet"),label = "Choice Model")),
          uiOutput(ns("training_model")),
          uiOutput(ns("evaluate_model"))

        )))
    })
    output$training_model <- renderUI({
      req(input$model)
      column(width = 5, offset = 1, actionButton(inputId = ns("train"), style = "btn-primary", label = "Training Model"))
  })
    observeEvent(input$train,{
      #ici tu mets la fct de training + alert success
    })

    output$evaluate_model <- renderUI({
    req(inpu$model)
    # req(input$train)
    column(width = 5, offset = 1, actionButton(inputId = ns("evaluate"), style = "btn-primary", label = "Evaluate Model"))
    })

    observeEvent(input$evaluate,{
      #ici mets la fct de evaluation de model + tableau des résultats
    })
##############################################################################################################

    #forecasting
    observeEvent(input$forecasting,{
      showModal(modalDialog(
        title = "Forecasting", easyClose = TRUE,
        fluidRow(
          column(width = 5, offset = 1, numericInput(inputId ="period", label = "Number of periods to predict", value = 3)),
          column(width = 5, offset = 1,selectInput(inputId ="unit", label = "Time unit", choices = c("days", "months", "year"))),
          column(width = 6, offset = 2, actionButton(inputId = ns("prediction"), style = "btn-primary", label = "Prediction"))
        )
      ))
   })

    observeEvent(input$prediction, {

      #Ici mets résultats de prédiction + plot
    })




  }
  )}
