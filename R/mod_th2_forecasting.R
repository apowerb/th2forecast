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
      ))
}


#' mod_th2_forecasting_server
#' @export

mod_th2_forecasting_server<- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    dataset <- reactive({
      req(input$dataset)
      read.csv(input$dataset$datapath)
    })

    #Data Clean
    observeEvent(input$clean_data, {
      #ici tu met la fct de clean + alert success

    })

    #feauture engineering
    observeEvent(input$feature_engi, {
      showModal(modalDialog(
        title = "Feature Engineering", easyClose = TRUE,
        fluidRow(
          column(width = 5, offset = 1, selectInput(inputId = ns("target"), choices = .... , label = "Target", value = NULL)),
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

    observeEvent(input$split_data_btn,{
      #ici tu met la fct de split + plot
    })


    #Training Model
    observeEvent(input$train_model,{
      showModal(modalDialog(
        title = "Training Model", easyClose = TRUE,
        fluidRow(
          column(width = 5, offset = 1, selectInput(inputId = ns("model"), choices = .....,label = "Choice Model", value = NULL)),
          uiOutput(ns("training_model")),
          uiOutput(ns("evaluate_model"))

        )))
    })
    output$training_model <- renderUI({
      req(input$model)
      column(width = 5, offset = 1, actionButton(inputId = ns("train"), style = "btn-primary", label = "Training Model"))
  })
    observeEvent(input$train,{
      #ici tu met la fct de training + alert success
    })


  }
  )}
