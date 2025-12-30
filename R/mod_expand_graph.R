mod_expand_graph_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("expand_graph"))
}

mod_expand_graph_server <- function(id, interactive_graph = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$expand_graph <- renderUI({
      actionButton(inputId = ns("expand_graph"), label = "", icon = icon("binoculars"))
    })

    output$expanded_graph <- renderUI({
      interactive_graph()
    })
    observeEvent(input$expand_graph, {
      showModal(
        modalDialog(
          title = "View", size = "xl",
          uiOutput(ns("expanded_graph"))
        )
      )
    })
  })
}
