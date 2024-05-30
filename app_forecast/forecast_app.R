
library(shiny)
library(th2forecast)
library(modeltime)
library(shinyalert)
library(plotly)
library(anomalize)
library(rsample)
library(bs4Dash)
library(shinyWidgets)
library(dplyr)
library(glue)
library(htmltools)
ml_dir <<- "../data_connectors"


options(shiny.launch.browser = .rs.invokeShinyWindowExternal)

ui <- tagList(
  shiny.info::powered_by("THAINK2", link = "https://www.thaink2.com/", position = "bottom right"),
  shinybusy::add_busy_bar(color = "#C4D517"),
  dashboardPage(
    header = bs4Dash::dashboardHeader(title = "Forecasting"),
    sidebar = bs4Dash::dashboardSidebar(
      bs4Dash::sidebarMenu(
        id = "sidebarMenuID",
        bs4Dash::menuItem("Vizualization", tabName = "vizualization", icon = icon("chart-line"),
                            bs4Dash::menuSubItem("Forecasting Viz", tabName = "forecsating_viz", icon = icon("chart-column")),
                            startExpanded = TRUE
        )
      )
    ),
    body = bs4Dash::dashboardBody(
      bs4Dash::tabItems(
        bs4Dash::tabItem(
          tabName = "forecsating_viz",
          mod_forecasting_viewer_ui("forecsating_viz")
        )
      )
    ),
    controlbar = bs4Dash::bs4DashControlbar(
      bs4Dash::controlbarMenu(
        id = "controlMenu",
        bs4Dash::controlbarItem("User Settings", icon = "users-cog",
                                uiOutput("clusterUI"),
                                uiOutput("controlBarUI"))
      )
    )
  )
)

if (file.exists("../R/initializer.R")) th2forecast:::init_envs()
server <- function(input, output) {
  if (file.exists("../R/initializer.R")) th2forecast:::init_envs_cluster(cluster = Sys.getenv("CURRENT_CLUSTER"))
  observe({
    tabName <- input$sidebarMenuID

    if (tabName == "forecsating_viz") {
      mod_forecasting_viewer_server("forecsating_viz")
    }
  })

  output$clusterUI <- renderUI({
    th2blender::mod_cluster_manage_server("cluster")
    fluidPage(
      th2blender::mod_cluster_manage_ui("cluster")
    )
  })

  output$controlBarUI <- renderUI({
    th2product::mod_change_current_user_server("user")
    th2ml::mod_manage_ml_projects_server(id = "manage_ml_projects_1")
    fluidPage(
      th2product::mod_change_current_user_ui("user"),
      th2ml::mod_manage_ml_projects_ui("manage_ml_projects_1")
    )
  })
}

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE, java.parameters = "-Xss3m"))

