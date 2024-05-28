#' @title mod_boxes
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export
#'
#' @importFrom shiny NS tagList
mod_fc_boxes_ui <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("box_ui")))
}

#' mod_boxes Server Functions
#' @export
mod_fc_boxes_server <-
  function(id,
           box_title = "Workflows",
           perm_table = "th2_wf_permissions",
           box_uuid = "workflows",
           box_color = "info",
           box_bg_color = "white",
           box_icon = "timeline",
           box_body = "fhc",
           data = NULL,
           selected_info = NULL,
           output_data_result = NULL,
           index = 0,
           parent_session = NULL
           ) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      output$box_ui <- renderUI({
        bx_dropdown_menu <- bs4Dash::boxDropdown(
          bs4Dash::boxDropdownItem(
            "Share",
            id = ns("box_share"),
            icon = icon("share-alt")
          ),
          bs4Dash::boxDropdownItem("Refresh", icon = icon("sync")),
          bs4Dash::dropdownDivider(),
          bs4Dash::boxDropdownItem(
            "Open",
            id = ns("box_open"),
            icon = icon("eye")
          ),
          bs4Dash::dropdownDivider(),
          bs4Dash::boxDropdownItem(
            "Delete",
            id = ns("box_delete"),
            icon = icon("trash"),
          ),
          bs4Dash::boxDropdownItem(
            "Permissions",
            id = ns("edit_permissions"),
            icon = icon("user-lock"),
          )
        )



        bs4Dash::box(
          title = box_title,
          status = box_color,
          background = box_bg_color,
          width = 12,
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          dropdownMenu = bx_dropdown_menu,
          uiOutput(ns("box_content"))
        )
      })
      output$box_content <- renderUI({
        box_body
      })

      observeEvent(input$box_share, {
        module_id <- th2product::generateID(prefix = box_title)
        th2utils::mod_th2_email_notif_server(id = module_id)
        showModal(
          modalDialog(
            title = glue::glue("Send {box_title}"),
            size = "m",
            icon = icon("paper-plane"),
            easyClose = TRUE,
            th2utils::mod_th2_email_notif_ui(ns(module_id))
          )
        )
      })

      observeEvent(input$box_open, {
        selected_info(data[index, ])
        output_connection <- th2product::decrypt_column(selected_info()['output_meta_connection'])

        decrypted_output_connection <- jsonlite::fromJSON(output_connection)
        db_conn <- db_conn_function(dbms = "postgresql",
                                    user = decrypted_output_connection$username,
                                    password = decrypted_output_connection$password,
                                    port = decrypted_output_connection$port ,
                                    host = decrypted_output_connection$host,
                                    db_name = decrypted_output_connection$database)

        output_data_result(output_data_fetch(db_conn = db_conn,
                                             target_table = decrypted_output_connection$target_table,
                                             schema = decrypted_output_connection$schema,
                                             target_var = selected_info()$target_var,
                                             group_target_var = selected_info()$group_target_var,
                                             date_var = selected_info()$date_var))

        updateTabsetPanel(session = parent_session, "forecastViz_tabbox", selected = "Forecasting Viewer")
      })

      observeEvent(input$edit_permissions, {
        print("edit_permissions")
        module_id <- th2product::generateID(prefix = "permission_")

        th2blender::mod_permission_data_server(module_id,
          target_table = perm_table,
          object_creator = Sys.getenv("SHINYPROXY_USERNAME"),
          data_name = box_uuid, object_type = "fc",
          refresh_file = mod_refresh_file
        )
        th2blender::mod_permission_data_ui(ns(module_id), data_name = box_uuid)
      })
    })
  }
