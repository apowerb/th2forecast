#===================== output data function ( prediction data)==================
output_data_function <- function (selected_table_output= NULL , db_conn = NULL, available_tables = NULL ) {

  # Vérifier si la table sélectionnée existe dans la base de données
  if (!selected_table_output %in% available_tables) {
    DBI::dbDisconnect(db_conn)
    stop("La table sélectionnée n'existe pas dans la base de données.")
  }

  # Récupérer les données de prédiction
  query_statement_output <- glue::glue("SELECT distinct(family) , _model_desc, _date, sales, _conf_lo, _conf_hi,as_of, date_start, date_end FROM {selected_table_output}")
  query_res_output <- DBI::dbSendQuery(db_conn, statement = query_statement_output)
  prediction_data <- DBI::dbFetch(query_res_output)

  prediction_data$`_date` <- as.Date(prediction_data$`_date`)
  prediction_data$as_of <- as.Date(prediction_data$as_of)

   DBI::dbDisconnect(db_conn)
  return(prediction_data)
}

#======================input data function ( historical data filtred) ==================
input_data_function <- function (prediction_data = NULL, selected_table_input= NULL , db_conn = NULL, available_tables = NULL , as_of = NULL) {

  output_data_filtred  <- prediction_data %>% dplyr::filter(as_of == as_of)

  date_start <- dplyr::first(output_data_filtred$date_start)
  date_end <- dplyr::first(output_data_filtred$date_end)

  # Vérifier si la table sélectionnée existe dans la base de données
  if (!selected_table_input %in% available_tables) {
    DBI::dbDisconnect(db_conn)
    stop("La table sélectionnée n'existe pas dans la base de données.")
  }

  query_statement_input <- glue::glue("SELECT distinct(family), _date, sales FROM {selected_table_input}")
  query_res_input <- DBI::dbSendQuery(db_conn, statement = query_statement_input)
  historical_data <- DBI::dbFetch(query_res_input)

  historical_data$`_date` <- as.Date(historical_data$`_date`)

  historical_data_filtred <- historical_data %>% dplyr::filter(`_date` >= date_start & `_date` <= date_end)
  DBI::dbDisconnect(db_conn)
  return(historical_data_filtred)
}

#======================== merged data fuction ===================================
# merged_data_function <- function ( historical_data = NULL, prediction_data= NULL, db_conn = NULL) {
#
#  # Fusionner les 2 data (input filtred and output)
# merged_data <- base::merge(historical_data, prediction_data, by = c("family", "_date"),all = TRUE)
#
# # Coller les colonnes de sales de historical data filtred and prediction data  dans une seule colonne
# merged_data$sales <- dplyr::coalesce(merged_data$sales.x, merged_data$sales.y)
#
# # Supprimer les colonnes sales.x et sales.y
# merged_data <- merged_data[, !grepl("\\.x$|\\.y$", names(merged_data))]
#
#   DBI::dbDisconnect(db_conn)
#   return(merged_data)
# }

#======= #merged data filtred function =======================================
prediction_data_filtred <- function(prediction_data = NULL, model = NULL, kpi_value = NULL) {

  if (!is.null(model)) {
    prediction_data_filtred  <- prediction_data %>% dplyr::filter(`_model_desc` == !!model ,family == !!kpi_value )
  }

  return(prediction_data_filtred)
}

historical_data_filtred <- function(historical_data = NULL, kpi_value = NULL) {

  if (!is.null(kpi_value)) {
    historical_data_filtred  <- historical_data %>% dplyr::filter(family == !!kpi_value)
  }

  return(historical_data_filtred)
}


#=========== #create plot (time series) function ==================================
# create_time_series_plot <- function(historical_data = NULL , prediction_data = NULL) {
#   # Fusionner les 2 data (input filtred and output)
#   merged_data <- base::merge(historical_data, prediction_data, by = c("family", "_date"),all = TRUE)
#
#   # Coller les colonnes de sales de historical data filtred and prediction data  dans une seule colonne
#   merged_data$sales <- dplyr::coalesce(merged_data$sales.x, merged_data$sales.y)
#   # Supprimer les colonnes sales.x et sales.y
#   merged_data <- merged_data[, !grepl("\\.x$|\\.y$", names(merged_data))]
#   View(merged_data)
#
#   merged_data$color <- ifelse(is.na(merged_data$`_conf_lo`) | is.na(merged_data$`_conf_hi`), I("green"), I("red"))
#
#   time_series_plot <- plotly::plot_ly(data = merged_data, type = 'scatter', mode = 'lines') %>%
#
#     plotly::add_trace(x = ~`_date`, y = ~sales, name = 'Values', color = ~color) %>%
#     plotly::add_trace(x = ~`_date`, y = ~`_conf_lo`, name = "Lower Confidence",
#                       line = list(shape = "spline"),color = 'rgba(255,250,250)', fill = 'tozeroy', fillcolor = 'rgba(11,156,49,0.2)') %>%
#
#     plotly::add_trace(x = ~`_date`, y = ~`_conf_hi`, name = "High Confidence",
#                       line = list(shape = "spline"),color = 'rgba(255,250,250)', fill = 'tozeroy', fillcolor = 'rgba(11,156,49,0.2)') %>%
#
#     plotly::layout(title = "Time Series with Confidence Interval",
#                    xaxis = list(title = "Date"),
#                    yaxis = list(title = "Value"),
#                    showlegend = TRUE)
#
#   time_series_plot
# }

create_time_series_plot <- function(historical_data = NULL , prediction_data = NULL) {


  historical_data <- historical_data[order(historical_data$`_date`), ]
  prediction_data <- prediction_data[order(prediction_data$`_date`), ]

  time_series_plot <- plotly::plot_ly() %>%
    plotly::add_trace(data = historical_data, type = 'scatter', mode = 'lines',
                      x = ~`_date`, y = ~sales, name = 'Historical Values', color = I("blue")) %>%

    plotly::add_trace(data = prediction_data, type = 'scatter', mode = 'lines',
                      x = ~`_date`, y = ~sales, name = 'Prediction Values', color = I("red")) %>%

    plotly::add_trace(data = prediction_data, type = 'scatter', mode = 'lines',
                      x = ~`_date`, y = ~`_conf_lo`, name = "Lower Confidence",
                      line = list(shape = "spline"),color = 'rgba(255,250,250)', fill = 'tozeroy', fillcolor = 'rgba(11,156,49,0.2)') %>%

    plotly::add_trace(data = prediction_data, type = 'scatter', mode = 'lines',
                      x = ~`_date`, y = ~`_conf_hi`, name = "High Confidence",
                      line = list(shape = "spline"),color = 'rgba(255,250,250)', fill = 'tozeroy', fillcolor = 'rgba(11,156,49,0.2)') %>%

    plotly::layout(title = "Time Series with Confidence Interval",
                   xaxis = list(title = "Date"),
                   yaxis = list(title = "Value"),
                   showlegend = TRUE)

  time_series_plot


}
