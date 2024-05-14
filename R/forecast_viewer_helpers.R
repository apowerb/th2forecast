
#merged data fuction
merged_data <- function (selected_table_input = NULL, selected_table_output= NULL , db_conn = NULL, available_tables = NULL ) {

  # Vérifier si les tables sélectionnées existent dans la base de données
  if (!selected_table_input %in% available_tables || !selected_table_output %in% available_tables) {
    DBI::dbDisconnect(db_conn)
    stop("Certaine table sélectionnée n'existe pas dans la base de données.")
  }

  # Récupérer les données historiques
  query_statement_input <- glue::glue("SELECT distinct(family), _date, sales FROM {selected_table_input}")
  query_res_input <- DBI::dbSendQuery(db_conn, statement = query_statement_input)
  historical_data <- DBI::dbFetch(query_res_input)

  # Récupérer les données de prédiction
  query_statement_output <- glue::glue("SELECT distinct(family), _model_desc, _date, sales, _conf_lo, _conf_hi FROM {selected_table_output}")
  query_res_output <- DBI::dbSendQuery(db_conn, statement = query_statement_output)
  prediction_data <- DBI::dbFetch(query_res_output)

  # Convertir les colonnes de date en objets de date
  historical_data$`_date` <- as.Date(historical_data$`_date`)
  prediction_data$`_date` <- as.Date(prediction_data$`_date`)

 # Fusionner les deux jeux de données en fonction de la date
merged_data <- base::merge(historical_data, prediction_data, by = c("family", "_date"),all = TRUE)

# Coller les colonnes de sales de historical and prediction data  dans une seule colonne
merged_data$sales <- dplyr::coalesce(merged_data$sales.x, merged_data$sales.y)

# Supprimer les colonnes sales.x et sales.y
merged_data <- merged_data[, !grepl("\\.x$|\\.y$", names(merged_data))]

  DBI::dbDisconnect(db_conn)
  return(merged_data)
}

#======= #merged data filtred function =======================================
merged_data_filtred <- function(merged_data = NULL, kpi_value = NULL , model = NULL) {

  if (!is.null(kpi_value)) {
    merged_data_filtred  <- merged_data %>% dplyr::filter(family == kpi_value, model == model)
  }

  return(merged_data_filtred)
}


#=========== #create plot (time series) function ==================================
create_time_series_plot <- function(data = NULL) {
   data$color <- ifelse(is.na(data$`_conf_lo`) | is.na(data$`_conf_hi`), "red","blue")

  time_series_plot <- plotly::plot_ly(data, type = 'scatter', mode = 'lines') %>%
    plotly::add_trace(x = ~`_date`, y = ~sales, name = 'Value', color = ~ color) %>%
    plotly::add_trace(x = ~`_date`, y = ~`_conf_lo`, name = "Lower Confidence", fill = 'tonexty', fillcolor = 'rgba(11,156,49,0.2)') %>%
    plotly::add_trace(x = ~`_date`, y = ~`_conf_hi`, name = "High Confidence", fill = 'tonexty', fillcolor = 'rgba(11,156,49,0.2)') %>%
    plotly::layout(title = "Time Series with Confidence Interval",
           xaxis = list(title = "Date"),
           yaxis = list(title = "Value"),
           showlegend = TRUE)

  time_series_plot
}
