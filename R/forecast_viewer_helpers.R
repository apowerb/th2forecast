
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
  query_statement_output <- glue::glue("SELECT distinct(family), _model_desc, _index, _value, _conf_lo, _conf_hi FROM {selected_table_output}")
  query_res_output <- DBI::dbSendQuery(db_conn, statement = query_statement_output)
  prediction_data <- DBI::dbFetch(query_res_output)


  # Convertir les colonnes de date en objets de date
  historical_data$`_date` <- as.Date(historical_data$`_date`)
  prediction_data$`_index` <- as.Date(prediction_data$`_index`)

   # Fusionner les deux jeux de données en fonction de la date
  merged_data <- base::merge(historical_data, prediction_data, by.x = c("family", "_date"), by.y = c("family", "_index"), all = TRUE)

  merged_data <- merged_data %>%
    tidyr::unite("value", c("sales", "_value"), sep = ",", na.rm = TRUE)

  DBI::dbDisconnect(db_conn)

  return(merged_data)
}
#==============================================

#merged data filtred function

merged_data_filtred <- function(merged_data = NULL, kpi_value = NULL , model = NULL) {

  if (!is.null(kpi_value)) {
    merged_data_filtred  <- merged_data %>% dplyr::filter(family == kpi_value, model == model)
  }

  return(merged_data_filtred)
}

#==============================================
 #create plot (time series) function
create_time_series_plot <- function(data = NULL) {
  data$value <- as.numeric(data$value)
  time_series_plot <- plotly::plot_ly(data,type = 'scatter', mode = 'lines') %>%
    plotly::add_trace(x = ~`_date`, y =  ~value, name = 'Value')%>%
    # plotly::add_trace(y = ~value, name = 'Value') %>%
    plotly::add_trace(x = ~`_date`, y = data[['_conf_lo']], name = "Confidence Lower Bound") %>%
    plotly::add_trace(x = ~`_date`, y = data[['_conf_hi']], name = "Confidence Upper Bound") %>%
    plotly::layout(title = "Time Series with Confidence Interval",
           xaxis = list(title = "Date"),
           yaxis = list(title = "Value"),
           showlegend = TRUE)

  return(time_series_plot)
}

