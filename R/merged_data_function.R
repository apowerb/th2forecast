
merged_data <- function (selected_table_input = NULL, selected_table_output= NULL , db_conn = NULL, available_tables = NULL ) {

  # Vérifier si les tables sélectionnées existent dans la base de données
  if (!selected_table_input %in% available_tables || !selected_table_output %in% available_tables) {
    DBI::dbDisconnect(db_conn)
    stop("Certaine table sélectionnée n'existe pas dans la base de données.")
  }

  # Récupérer les données historiques
  query_statement_input <- glue::glue("SELECT _date, sales FROM {selected_table_input}")
  query_res_input <- DBI::dbSendQuery(db_conn, statement = query_statement_input)
  historical_data <- DBI::dbFetch(query_res_input)

  # Récupérer les données de prédiction
  query_statement_output <- glue::glue("SELECT _index, _value, _conf_lo, _conf_hi FROM {selected_table_output}")
  query_res_output <- DBI::dbSendQuery(db_conn, statement = query_statement_output)
  prediction_data <- DBI::dbFetch(query_res_output)


  # Convertir les colonnes de date en objets de date
  historical_data$`_date` <- as.Date(historical_data$`_date`)
  prediction_data$`_index` <- as.Date(prediction_data$`_index`)

   # Fusionner les deux jeux de données en fonction de la date
  merged_data <- base::merge(historical_data, prediction_data, by.x = "_date", by.y = "_index", all = TRUE)

  merged_data <- merged_data %>%
    unite("value", c("sales", "_value"), sep = ",", na.rm = TRUE)

  DBI::dbDisconnect(db_conn)

  return(merged_data)
}

