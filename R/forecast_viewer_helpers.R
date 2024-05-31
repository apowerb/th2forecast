# ===================== DB cconn function ==================
db_conn_function <- function(dbms = NULL, server = NULL, user = NULL, password = NULL, port = NULL,
                             host = NULL, db_name = NULL) {
  db_conn <- DatabaseConnector::connect(
    dbms = "postgresql",
    server = paste0(host, "/", db_name),
    user = user,
    password = password,
    port = as.numeric(port)
  )
  return(db_conn)
}
# ===================== output data function ( prediction data)==================
output_data_fetch <- function(db_conn = NULL, target_table = NULL, schema = NULL, target_var = NULL, group_target_var = NULL, date_var = NULL) {
  # Récupérer les données de prédiction
  tryCatch({
    query_statement_output <- glue::glue("SELECT DISTINCT {group_target_var},{target_var}, {date_var} , _model_desc, _conf_lo, _conf_hi,as_of, start_date, end_date
                                        FROM {schema}.{target_table}")

    query_res_output <- DBI::dbSendQuery(db_conn, statement = query_statement_output)
    prediction_data <- DBI::dbFetch(query_res_output)

    prediction_data$as_of <- as.Date(prediction_data$as_of)

    DBI::dbDisconnect(db_conn)
    return(prediction_data)
  }, error = function (error) {
    DBI::dbDisconnect(db_conn)
    shinyalert("error when return output data !", type = "error")
    return(NULL)
  })

}

# ======================input data function ( historical data filtred) ==================
input_data_fetch <- function(prediction_data = NULL, db_conn = NULL, target_table = NULL, target_var = NULL, group_target_var = NULL, date_var = NULL, as_of = NULL) {
  output_data_filtred <- prediction_data %>% dplyr::filter(as_of == !!as_of)

  start_date <- as.Date(dplyr::first(output_data_filtred$start_date))
  end_date <- as.Date(dplyr::first(output_data_filtred$end_date))

  query_statement_input <- glue::glue("SELECT DISTINCT {group_target_var},{target_var}, {date_var}
                                       FROM {target_table}")

  query_res_input <- DBI::dbSendQuery(db_conn, statement = query_statement_input)
  historical_data <- DBI::dbFetch(query_res_input)
  historical_data_filtred <- historical_data %>% filter(between(as.Date(historical_data[[date_var]]), start_date, end_date))
  DBI::dbDisconnect(db_conn)
  return(historical_data_filtred)
}


# ======= #merged data filtred function =======================================
prediction_data_filtred <- function(prediction_data = NULL, model = NULL, kpi_value = NULL, group_target_var = NULL) {
  if (!is.null(model)) {
    prediction_data_filtred <- prediction_data %>% dplyr::filter(`_model_desc` == !!model, prediction_data[[group_target_var]] == !!kpi_value)
  }

  return(prediction_data_filtred)
}

historical_data_filtred <- function(historical_data = NULL, kpi_value = NULL, group_target_var = NULL) {
  if (!is.null(kpi_value)) {
    historical_data_filtred <- historical_data %>% dplyr::filter(historical_data[[group_target_var]] == !!kpi_value)
  }

  return(historical_data_filtred)
}


# =========== #create plot (time series) function ==================================
create_time_series_plot <- function(historical_data = NULL, prediction_data = NULL, x_var = NULL, y_var = NULL) {
  historical_data <- historical_data[order(historical_data[[x_var]]), ]
  prediction_data <- prediction_data[order(prediction_data[[x_var]]), ]

  time_series_plot <- plotly::plot_ly() %>%
    plotly::add_trace(
      data = historical_data, type = "scatter", mode = "lines",
      x = ~ get(x_var), y = ~ get(y_var), name = "Historical Values", color = I("blue")
    ) %>%
    plotly::add_trace(
      data = prediction_data, type = "scatter", mode = "lines",
      x = ~ get(x_var), y = ~ get(y_var), name = "Prediction Values", color = I("red")
    ) %>%
    plotly::add_trace(
      data = prediction_data, type = "scatter", mode = "lines",
      x = ~ get(x_var), y = ~`_conf_lo`, name = "Lower Confidence",
      line = list(shape = "spline"), color = "rgba(255,250,250)"
    ) %>%
    plotly::add_trace(
      data = prediction_data, type = "scatter", mode = "lines",
      x = ~ get(x_var), y = ~`_conf_hi`, name = "High Confidence",
      line = list(shape = "spline"), color = "rgba(255,250,250)", fill = "tonexty", fillcolor = "rgba(11,156,49,0.2)"
    ) %>%
    plotly::layout(
      title = "Time Series with Confidence Interval",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Value"),
      showlegend = TRUE
    )

  time_series_plot
}
