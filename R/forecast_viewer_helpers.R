# ===================== DB cconn function ==================
#' @export
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
#' @export
output_data_fetch <- function(db_conn = NULL, target_table = NULL, schema = NULL, target_var = NULL, group_target_var = NULL, date_var = NULL) {
  # Récupérer les données de prédiction
  tryCatch(
    {
      query_statement_output <- glue::glue("SELECT DISTINCT {group_target_var},{target_var}, {date_var} , _model_desc, _conf_lo, _conf_hi,execution_date, start_date, end_date
                                        FROM {schema}.{target_table}")

      query_res_output <- DBI::dbSendQuery(db_conn, statement = query_statement_output)
      prediction_data <- DBI::dbFetch(query_res_output)

      prediction_data$execution_date <- as.POSIXct(prediction_data$execution_date)

      DBI::dbDisconnect(db_conn)

      return(prediction_data)
    },
    error = function(error) {
      print(error)
      DBI::dbDisconnect(db_conn)
      shinyalert::shinyalert("Error returning output data. Please check the output datasource configuration.", type = "error")
      return(NULL)
    }
  )
}

# ======================input data function ( historical data filtred) ==================
#' @export
input_data_fetch <- function(prediction_data = NULL, db_conn = NULL, target_table = NULL, target_var = NULL, group_target_var = NULL, date_var = NULL, as_of = NULL) {
  tryCatch(
    {
      output_data_filtred <- prediction_data %>% dplyr::filter(as_of == !!as_of)

      start_date <- as.Date(dplyr::first(output_data_filtred$start_date))
      end_date <- as.Date(dplyr::first(output_data_filtred$end_date))

      if(group_target_var == "all_columns"){

        query_statement_input <- glue::glue("SELECT DISTINCT * FROM {target_table}")

        query_res_input <- DBI::dbSendQuery(db_conn, statement = query_statement_input)
        historical_data <- DBI::dbFetch(query_res_input)

        historical_data <- historical_data %>%
          tidyr::pivot_longer(!date_var , names_to = group_target_var, values_to = target_var)

      }else{
        query_statement_input <- glue::glue("SELECT DISTINCT {group_target_var},{target_var}, {date_var}
                                       FROM {target_table}")

        query_res_input <- DBI::dbSendQuery(db_conn, statement = query_statement_input)
        historical_data <- DBI::dbFetch(query_res_input)
      }

      historical_data_filtred <- historical_data %>% filter(between(as.Date(historical_data[[date_var]]), start_date, end_date))
      DBI::dbDisconnect(db_conn)
      return(historical_data_filtred)
    },
    error = function(error) {
      DBI::dbDisconnect(db_conn)

      shinyalert::shinyalert("Error returning input data. Please check the input datasource configuration.", type = "error")
      return(NULL)
    }
  )
}


# ======= #merged data filtred function =======================================
#' @export
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
#' @export
create_time_series_plot <- function(historical_data = NULL, prediction_data = NULL, x_var = NULL, y_var = NULL) {
  historical_data <- historical_data[order(historical_data[[x_var]]), ]
  prediction_data <- prediction_data[order(prediction_data[[x_var]]), ]

  historical_data[[x_var]] <- as.Date(historical_data[[x_var]])
  prediction_data[[x_var]] <- as.Date(prediction_data[[x_var]])

  combined_data <- dplyr::full_join(historical_data, prediction_data, by = x_var, suffix = c(".hist", ".pred"))

  time_series_plot <- combined_data %>%
    echarts4r::e_charts_(x_var) %>%
    echarts4r::e_line_(paste0(y_var, ".hist"), name = "Historical Values", color = "blue") %>%
    echarts4r::e_line_(paste0(y_var, ".pred"), name = "Prediction Values", color = "orange") %>%
    echarts4r::e_line_('_conf_lo', name = "Lower Confidence", lineStyle = list(type = "dashed"), color = "green")%>%
    echarts4r::e_line_('_conf_hi', name = "High Confidence", lineStyle = list(type = "dashed"), color = "green") %>%
    echarts4r::e_x_axis(name = "Date") %>%
    echarts4r::e_y_axis(name = "Value") %>%
    echarts4r::e_tooltip(trigger = "axis") %>%
    echarts4r::e_legend(show = TRUE)

  time_series_plot
}
# =========== #create bar chart (weekly time series) function ==================================
#' @export
  create_weekly_bar_chart <- function(historical_data = NULL, prediction_data = NULL, x_var = NULL, y_var = NULL) {

    historical_data <- historical_data[order(historical_data[[x_var]]), ]
    prediction_data <- prediction_data[order(prediction_data[[x_var]]), ]

    historical_data[[x_var]] <- as.Date(historical_data[[x_var]])
    prediction_data[[x_var]] <- as.Date(prediction_data[[x_var]])

    combined_data <- dplyr::full_join(historical_data, prediction_data, by = x_var, suffix = c(".hist", ".pred"))

    weekly_data <- combined_data %>%
      dplyr::mutate(week = lubridate::floor_date(as.Date(get(x_var)), unit = "week")) %>%
      dplyr::group_by(week) %>%
      dplyr::summarise(across(ends_with(".hist"), mean, na.rm = TRUE),
                       across(ends_with(".pred"), mean, na.rm = TRUE),
                      `_conf_lo` = mean(`_conf_lo`, na.rm = TRUE),
                      `_conf_hi` = mean(`_conf_hi`, na.rm = TRUE))


   weekly_bar_chart <- weekly_data %>%
     echarts4r::e_charts_('week') %>%
     echarts4r::e_bar_(paste0(y_var, ".hist"), name = "Historical Values", stack = "grp") %>%
     echarts4r::e_bar_(paste0(y_var, ".pred"), name = "Prediction Values", stack = "grp2", color = "orange") %>%
     echarts4r::e_x_axis(name = "Week") %>%
     echarts4r::e_y_axis(name = "Value") %>%
     echarts4r::e_tooltip(trigger = "axis") %>%
     echarts4r::e_legend(show = TRUE)

   weekly_bar_chart
  }


# ===================== output data function ( prediction data)==================
calendars_businness_days <- function(db_conn = NULL, country_code = NULL) {
  tryCatch(
    {
      query_statement_output <- glue::glue("SELECT * FROM public.holidays_country_years where countrycode = '{country_code}'")

      query_res_output <- DBI::dbSendQuery(db_conn, statement = query_statement_output)
      calendar_bh_country <- DBI::dbFetch(query_res_output)

      return(calendar_bh_country)
    },
    error = function(error) {
      print(error)
      DBI::dbDisconnect(db_conn)
      shinyalert::shinyalert("Error returning output data. Please check the conexion.", type = "error")
      return(NULL)
    }
  )
}
