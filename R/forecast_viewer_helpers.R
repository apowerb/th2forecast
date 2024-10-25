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

  if (!startsWith(date_var, "_")) {
    date_var <- paste0("_", date_var)
  }

  # Récupérer les données de prédiction

  tryCatch(
    {
      query_statement_output <- glue::glue("SELECT DISTINCT {group_target_var},{target_var}, {date_var} , _model_desc, _conf_lo, _conf_hi,execution_date, start_date, end_date
                                        FROM {schema}.{target_table}")
      if(schema == "" || is.null(schema)){
        query_statement_output <- glue::glue("SELECT DISTINCT {group_target_var},{target_var}, {date_var} , _model_desc, _conf_lo, _conf_hi,execution_date, start_date, end_date
                                        FROM {target_table}")
      }

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
      print(paste0("Error returning output data. Please check the output datasource configuration."))
      print(paste0("Error: ", error))
      return(NULL)
    }
  )
}

# ======================input data function ( historical data filtred) ==================
#' @export
input_data_fetch <- function(prediction_data = NULL, db_conn = NULL, target_table = NULL, target_var = NULL, group_target_var = NULL, date_var = NULL, as_of = NULL) {
  tryCatch(
    {
      if (as_of != "" || is.null(as_of)) {
        output_data_filtred <- prediction_data %>% dplyr::filter(execution_date == !!as_of)

        start_date <- as.Date(dplyr::first(output_data_filtred$start_date))
        end_date <- as.Date(dplyr::first(output_data_filtred$end_date))

        if (group_target_var == "all_columns") {
          query_statement_input <- glue::glue("SELECT DISTINCT * FROM {target_table}")

          query_res_input <- DBI::dbSendQuery(db_conn, statement = query_statement_input)
          historical_data <- DBI::dbFetch(query_res_input)
          historical_data <- historical_data %>%
            tidyr::pivot_longer(!date_var, names_to = group_target_var, values_to = target_var)
        } else {
          query_statement_input <- glue::glue("SELECT DISTINCT {group_target_var},{target_var}, {date_var}
                                         FROM {target_table}")

          query_res_input <- DBI::dbSendQuery(db_conn, statement = query_statement_input)
          historical_data <- DBI::dbFetch(query_res_input)
        }
        start_date <- as.Date(start_date)
        end_date <- as.Date(end_date)
        date_var <- tolower(date_var)
        historical_data_filtred <- historical_data %>% filter(between(as.Date(historical_data[[date_var]]), start_date, end_date))
        DBI::dbDisconnect(db_conn)
        return(historical_data_filtred)
      }
    },
    error = function(error) {
      DBI::dbDisconnect(db_conn)
      print(error)
      shinyalert::shinyalert("Error returning input data. Please check the input datasource configuration.", type = "error")
      return(NULL)
    }
  )
}


# ======= #merged data filtred function =======================================
#' @export
prediction_data_filtred <- function(prediction_data = NULL, model = NULL, kpi_value = NULL, group_target_var = NULL) {
  # browser()
  group_target_var <- tolower(group_target_var)
  model <- toupper(model)
  # kpi_value <- toupper(kpi_value)
  colnames(prediction_data) <- lapply(colnames(prediction_data), function(x) {
    if(startsWith(x, ".")){
      # remplacer les . par _
      x <- gsub("\\.", "_", x)
    } else {
      x
    }
  })

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
create_time_series_plot <- function(historical_data = NULL, prediction_data = NULL, x_var = NULL, y_var = NULL, agg_by = NULL) {
  historical_data2 <<- historical_data
  prediction_data2 <<- prediction_data
  if (!startsWith(x_var, "_") && is.null(prediction_data[[x_var]])) {
    prediction_data[[x_var]] <- prediction_data[[paste0("_",x_var)]]
  }

  new_name <- paste0(y_var, "_hist")
  historical_data <- historical_data%>%
    dplyr::arrange(.data[[x_var]])%>%
    dplyr::rename(!!quo_name(new_name) := actuals)%>%
    tail(100)

  new_name <- paste0(y_var, "_pred")
  prediction_data <- prediction_data%>%
    dplyr::arrange(.data[[x_var]])%>%
    dplyr::rename(!!quo_name(new_name) := `_value`)

  hist_var <- paste0(y_var, "_hist")
  pred_var <- paste0(y_var, "_pred")
  prediction_data <- prediction_data%>%
    # janitor::clean_names()%>%
    dplyr::select(!!x_var,!!pred_var, `_conf_lo`, `_conf_hi`)
  combined_data <- historical_data%>%
    dplyr::full_join(prediction_data, by = c(x_var))%>%
    dplyr::arrange(.data[[x_var]])
  # combined_data <- dplyr::full_join(historical_data, prediction_data, by = x_var, suffix = c("_hist", "_pred"))


  time_series_plot <- combined_data%>%
    echarts4r::e_charts_(x_var) %>%
    echarts4r::e_line_(hist_var, name = "actuals", lineStyle = list(type = "normal"), color = "blue") %>%
    echarts4r::e_line_(pred_var, name = "forecast", lineStyle = list(type = "normal"), color = "orange") %>%
    # echarts4r::e_line_("_conf_lo", name = "Low Conf.", lineStyle = list(type = "dashed"), color = "green") %>%
    # echarts4r::e_line_("_conf_hi", name = "Upper Conf.", lineStyle = list(type = "dashed"), color = "green") %>%
    echarts4r::e_x_axis(name = "Date") %>%
    echarts4r::e_y_axis_(name = y_var) %>%
    echarts4r::e_tooltip(trigger = "axis") %>%
    echarts4r::e_legend(show = TRUE)

  return(time_series_plot)
}
# =========== #create bar chart (weekly time series) function ==================================
#' @export
create_weekly_bar_chart <- function(historical_data = NULL, prediction_data = NULL, x_var = NULL, y_var = NULL, agg_type = "sum", agg_freq = "weeks") {
  if (!startsWith(x_var, "_") && is.null(prediction_data[[x_var]])) {
    prediction_data[[x_var]] <- prediction_data[[paste0("_",x_var)]]
  }

  new_name <- paste0(y_var, "_hist")
  historical_data <- historical_data%>%
    dplyr::arrange(!!x_var)%>%
    dplyr::rename(!!quo_name(new_name) := actuals)%>%
    tail(1000)

  new_name <- paste0(y_var, "_pred")
  prediction_data <- prediction_data%>%
    dplyr::arrange(!!x_var)%>%
    dplyr::rename(!!quo_name(new_name) := `_value`)


  combined_data <- historical_data%>%
    dplyr::bind_rows(prediction_data)

  combined_data <- combined_data %>%
    dplyr::mutate(day = lubridate::date(get(x_var)),
                  weeks = lubridate::ceiling_date(as.Date(get(x_var)), unit = "week", week_start = 1),
                  months = lubridate::ceiling_date(as.Date(get(x_var)), unit = "month", week_start = 1),
                  quarters = lubridate::ceiling_date(as.Date(get(x_var)), unit = "quarter", week_start = 1))

  if (agg_type == "sum") {
    weekly_data <- combined_data %>%
      dplyr::group_by(.data[[agg_freq]]) %>%
      dplyr::reframe(dplyr::across(dplyr::ends_with("_hist"), sum, na.rm = TRUE),
                     dplyr::across(dplyr::ends_with("_pred"), sum, na.rm = TRUE),
        `_conf_lo` = sum(`_conf_lo`, na.rm = TRUE),
        `_conf_hi` = sum(`_conf_hi`, na.rm = TRUE)
      )
  } else if (agg_type == "mean") {
    weekly_data <- combined_data %>%
      group_by(.data[[agg_freq]]) %>%
      summarise(across(ends_with("_hist"), mean, na.rm = TRUE),
        across(ends_with("_pred"), mean, na.rm = TRUE),
        `_conf_lo` = mean(`_conf_lo`, na.rm = TRUE),
        `_conf_hi` = mean(`_conf_hi`, na.rm = TRUE)
      )
  } else if (agg_type == "max") {
    weekly_data <- combined_data %>%
      dplyr::group_by(.data[[agg_freq]]) %>%
      dplyr::reframe(dplyr::across(dplyr::ends_with("_hist"), max, na.rm = TRUE),
                     dplyr::across(dplyr::ends_with("_pred"), max, na.rm = TRUE),
        `_conf_lo` = max(`_conf_lo`, na.rm = TRUE),
        `_conf_hi` = max(`_conf_hi`, na.rm = TRUE)
      )
  } else if (agg_type == "min") {
    weekly_data <- combined_data %>%
      group_by(.data[[agg_freq]]) %>%
      summarise(across(ends_with("_hist"), min, na.rm = TRUE),
        across(ends_with("_pred"), min, na.rm = TRUE),
        `_conf_lo` = min(`_conf_lo`, na.rm = TRUE),
        `_conf_hi` = min(`_conf_hi`, na.rm = TRUE)
      )
  }

  weekly_bar_chart <- weekly_data %>%
    echarts4r::e_charts_(agg_freq) %>%
    echarts4r::e_bar_(paste0(y_var, "_hist"), name = "Historical Values", stack = "grp", color = "blue") %>%
    echarts4r::e_bar_(paste0(y_var, "_pred"), name = "Prediction Values", stack = "grp", color = "orange") %>%
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
      query_statement_output <- glue::glue("SELECT * FROM public.holidays_country_years where country = '{country_code}'")

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
