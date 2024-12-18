#' Calibre et évalue une liste de modèles
#'
#' Une fonction qui prend une liste de modèles entraînés à calibrer avec les
#' données de test et qui calcule ensuite les différentes mesures d'évaluation.
#'
#' @param input_data ensemble de données de test
#' @param model_table liste des modèles entraînés
#'
#' @return une liste avec 2 éléments : modèles calibrés - métriques d'évaluation
#' @export
#'
#' @examples model_evaluation(input_data, model_tbl)
model_evaluation <- function(input_data, model_table) {
  calib_tbl <- model_table %>%
    modeltime::modeltime_calibrate(rsample::testing(input_data), quiet = FALSE)

  accuracy_model <- calib_tbl %>%
    modeltime::modeltime_accuracy(metric_set = yardstick::metric_set(yardstick::mae, yardstick::rmse, yardstick::rsq))

  list("model_calibrated" = calib_tbl, "accuracy_models" = accuracy_model)
}


#' @export
th2_benchmarking <- function(test_data, forecasting_data, group_target = NULL, group_value = NULL, target_var = NULL, date_var = NULL, as_of = NULL) {
  test_data <- test_data[order(test_data[["_date"]]), ]

  forecasting_data <- forecasting_data[order(forecasting_data[["_date"]]), ]

  list_models <- unique(forecasting_data$`_model_desc`)

  if (!is.null(group_value)) {
    forecasting_data <- forecasting_data %>%
      dplyr::filter(forecasting_data[[group_target]] == !!group_value)
  }

  df_accuracy_test <- tibble(
    id = numeric(),
    .model_desc = character(),
    .type = character(),
    mae = numeric(),
    rmse = numeric(),
    rsq = numeric()
  )

  y <- as.double(unlist(test_data[[target_var]]))

  for (model in list_models) {
    y_hat <- as.double(forecasting_data %>%
      dplyr::filter(forecasting_data$`_model_desc` == model & forecasting_data$execution_date == as_of) %>%
      dplyr::pull(!!target_var))

    add_model <- tibble(
      id = group_value,
      .model_desc = model,
      .type = "Test",
      mae = yardstick::mae_vec(y, y_hat),
      rmse = yardstick::rmse_vec(y, y_hat),
      rsq = yardstick::rsq_vec(y, y_hat)
    )

    df_accuracy_test <- rbind(df_accuracy_test, add_model)
  }

  return(df_accuracy_test)
}

#' @export
th2_metric_test <- function(test_data, forecasting_data, date_var = NULL, target_var = NULL) {
  test_data <- test_data[order(test_data[[date_var]]), ]

  forecasting_data <- forecasting_data[order(forecasting_data$.index), ]

  list_models <- unique(forecasting_data$`.model_desc`)
  list_kpi <- unique(forecasting_data$column_kpi)

  # if (!is.null(group_value)) {
  #   forecasting_data <- forecasting_data %>%
  #     dplyr::filter(forecasting_data[[group_target]] == !!group_value)
  # }

  df_accuracy_test <- tibble(
    model_desc = character(),
    kpi_data = character(),
    type = character(),
    test_mae = numeric(),
    test_rmse = numeric(),
    test_rsq = numeric()
  )

  for (kpi_bench in list_kpi)
  {
    test_data_bench <- test_data %>%
      dplyr::group_by_at(dplyr::vars(date_var, "column_kpi")) %>%
      dplyr::summarise_at(dplyr::vars(target_var), sum)

    test_data_bench <- test_data_bench %>%
      dplyr::filter(column_kpi == kpi_bench)

    y <- as.double(unlist(test_data_bench[[target_var]]))

    forecasting_data_kpi <- forecasting_data %>%
      dplyr::filter(column_kpi == kpi_bench)

    for (model in list_models) {
      y_hat <- as.double(forecasting_data_kpi %>%
        dplyr::filter(forecasting_data_kpi$`.model_desc` == model) %>%
        dplyr::pull(.value))

      add_model <- tibble(
        model_desc = model,
        kpi_data = kpi_bench,
        type = "Test",
        test_mae = yardstick::mae_vec(y, y_hat),
        test_rmse = yardstick::rmse_vec(y, y_hat),
        test_rsq = yardstick::rsq_trad_vec(y, y_hat)
      )

      df_accuracy_test <- rbind(df_accuracy_test, add_model)
    }
  }

  return(df_accuracy_test)
}


#' @export
th2_benchmark_timegpt <- function(input_data, group_target, target_var, date_var, forecast_horizon, train_split = NULL) {
  dataframe_input <- input_data


  dataframe_input <- dataframe_input %>%
    dplyr::group_by_at(dplyr::vars(date_var, group_target)) %>%
    dplyr::summarise_at(dplyr::vars(target_var), sum)

  data_clean <- preprocessing_data(dataframe_input %>% dplyr::select(target_var, date_var))$dataset_clean # [["dataset_clean"]]

  dataframe_input[[target_var]] <- data_clean[[target_var]]

  if (!is.null(train_split)) {
    train_data <- dataframe_input %>%
      dplyr::filter(.data[[date_var]] < as.Date(train_split))

    test_data <- dataframe_input %>%
      dplyr::filter(.data[[date_var]] >= as.Date(train_split)) %>%
      dplyr::rename(date := !!date_var)

    forecast_horizon <- nrow(test_data)
  } else {
    train_data <- dataframe_input
  }

  if (group_target == "all_columns") {
    train_data <- train_data %>%
      tidyr::pivot_longer(!date_var, names_to = "id", values_to = target_var) %>%
      dplyr::rename(date := !!date_var)
    group_target <- "id"
  } else {
    select_vars <- c(group_target, date_var, target_var)

    train_data <- train_data %>%
      dplyr::select(all_of(select_vars)) %>%
      dplyr::rename(id := !!group_target, date := !!date_var)
    group_target <- "id"
  }

  nixtlar::nixtla_set_api_key(api_key = "nixtla-tok-mxDQi7K32j4ZygikZfIpHdGMUMAYj0pIhKUFn327lx68ToncxK3WHZTsGgnpM6E57dJDMD3Kmm2EWFEP")

  dataset_large_filter <- train_data %>%
    dplyr::select(id, !!date_var, !!target_var) %>%
    dplyr::rename(ds = !!date_var, y = !!target_var, unique_id = id)

  nixtla_fcst <- nixtlar::nixtla_client_forecast(dataset_large_filter, h = forecast_horizon, id_col = "unique_id", level = c(95), model = "timegpt-1-long-horizon", finetune_steps = 10)

  timegpt_result <- nixtla_fcst %>%
    dplyr::rename(.index = ds, .value = TimeGPT, .conf_lo = `TimeGPT-lo-95`, .conf_hi = `TimeGPT-hi-95`) %>%
    dplyr::select(-unique_id) %>%
    dplyr::mutate(.model_id = 0, .model_desc = "TIMEGPT", .key = "prediction", as_of = Sys.Date(), start_date = min(input_data[[date_var]]), end_date = max(input_data[[date_var]]), accuracy = list(data.frame())) %>%
    dplyr::relocate(.model_id, .before = 1) %>%
    dplyr::relocate(.model_desc, .after = 1) %>%
    dplyr::relocate(.key, .after = 2) %>%
    as_tibble()

  return(timegpt_result)
}


#' @export
th2_rolling_forecast_stablizer <- function(input_data, var_date, var_target, kpi, model, split_date = NULL, horizon = NULL, months_test = 3, previsions = 3, use_holidays = NULL, country_column = NULL, lags = FALSE, path_driver = NULL, use_meteo = NULL, spark_conection = NULL, use_timegpt = TRUE) {
  list_kpis <- unique(input_data[[kpi]])

  if (is.null(split_date)) {
    max_date <- max(input_data[[var_date]])

    size_data <- (31 * months_test) + (previsions * horizon)

    date_filter <- max_date - size_data

    input_data <- input_data %>%
      filter(input_data[[var_date]] >= date_filter)

    year_data <- unique(lubridate::year(input_data[[var_date]]))

    months_list <- unique(lubridate::month(input_data[[var_date]]))

    start_date <- as.Date(paste0(year_data, "-", months_list[2], "-01"), format = "%Y-%m-%d")
    end_date <- as.Date(paste0(year_data, "-", months_list[2 + months_test], "-01"), format = "%Y-%m-%d")

    input_data <- input_data %>%
      dplyr::filter(input_data[[var_date]] >= start_date & input_data[[var_date]] < end_date)
  } else {
    input_data <- input_data %>%
      dplyr::rename(column_kpi = !!kpi)
  }

  num_days <- 0
  list_forecast <- list()
  last_date <- ""

  accuracy_df <- data.frame()

  for (i in c(1:previsions)) {
    # num_days = num_days + 7
    # cat("\nForecast for dates between", as.character(data$X_date[1] %m+% months(1)), " and ", as.character(data$X_date[1] %m+% months(2) + num_days), "\n")

    if (is.null(split_date)) {
      date_increment <- input_data[[var_date]][1] %m+% months(2) + num_days

      data_prevision <- input_data %>% dplyr::filter(input_data[[var_date]] >= input_data[[var_date]][1] %m+% months(1) & input_data[[var_date]] < date_increment)

      data_test <- input_data %>% dplyr::filter(input_data[[var_date]] >= date_increment & input_data[[var_date]] < date_increment + horizon)
    } else {
      data_prevision <- input_data

      data_test <- input_data %>%
        dplyr::filter(input_data[[var_date]] >= as.Date(split_date) & input_data[[var_date]] < as.Date(split_date) + horizon)
    }

    output_forecast <- th2_bulk_forecasting_spark(data_prevision, "column_kpi", var_target, var_date, horizon, c(model), train_split = split_date, lags = lags, use_holidays = use_holidays, country_column = country_column, path_driver = path_driver, use_meteo = use_meteo)

    if (use_timegpt == TRUE) {
      timegpt <- th2_benchmark_timegpt(data_prevision, "column_kpi", var_target, var_date, horizon, train_split = split_date)
      output_forecast <- rbind(output_forecast, timegpt)
    }

    if (!("column_kpi" %in% colnames(output_forecast))) {
      output_forecast$column_kpi <- list_kpis[1]
    }

    if (!is.null(split_date)) {
      output_forecast <- output_forecast %>%
        dplyr::filter(`.index` < as.Date(split_date) + horizon)
    }

    if (use_timegpt == TRUE) {
      output_forecast$accuracy[1][[1]] <- output_forecast$accuracy[1][[1]] %>%
        dplyr::add_row(.model_id = 0, .model_desc = "TIMEGPT", .type = "Test", mae = Inf, rmse = Inf, rsq = 0)
    }

    test_metric <- cbind(
      th2_metric_test(data_test, output_forecast, var_date, var_target),
      output_forecast$accuracy[1][[1]] %>%
        dplyr::select(mae, rmse, rsq)
    )
    if (is.null(split_date)) {
      test_metric <- test_metric %>%
        dplyr::mutate(period = paste0(as.character(input_data[[var_date]][1] %m+% months(1)), " - ", as.character(date_increment)))
    } else {
      test_metric <- test_metric %>%
        dplyr::mutate(period = paste0(min(input_data[[var_date]]), " - ", as.character(split_date)))
    }

    accuracy_df <- rbind(accuracy_df, test_metric)

    # print(output_forecast)
    # accuracy_df <- rbind(
    #   accuracy_df,
    #   as.data.frame(output_forecast$accuracy[1]) %>%
    #     dplyr::mutate(period = paste0(as.character(data$X_date[1] %m+% months(1)), " - ", as.character(data$X_date[1] %m+% months(2) + num_days)) )
    #   )

    list_forecast[i] <- output_forecast %>%
      dplyr::select(column_kpi, `.index`, `.value`, `.model_desc`) %>%
      list()

    if (is.null(split_date)) {
      last_date <- date_increment + horizon
      num_days <- num_days + horizon
    } else {
      split_date <- as.Date(split_date) + horizon
      last_date <- split_date
    }
  }

  # browser()
  accuracy_df <- accuracy_df %>%
    dplyr::mutate(test_mae = round(test_mae, 2)) %>%
    dplyr::mutate(test_rmse = round(test_rmse, 2)) %>%
    dplyr::mutate(test_rsq = round(test_rsq, 2)) %>%
    dplyr::select(-c(`type`))

  input_data <- input_data %>%
    dplyr::group_by_at(dplyr::vars(var_date, "column_kpi")) %>%
    dplyr::summarise_at(dplyr::vars(var_target), sum)

  input_data <- input_data %>%
    dplyr::filter(.data[[var_date]] >= as.Date(last_date) - (10 * horizon) & .data[[var_date]] <= as.Date(last_date) - 1)

  list_kpis_plots <- list()
  input_data_plot <- ""

  model <- c(model, "timegpt", "TH2ENSEMBLE")

  for (kpi_plot in list_kpis)
  {
    # "grouped_df" "tbl_df"     "tbl"        "data.frame"
    hist_plot <- input_data %>%
      dplyr::filter(column_kpi == kpi_plot)

    forecast_inter_one <- list_forecast[[1]] %>%
      dplyr::filter(column_kpi == kpi_plot)

    forecast_inter_two <- list_forecast[[2]] %>%
      dplyr::filter(column_kpi == kpi_plot)

    forecast_inter_three <- list_forecast[[3]] %>%
      dplyr::filter(column_kpi == kpi_plot)

    list_plots <- list()

    for (m in model) {
      time_series_plot <- NULL

      if (m == "lr") {
        m <- "lm"
      }

      if (m == "mars") {
        m <- "earth"
      }

      # time_series_plot <- plotly::plot_ly() %>%
      #   plotly::add_trace(
      #     data = input_data,
      #     type = "scatter", mode = "lines",
      #     x = ~ get(var_date), y = ~ get(var_target), name = "Historical Values", color = I("#2C3E50")
      #   ) %>%
      #   plotly::add_trace(
      #     data = forecast_inter_one %>%
      #               dplyr::filter(`.model_desc` == gsub("_", "", toupper(m))),
      #     type = "scatter", mode = "lines",
      #     x = ~ get(".index"), y = ~ get(".value"), name = paste(m, "1"), color = I("#B0226B")
      #   ) %>%
      #   plotly::add_trace(
      #     data = forecast_inter_two %>%
      #       dplyr::filter(`.model_desc` == gsub("_", "", toupper(m))),
      #     type = "scatter", mode = "lines",
      #     x = ~ get(".index"), y = ~ get(".value"), name = paste(m, "2"), color = I("#6DB539")
      #   ) %>%
      #   plotly::add_trace(
      #     data = forecast_inter_three %>%
      #       dplyr::filter(`.model_desc` == gsub("_", "", toupper(m))),
      #     type = "scatter", mode = "lines",
      #     x = ~ get(".index"), y = ~ get(".value"), name = paste(m, "3"),  color = I("#7AC6EA")
      #   ) %>%
      #   plotly::layout(
      #     autosize = FALSE,
      #     width = 950,
      #     title = "Time Series Test",
      #     xaxis = list(title = "Date"),
      #     yaxis = list(title = "Value"),
      #     showlegend = TRUE
      #   )

      forecast_one_temp <- forecast_inter_one %>%
        dplyr::filter(`.model_desc` == gsub("_", "", toupper(m))) %>%
        dplyr::rename(!!var_date := `.index`, forecast_1 = `.value`) %>%
        dplyr::select(!!var_date, forecast_1)

      forecast_two_temp <- forecast_inter_two %>%
        dplyr::filter(`.model_desc` == gsub("_", "", toupper(m))) %>%
        dplyr::rename(!!var_date := `.index`, forecast_2 = `.value`) %>%
        dplyr::select(!!var_date, forecast_2)

      forecast_three_temp <- forecast_inter_three %>%
        dplyr::filter(`.model_desc` == gsub("_", "", toupper(m))) %>%
        dplyr::rename(!!var_date := `.index`, forecast_3 = `.value`) %>%
        dplyr::select(!!var_date, forecast_3)

      data_temp <- full_join(hist_plot, forecast_one_temp, by = var_date)
      data_temp <- full_join(data_temp, forecast_two_temp, by = var_date)
      data_temp <- full_join(data_temp, forecast_three_temp, by = var_date)

      time_series_plot <- data_temp %>%
        echarts4r::group_by(column_kpi) %>%
        echarts4r::e_charts(date) %>%
        echarts4r::e_line(sales, name = "Historical Values", color = "#2C3E50") %>%
        echarts4r::e_line(forecast_1, name = paste(m, "1"), color = "#B0226B") %>%
        echarts4r::e_line(forecast_2, name = paste(m, "2"), color = "#6DB539") %>%
        echarts4r::e_line(forecast_3, name = paste(m, "3"), color = "#7AC6EA") %>%
        echarts4r::e_title("Time Series Test") %>%
        echarts4r::e_x_axis(date, name = "Date") %>%
        echarts4r::e_y_axis(sales, name = "Value") %>%
        echarts4r::e_legend(TRUE) %>%
        echarts4r::e_tooltip(trigger = "axis")


      list_plots[[m]] <- time_series_plot
    }
    list_kpis_plots[[kpi_plot]] <- list_plots
  }

  return(list(time_series_plots = list_kpis_plots, accuracy_df = accuracy_df))
}
