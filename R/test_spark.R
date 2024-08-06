#' @export
th2_forecast_spark <- function(input_data, group_target, target_var, date_var, future_forecast, models_list, train_split=NULL, lags = FALSE, group_by_col = NULL, master_spark = NULL){
  library(sparklyr)
  # suppressPackageStartupMessages({
  #   library(sparklyr)
  # })
  result_forecast <- data.frame()
  Sys.setenv("SPARK_HOME" = sparklyr::spark_home_dir(version = "3.5.1"))
  ip_address <- system("hostname -I | awk '{print $1}'", intern = TRUE)

  # options(sparklyr.log.console = TRUE)

  config <- sparklyr::spark_config()
  config$spark.executor.memory <- "2000M"
  config$spark.driver.memory <- "6G"

  config$`spark.executor.cores` <- "1"
  config$`spark.executor.instances` <- "12"

  config$`spark.executor.extraJavaOptions=-Dlog4j.logLevel`<- "debug"
  config$`spark.app.name` <- "test bulk forecast"
  config$`spark.driver.host` <- ip_address
  config$spark.executor.extraJavaOptions <- "-XX:+UseG1GC -XX:InitiatingHeapOccupancyPercent=35"

  sc <- sparklyr::spark_connect(master = master_spark, config = config)
  # sc <- sparklyr::spark_connect(master = "local")

  dataset_raw <- input_data %>%
    dplyr::select(group_by_col)%>%
    dplyr::distinct_all(.keep_all = TRUE)

  unique_kpis  <- sparklyr::sdf_copy_to(sc, dataset_raw)

  result_forecast <- sparklyr::spark_apply(
    unique_kpis,
    function(e){
      library(dplyr);
      library(modeltime);
      dataset_raw <- input_data;

      tryCatch(
        {
          result_forecast <- e %>%
            dplyr::inner_join(dataset_raw, by = group_by_col) %>%
            th2forecast::th2_bulk_forecasting_spark(., group_target, target_var, date_var, future_forecast, models_list, train_split = train_split, lags = lags);

          return(result_forecast)
        },
        error = function(error) {
          print(error)
          shinyalert::shinyalert("Error th2Forecast.", type = "error")
          return(NULL)
        }
      )

    },
    group_by = group_by_col,
    packages = FALSE)

  # write.csv(x=result_forecast, file="result_forecast.csv", row.names = FALSE)
  result_forecast <- sparklyr::sdf_collect(result_forecast)

  result_forecast <- result_forecast %>%
    tidyr::unite(!!group_target, all_of(group_by_col), sep = "_", remove = TRUE)

  spark_disconnect(sc)

  return(result_forecast)
}


#' @export
test_forecast_spark_perfomance <- function(input_data, group_target, target_var, date_var, future_forecast, models_list, train_split=NULL, lags = FALSE, test_models = FALSE, group_by_col = NULL, master_spark = NULL){
  library(sparklyr)
  library(dplyr)
  library(modeltime)

  Sys.setenv("SPARK_HOME" = sparklyr::spark_home_dir(version = "3.5.1"))
  ip_address <- system("hostname -I | awk '{print $1}'", intern = TRUE)

  # options(sparklyr.log.console = TRUE)

  config <- sparklyr::spark_config()
  config$spark.executor.memory <- "2000M"
  config$spark.driver.memory <- "6G"

  config$`spark.executor.cores` <- "1"
  config$`spark.executor.instances` <- "12"

  config$`spark.executor.extraJavaOptions=-Dlog4j.logLevel`<- "debug"
  config$`spark.app.name` <- "test bulk forecast"
  config$`spark.driver.host` <- ip_address
  config$spark.executor.extraJavaOptions <- "-XX:+UseG1GC -XX:InitiatingHeapOccupancyPercent=35"

  sc <- sparklyr::spark_connect(master = master_spark, config = config)

  dataset_raw <- input_data %>%
    dplyr::select(group_by_col)%>%
    dplyr::distinct_all(.keep_all = TRUE)

  unique_kpis  <- sparklyr::sdf_copy_to(sc, dataset_raw)

  list_parallel <- NULL
  list_serie <- NULL

  df_performance <- data.frame()

  print("Start of parallel process")

  pb <- utils::txtProgressBar(min = 0, max = 100, style = 3, width = 50, char = "=")

  for (nkpis in c(10,50,100)) {

    t <- proc.time()

    result_forecast <- sparklyr::spark_apply(
      unique_kpis %>% head(nkpis),
      function(e){
        library(dplyr);
        library(modeltime);
        dataset_raw <- input_data;

        result_forecast <- e %>%
          dplyr::inner_join(dataset_raw, by = group_by_col) %>%
          suppressMessages(th2forecast::th2_bulk_forecasting_spark(., group_target, target_var, date_var, future_forecast, models_list, train_split = train_split, lags = lags));

        return(result_forecast)
      },
      group_by = group_by_col,
      packages = FALSE)

    time_execution <- proc.time() - t
    time_execution <- time_execution[[3]]

    # list_parallel <- c(list_parallel, list(!!nkpis := time_execution))
    # list_parallel[[as.character(nkpis)]] <- time_execution
    row_execution <- data.frame(type = "parallel", n_kpis = as.character(nkpis), time_execution = time_execution)

    df_performance <- rbind(df_performance, row_execution)

    utils::setTxtProgressBar(pb, nkpis)

  }

  close(pb)
  print("End of parallel process")

  if(test_models == TRUE & length(models_list) > 1){
    print("Start of parallel process by model")
    for (model in models_list) {
      t <- proc.time()

      result_forecast <- sparklyr::spark_apply(
        unique_kpis %>% head(100),
        function(e){
          library(dplyr);
          library(modeltime);
          dataset_raw <- input_data;

          result_forecast <- e %>%
            dplyr::inner_join(dataset_raw, by = group_by_col) %>%
            suppressWarnings(suppressMessages({th2forecast::th2_bulk_forecasting_spark(., group_target, target_var, date_var, future_forecast, c(model), train_split = train_split, lags = lags)}));

          return(result_forecast)
        },
        group_by = group_by_col,
        packages = FALSE)

      time_execution <- proc.time() - t
      time_execution <- time_execution[[3]]

      row_execution <- data.frame(type = model, n_kpis = "100", time_execution = time_execution)

      df_performance <- rbind(df_performance, row_execution)
    }

    print("End of parallel process by model")
  }

  spark_disconnect(sc)

  print("Start of series process")
  for (nkpis in c(10,50,100)) {

    t <- proc.time()

    print(paste(nkpis," kpis process in series"))
    pb <- utils::txtProgressBar(min = 0, max = nkpis, style = 3, width = 50, char = "=")
    for (i_kpi in 1:nkpis) {
      data_serie <- dataset_raw[i_kpi, ] %>%
        dplyr::inner_join(input_data, by = group_by_col)
      suppressWarnings(suppressMessages({
        result_forecast <-  th2forecast::th2_bulk_forecasting_spark(data_serie, group_target, target_var, date_var, future_forecast, models_list, train_split = train_split, lags = lags)
      }))

      utils::setTxtProgressBar(pb, i_kpi)
    }

    close(pb)

    time_execution <- proc.time() - t
    time_execution <- time_execution[[3]]

    # list_serie[[as.character(nkpis)]] <- time_execution
    # list_serie <- c(list_serie, list(!!nkpis := time_execution))
    row_execution <- data.frame(type = "series", n_kpis = as.character(nkpis), time_execution = time_execution)

    df_performance <- rbind(df_performance, row_execution)

  }
  print("End of series process")

  return(df_performance)
}
