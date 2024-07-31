#' @export
th2_forecast_spark <- function(input_data, group_target, target_var, date_var, future_forecast, models_list, train_split=NULL, lags = FALSE){
  library(sparklyr)
  # suppressPackageStartupMessages({
  #   library(sparklyr)
  # })

  Sys.setenv("SPARK_HOME" = sparklyr::spark_home_dir(version = "3.5.1"))
  ip_address <- system("hostname -I | awk '{print $1}'", intern = TRUE)

  options(sparklyr.log.console = TRUE)

  config <- sparklyr::spark_config()
  config$spark.executor.memory <- "2000M"
  config$spark.driver.memory <- "6G"

  config$`spark.executor.cores` <- "1"
  config$`spark.executor.instances` <- "12"

  config$`spark.executor.extraJavaOptions=-Dlog4j.logLevel`<- "debug"
  config$`spark.app.name` <- "test bulk forecast"
  config$`spark.driver.host` <- ip_address
  config$spark.executor.extraJavaOptions <- "-XX:+UseG1GC -XX:InitiatingHeapOccupancyPercent=35"

  sc <- sparklyr::spark_connect(master = "spark://spark-1721310514-master-0.spark-1721310514-headless.th2mage.svc.cluster.local:7077", config = config)
  # sc <- sparklyr::spark_connect(master = "local")

  dataset_raw <- input_data %>%
    dplyr::select(store_nbr, family)%>%
    dplyr::distinct_all(.keep_all = TRUE)

  unique_kpis  <- sparklyr::sdf_copy_to(sc, dataset_raw)

  result_forecast <- sparklyr::spark_apply(
    unique_kpis,
    function(e){
      library(dplyr);
      library(modeltime);
      dataset_raw <- input_data;

      # tryCatch(
      #   {
          result_forecast <- e %>%
            dplyr::inner_join(dataset_raw, by = c("store_nbr","family")) %>%
            th2forecast::th2_bulk_forecasting_spark(., group_target, target_var, date_var, future_forecast, models_list, train_split = train_split, lags = lags);

          return(result_forecast)
        # },
        # error = function(error) {
        #   print(error)
        #   shinyalert::shinyalert("Error th2 forecast.", type = "error")
        #   return(NULL)
        # }
      # )

    },
    group_by = c("family","store_nbr"),
    packages = FALSE)

  write.csv(x=result_forecast, file="result_forecast.csv", row.names = FALSE)

  spark_disconnect(sc)
}
