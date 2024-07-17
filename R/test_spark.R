library(sparklyr)

function_test_sparklyr <- function(){
  ip_address <- system("hostname -I | awk '{print $1}'", intern = TRUE)

  config <- sparklyr::spark_config()
  config$spark.executor.memory <- "8G"
  config$`spark.executor.cores` <- "1"
  config$`spark.executor.extraJavaOptions=-Dlog4j.logLevel`<- "debug"
  config$`spark.app.name` <- "minhut test 1222333"
  config$`spark.driver.host` <- ip_address


  sc <- sparklyr::spark_connect(master = "spark://spark-1721050712-master-0.spark-1721050712-headless.th2mage.svc.cluster.local:7077", config = config)


  result <- th2forecast::th2_bulk_forecasting(dataset_raw, "family", "sales", "X_date", 50, c("prophet"), train_split = "2016-12-31", spark_conection =  sc)
}
