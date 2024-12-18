# nixtlar_key <- "nixak-ZJh0dHQi2PVSxL4glpz4IuHaKfnuK1y93gwXOK2EbjeCnvudm6Src6qnjSe0RQknPZRDWwPx2F8S1ZfV"
#
# nixtlar::nixtla_set_api_key(api_key = nixtlar_key)


compare_th2_vs_nixtlar <- function(main_dataset = NULL, fcast_horizon = 200, target_kpi = "DE"){
  main_dataset <- main_dataset%>%
    dplyr::filter(unique_id == !!target_kpi)
  main_dataset <- main_dataset%>%
    dplyr::mutate(ds = lubridate::as_datetime(ds))%>%
    dplyr::filter(unique_id == target_kpi)%>%
    dplyr::arrange(ds)

  train_dataset <- main_dataset%>%
    head(round(nrow(main_dataset)*0.85,0))
  main_dataset2 <<- main_dataset
  nixtla_client_fcst <- nixtlar::nixtla_client_forecast(train_dataset, h = fcast_horizon, level = c(80,95))


  nixtlar::nixtla_client_plot(main_dataset, nixtla_client_fcst, max_insample_length = 200, h = 30)


  th2fcast <- th2forecast::th2_bulk_forecasting(input_data = train_dataset,
                                                group_target = 'unique_id',
                                                target_var = 'y',
                                                date_var = 'ds',
                                                future_forecast = fcast_horizon,
                                                models_list = c("random_forest"))

  th2fcast_short2 <- th2fcast%>%
    dplyr::rename(thaink2 = .value, ds = .index)
  th2nixtlar <- nixtla_client_fcst%>%
    dplyr::inner_join(th2fcast_short2, by = c("ds","unique_id"))%>%
    dplyr::full_join(main_dataset, by = c("ds","unique_id"))%>%
    dplyr::arrange(ds)

  benchmark_chart <- th2nixtlar%>%
    tail(300)%>%
    dplyr::arrange(ds)%>%
    echarts4r::e_chart(data = ., x = ds)%>%
    echarts4r::e_line(serie = y, lineStyle = list(type = "normal"), color = "#013DFF")%>%
    echarts4r::e_line(serie = TimeGPT, lineStyle = list(type = "normal"), color = "black")%>%
    echarts4r::e_line(serie = thaink2, lineStyle = list(type = "normal"), color = "#00FFC5")

  delta_nixtlar = th2nixtlar%>%
    yardstick::rmse(data = ., truth = "y", estimate = "TimeGPT")%>%
    dplyr::mutate(model = "TimeGTP")

  delta_th2 = th2nixtlar%>%
    yardstick::rmse(data = ., truth = "y", estimate = "thaink2")%>%
    dplyr::mutate(model = "thaink2")

  benchmark_perf <- delta_nixtlar%>%dplyr::bind_rows(delta_th2)%>%
    dplyr::mutate(kpi = !!target_kpi, forecast_horizon = !!fcast_horizon)

  benchmark_res <- list(benchmark_chart = benchmark_chart,
                        benchmark_perf = benchmark_perf)
  return(benchmark_res)
}

compare_th2_vs_nixtlar_bulk <- function(){
  main_dataset <- nixtlar::electricity

  fcast_horizons <- c(5, 20, 100, 300)
  target_kpis <- c("BE","DE","PJM","FR")
  benchmark_perfs <- data.frame()
  for(fcast_horizon in fcast_horizons){
    for(target_kpi in target_kpis){
      benchmark_results <- compare_th2_vs_nixtlar(main_dataset = main_dataset,
                                                  fcast_horizon = fcast_horizon,
                                                  target_kpi = target_kpi)
      benchmark_perfs <- benchmark_perfs%>%
        dplyr::bind_rows(benchmark_results$benchmark_perf)
    }
  }
  return(benchmark_perfs)
}

benchmark_perfs <- compare_th2_vs_nixtlar_bulk()

perf_table <- benchmark_perfs%>%
  dplyr::mutate(.estimate = round(.estimate,3))%>%
  dplyr::select(-.estimator)%>%
  tidyr::pivot_wider(names_from = "model", values_from = ".estimate")%>%
  dplyr::mutate(best_model = dplyr::case_when(TimeGTP  < thaink2 ~ "TimeGTP", .default = "THAINK²"))%>%
  dplyr::mutate(delta_percent = dplyr::case_when(TimeGTP  < thaink2 ~ (thaink2 - TimeGTP)/thaink2, .default = (TimeGTP - thaink2)/TimeGTP))

perf_table_dt <- perf_table%>%
  DT::datatable(options = list(pageLength = 16))%>%
  DT::formatPercentage("delta_percent", digits = 2)%>%
  DT::formatStyle("best_model",
                  # target = "row",
                  color = "white",
                  backgroundColor = DT::styleEqual(c("TimeGTP","THAINK²"), c("black","#00FFC5")))%>%
  DT::formatStyle(
    'delta_percent',
    background = DT::styleColorBar(c(0,1), 'orange'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  )

perf_table_dt



#===============

source("C:/TEMP/thaink2/Dive2ML/th2forecast/R/th2forecast_api_helpers.R")
base_url <- "http://127.0.0.1:3838/"
# base_url <- "https://apis-dev.thaink2.fr/"
fcast_res <- th2forecast_forecast_api(
  input_data = main_dataset2,
  base_url = base_url,
  fcast_horizon = 30,
  target_var = "y",
  date_var = "ds",
  group_target = "unique_id",
  models_list = c("xgboost")
)
fcast_res <- fcast_res%>%
  do.call(dplyr::bind_rows, . )
