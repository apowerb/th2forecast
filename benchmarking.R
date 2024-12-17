# nixtlar_key <- "nixak-*****"

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
  nixtla_client_fcst <- nixtlar::nixtla_client_forecast(train_dataset, h = fcast_horizon, level = c(80,95))


  nixtlar::nixtla_client_plot(main_dataset, nixtla_client_fcst, max_insample_length = 200, h = 30)


  th2fcast <- th2forecast::th2_bulk_forecasting(input_data = train_dataset,
                                                group_target = 'unique_id',
                                                target_var = 'y',
                                                date_var = 'ds',
                                                future_forecast = fcast_horizon,
                                                models_list = c("xgboost"))

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
    echarts4r::e_line(serie = y)%>%
    echarts4r::e_line(serie = TimeGPT)%>%
    echarts4r::e_line(serie = thaink2)

  delta_nixtlar = th2nixtlar%>%
    yardstick::rmse(data = ., truth = "y", estimate = "TimeGPT")%>%
    dplyr::mutate(model = "TimeGTP", metric = "rmse")

  delta_th2 = th2nixtlar%>%
    yardstick::rmse(data = ., truth = "y", estimate = "thaink2")%>%
    dplyr::mutate(model = "thaink2", metric = "rmse")

  benchmark_res <- list(benchmark_chart = benchmark_chart,
                        benchmark_perf = delta_nixtlar%>%dplyr::bind_rows(delta_th2) )
  return(benchmark_res)
}

main_dataset <- nixtlar::electricity

benchmark_results <- compare_th2_vs_nixtlar(main_dataset = main_dataset,
                                            fcast_horizon = 100,
                                            target_kpi = "NP")
benchmark_results$benchmark_perf

benchmark_results$benchmark_chart
