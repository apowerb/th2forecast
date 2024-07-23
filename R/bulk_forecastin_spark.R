#' effectue le processus de prévision pour plusieurs séries temporelles avec différents modèles.
#'
#' @param input_data un tableau de données avec une colonne au format date ou datetime
#' @param group_target colonne de regroupement
#' @param target_var variables target pour les prévisions
#' @param date_var nom de la colonne date
#' @param future_forecast temps futur pour les prévisions
#' @param models_list liste des modèles de prévision
#'
#' @return forecast_result - renvoie un tableau de données contenant des informations sur les prévisions
#' @export
#' @examples
th2_bulk_forecasting_spark <- function(input_data, group_target, target_var, date_var, future_forecast, models_list, train_split = NULL, external_data = NULL, exogenous_var = NULL, use_holidays = NULL, country_column = NULL, lags = FALSE, path_driver = NULL, use_meteo = NULL, spark_conection = NULL) {

  # allow_par <- FALSE
  # if (!is.null(spark_conection)) {
  #   allow_par <- TRUE
  #   modeltime::parallel_start(spark_conection, .method = "spark")
  # }

  dataframe_input <- input_data

  data_clean <- preprocessing_data(dataframe_input %>% dplyr::select(target_var, date_var))[["dataset_clean"]]

  dataframe_input[[target_var]] <- data_clean[[target_var]]


  list_output_models <- list()

  if (!is.null(train_split)) {
    train_data <- dataframe_input %>%
      dplyr::filter(dataframe_input[[date_var]] < as.Date(train_split))

    test_data <- dataframe_input %>%
      dplyr::filter(dataframe_input[[date_var]] >= as.Date(train_split))

    # test_data <- test_data %>%
    #   dplyr::group_by(test_data[[date_var]]) %>%
    #   dplyr::summarise(count = dplyr::n())

    future_forecast <- nrow(test_data)
  } else {
    train_data <- dataframe_input
  }

  db_conn <- NULL
  if (!is.null(use_holidays)) {
    db_conn <- th2product::connect_to_database(
      path_driver = path_driver
    )
  }

  group_target_output <- group_target
  if (group_target == "all_columns") {
    train_data <- train_data %>%
      tidyr::pivot_longer(!date_var, names_to = "id", values_to = target_var) %>%
      rename(date := !!date_var)
    group_target <- "id"
  } else {
    if (!is.null(country_column) && (use_holidays == "in_data")) {
      select_vars <- c(group_target, date_var, target_var, country_column)
    } else {
      select_vars <- c(group_target, date_var, target_var)
    }

    train_data <- train_data %>%
      dplyr::select(all_of(select_vars)) %>%
      rename(id := !!group_target, date := !!date_var)
    group_target <- "id"
    # data_tbl <- data_tbl %>%
    #   mutate(id_group = paste(store_nbr, family, sep = "_"))
  }

  if (!is.null(country_column) && use_holidays == "in_data") {
    train_data <- train_data %>%
      dplyr::mutate(!!group_target := paste(train_data[[group_target]], "_", train_data[[country_column]], sep = ""))

    train_data <- train_data %>%
      dplyr::group_by_at(vars("date", group_target, country_column)) %>%
      dplyr::summarise_at(vars(target_var), sum)
  } else {
    train_data <- train_data %>%
      dplyr::group_by_at(vars("date", group_target)) %>%
      dplyr::summarise_at(vars(target_var), sum)
  }

  train_data <- train_data %>%
    dplyr::group_by_at(vars("date", group_target)) %>%
    dplyr::summarise_at(vars(target_var), sum)


  count_data <- table(train_data[[group_target]])
  column_value <- as.numeric(count_data[1])
  num_ids <- length(names(count_data[count_data > 1]))

  train_size <- round((nrow(train_data) / num_ids) * 0.8)
  test_size <- column_value - train_size

  dataset_train_test <- th2forecast::split_dataset(input_data = train_data, var_time = date_var, var_target = target_var, assess = test_size)[["traintest"]]

  error_models <- list()

  for (model in models_list) {
    if (model == "arima") {
      model_arima <- th2forecast::th2_arima_engine(rsample::training(dataset_train_test), target_var, date_var, fit_model = TRUE)
      list_output_models[["model_arima"]] <- model_arima
    } else if (model == "prophet") {
      model_prophet <- th2forecast::th2_prophet_engine(rsample::training(dataset_train_test), target_var, date_var, fit_model = TRUE, use_holidays = NULL)
      list_output_models[["model_prophet"]] <- model_prophet
    } else if (model == "lr") {
      model_lm <- th2_linear_engine(input_data, var_target, var_date)
      list_output_models[["model_lm"]] <- model_lm
    } else if (model == "mars") {
      model_mars <- th2_mars_engine(input_data, var_target, var_date)
      list_output_models[["model_mars"]] <- model_mars
    } else if (model == "random_forest") {
      # model_random_forest <- th2_random_forest_engine(input_data, var_target, 200)
      model_random_forest <- th2forecast::th2_random_forest_engine(rsample::training(dataset_train_test) %>% dplyr::select(- group_target), target_var, use_holidays = NULL, fit_model = TRUE)
      list_output_models[["model_random_forest"]] <- model_random_forest
    } else if (model == "xgboost") {
      # model_xgboost <- th2_xgboost_engine(input_data, var_date, var_target, 15)
      model_xgboost <- th2forecast::th2_xgboost_engine(rsample::training(dataset_train_test) %>% dplyr::select(- group_target), date_var, target_var, use_holidays = NULL, fit_model = TRUE)
      list_output_models[["model_xgboost"]] <- model_xgboost
    } else {
      error_models <- c(error_models, model)
    }
  }

  if (length(error_models) > 0) {
    return(warning(paste("Models not found:", error_models)))
  }


  models_trained <- do.call(modeltime_table, list_output_models)

  models_evaluated <- model_evaluation(dataset_train_test, models_trained)
  table_performance <- models_evaluated$accuracy_models
  models_evaluated <- models_evaluated$model_calibrated

  # print(models_evaluated %>%
  #   modeltime_forecast(
  #     new_data    = rsample::testing(dataset_train_test),
  #     actual_data = rsample::training(dataset_train_test)
  #   ) %>%
  #   plot_modeltime_forecast(
  #     .legend_max_width = 25,
  #     .interactive      = TRUE
  #   ))
  #
  print(table_performance %>%
          table_modeltime_accuracy(
            .interactive = FALSE
          ))

  df_models_evaluated <- models_evaluated %>%
    modeltime::modeltime_forecast(
      new_data = rsample::testing(dataset_train_test)
    )

  model_refit <- models_evaluated %>%
    modeltime::modeltime_refit(data = train_data)

  models_predictions <- model_refit %>%
    modeltime::modeltime_forecast(
      # actual_data = input_data,
      new_data = test_data
    )

  models_predictions$.model_desc <- ifelse(grepl("ARIMA", models_predictions$.model_desc), "ARIMA", models_predictions$.model_desc)

  models_predictions <- models_predictions %>%
    dplyr::mutate(as_of = Sys.Date()) %>%
    dplyr::mutate(start_date = min(input_data[[date_var]])) %>%
    dplyr::mutate(end_date = max(input_data[[date_var]])) %>%
    dplyr::mutate(accuracy = list(table_performance))


  return(models_predictions)
}
