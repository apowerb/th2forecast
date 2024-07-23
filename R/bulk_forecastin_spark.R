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
  allow_par <- FALSE
  if (!is.null(spark_conection)) {
    allow_par <- TRUE
    modeltime::parallel_start(spark_conection, .method = "spark")
  }

  data_clean <- preprocessing_data(input_data %>% dplyr::select(target_var, date_var))[["dataset_clean"]]

  input_data[[target_var]] <- data_clean[[target_var]]


  list_output_models <- list()

  if (!is.null(train_split)) {
    train_data <- input_data %>%
      dplyr::filter(input_data[[date_var]] < as.Date(train_split))

    test_data <- input_data %>%
      dplyr::filter(input_data[[date_var]] >= as.Date(train_split))

    # test_data <- test_data %>%
    #   dplyr::group_by(test_data[[date_var]]) %>%
    #   dplyr::summarise(count = dplyr::n())

    future_forecast <- nrow(test_data)
  } else {
    train_data <- input_data
  }

  db_conn <- NULL

  # group_target_output <- group_target
  # if (group_target == "all_columns") {
  #   data_tbl <- train_data %>%
  #     tidyr::pivot_longer(!date_var, names_to = "id", values_to = target_var) %>%
  #     rename(date := !!date_var)
  #   group_target <- "id"
  # } else {
  #   if (!is.null(country_column) && (use_holidays == "in_data")) {
  #     select_vars <- c(group_target, date_var, target_var, country_column)
  #   } else {
  #     select_vars <- c(group_target, date_var, target_var)
  #   }
  #
  #   data_tbl <- train_data %>%
  #     dplyr::select(all_of(select_vars)) %>%
  #     rename(id := !!group_target, date := !!date_var)
  #   group_target <- "id"
  #   # data_tbl <- data_tbl %>%
  #   #   mutate(id_group = paste(store_nbr, family, sep = "_"))
  # }

  train_data <- train_data %>%
    dplyr::group_by_at(vars("date", group_target)) %>%
    dplyr::summarise_at(vars(target_var), sum)


  count_data <- table(train_data[[group_target]])
  column_value <- as.numeric(count_data[1])
  num_ids <- length(names(count_data[count_data > 1]))

  train_size <- round((nrow(train_data) / num_ids) * 0.8)
  test_size <- column_value - train_size

  dataset_train_test <- th2forecast::split_dataset(input_data = train_data, var_time = date_var, var_target = target_var, assess = test_size)[["traintest"]]

  models_trained <- th2forecast::th2_prophet_engine(rsample::training(dataset_train_test), target_var, date_var, fit_model = TRUE, use_holidays = NULL)

  models_evaluated <- model_evaluation(dataset_train_test, models_trained)
  table_performance <- models_evaluated$accuracy_models
  models_evaluated <- models_evaluated$model_calibrated

  df_models_evaluated <- models_evaluated %>%
    modeltime::modeltime_forecast(
      new_data = rsample::testing(dataset_train_test)
    )

  models_predictions <- models_evaluated %>%
    modeltime::modeltime_forecast(
      new_data = test_data
    )

  # modeltime::parallel_stop()

  return(models_predictions)


}
