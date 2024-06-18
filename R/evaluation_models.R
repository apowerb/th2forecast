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
    modeltime::modeltime_calibrate(testing(input_data), quiet = FALSE)

  accuracy_model <- calib_tbl %>%
    modeltime::modeltime_accuracy(metric_set = yardstick::metric_set(yardstick::mae, yardstick::rmse, yardstick::rsq))

  list("model_calibrated" = calib_tbl, "accuracy_models" = accuracy_model)
}


#' @export
th2_benchmarking <- function(test_data, forecasting_data, group_target = NULL, group_value = NULL ){

  list_models <- unique(forecasting_data$.model_desc)

  if (!is.null(group_value)) {
    forecasting_data <- forecasting_data %>%
      dplyr::filter(forecasting_data[[group_target]] == !!group_value)
  }

  df_accuracy_test <- tibble(
    id = numeric(),
    .model_desc = character(),
    .type = character(),
    mae = numeric(),
    rmse = numeric()
  )

  for(model in list_models){
    add_model <- tibble(
      id = group_value,
      .model_desc = model,
      .type = "Validation",
      mae = yardstick::mae_vec(as.double(unlist(test_data)) ,  as.double( forecasting_data %>%
                                                                            dplyr::filter(forecasting_data$.model_desc == model) %>%
                                                                            dplyr::pull(.value))),
      rsq = yardstick::rmse_vec(as.double(unlist(test_data)) ,  as.double( forecasting_data %>%
                                                                             dplyr::filter(forecasting_data$.model_desc == model) %>%
                                                                             dplyr::pull(.value)))
    )

    df_accuracy_test <- rbind(df_accuracy_test, add_model)
  }

  print(df_accuracy_test)



}
