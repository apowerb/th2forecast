#' Calibre et évalue une liste de modèles
#'
#' Une fonction qui prend une liste de modèles entraînés à calibrer avec les
#' données de test et qui calcule ensuite les différentes mesures d'évaluation.
#'
#' @param input_data ensemble de données de test
#' @param model_table liste des modèles entraînés
#'
#' @import modeltime
#' @import tidymodels
#'
#' @return une liste avec 2 éléments : modèles calibrés - métriques d'évaluation
#' @export
#'
#' @examples model_evaluation(input_data, model_tbl)
model_evaluation <- function(input_data, model_table) {

  calib_tbl <- model_table %>%
    modeltime_calibrate(testing(input_data), quiet = FALSE)

  accuracy_model <- calib_tbl %>%
    modeltime_accuracy(metric_set = metric_set(mae, rmse, rsq))

  list("model_calibrated" = calib_tbl, "accuracy_models" = accuracy_model)
}
