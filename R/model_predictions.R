#' Prédiction par le Forcasting
#'
#' @param input_data dataframe: ensemble de données complet
#' @param model object: modèle calibré
#' @param h nombre de temps pour la prédiction
#'
#' @import modeltime
#' @import tidymodels
#'
#' @return prédiction pour l'avenir.
#' @export
#'
#' @examples prediction_forecast(input_data, calib_tbl, h = "3 months")
prediction_forecast <- function(input_data, model, h = "3 months"){
  prediction_forecast_tbl <- model %>%
    modeltime_refit(data = input_data) %>%
    modeltime_forecast(
      h = h,
      actual_data = input_data
    )

  return(prediction_forecast_tbl)
}
