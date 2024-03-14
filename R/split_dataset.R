#' Générer des ensembles de données d'entraînement et de test
#'
#' @param input_data un dataframe
#' @param var_time une caractères contenant le nom de la variable de type date de l'ensemble de données
#' @param var_target une caractères contenant le nom de la variable de target de l'ensemble de données
#' @param assess nombre d'échantillons utilisés pour chaque rééchantillonnage d'évaluation.
#'
#' @return renvoie une liste avec 2 objets: 1. un dataframe avec l'ensemble de training et de test  2. un dataframe avec les 2 variables sélectionnées.
#' @import timetk
#' @import dplyr
#' @export
#'
#' @examples
#' split_dataset(input_data, "dteday", "cnt"; assess="3 months")
split_dataset <- function(input_data, var_time, var_target, assess = "3 months" ){

  if (!(var_time %in% colnames(input_data) && var_target %in%  colnames(input_data))){
    print("Les variables sélectionnées n'existent pas")
    stop()
  }

  output_data <- input_data %>% select(var_time, var_target)

  data_train_test <- time_series_split(output_data, assess = assess, cumulative = TRUE)

  list("traintest"= data_train_test, "data_selected"= output_data)
}
