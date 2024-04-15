#' Générer des ensembles de données d'entraînement et de test
#'
#' @param input_data un dataframe
#' @param var_time une caractères contenant le nom de la variable de type date de l'ensemble de données
#' @param var_target une caractères contenant le nom de la variable de target de l'ensemble de données
#' @param assess nombre d'échantillons utilisés pour chaque rééchantillonnage d'évaluation.
#'
#' @return renvoie une liste avec 2 objets: 1. un dataframe avec l'ensemble de training et de test  2. un dataframe avec les 2 variables sélectionnées.
#' @export
#'
#' @examples
#' split_dataset(input_data, "dteday", "cnt"; assess="3 months")
split_dataset <- function(input_data, var_time, var_target, assess = "3 months" ){

  if (is.data.frame(input_data))
  {

    if( nrow(input_data) == 0 || ncol(input_data) == 0  ){
      return(warning("The *input_date* variable is empty"))
    }else{
      if(nrow(input_data) < 5){
        return(warning("The rows of the dataset are very few"))
      }
    }

    if (!(var_time %in% colnames(input_data) && var_target %in% colnames(input_data))){
      return(warning("Selected variables do not exist"))
    }

    output_data <- input_data

    train_size <- round(dim(output_data)[1] * 0.8)
    test_size <- dim(output_data)[1] - train_size

    data_train_test <- timetk::time_series_split(output_data, initial = train_size, assess = test_size, cumulative = TRUE)

    list("traintest"= data_train_test, "data_selected"= output_data)
  }else{
    return(warning("The *input_date* variable is not a data.frame"))
  }
}
