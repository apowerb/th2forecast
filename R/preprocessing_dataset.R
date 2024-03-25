#' Prétraitement d'une Dataset
#'
#' Une fonction pour nettoyer les données (suppression des valeurs manquantes, détection des valeurs redondantes et analyse des anomalies).
#'
#' @param input_data un dataframe
#'
#' @return la fonction renvoie un dataset propre
#'
#' @import naniar
#' @import janitor
#'
#' @export
#'
#' @examples
#' preprocessing_data(input_data)
preprocessing_data <- function(input_data){

  if (is.data.frame(input_data))
  {

    if( nrow(input_data) == 0 || ncol(input_data) == 0  ){
      return(warning("The *input_date* variable is empty."))
    }

    input_data <- clean_names(input_data)
    input_data <- na.omit(input_data)
    input_data <- remove_empty(input_data)

    output_data <- unique(input_data)

    number_miss <- n_miss(output_data)
    percent_miss <- prop_miss(output_data)

    number_complet <- n_complete(output_data)
    percent_complet <- prop_complete(output_data)

    detail_missing <- miss_var_summary(output_data)

    list("dataset_clean"=output_data, "numnber_missing" = number_miss)

  }else{
    return(warning("The *input_date* variable is not a data.frame ."))
  }
}
