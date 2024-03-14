#' Extraction des caractéristiques
#'
#' Une fonction permettant d'extraire les caractéristiques les plus pertinentes.
#'
#' @param dataset un dataframe
#'
#' @return a fonction renvoie un dataset avec les caractéristiques importantes
#' @import caret
#' @export
#'
#' @examples
#' feature_selection(input_data)
feature_selection <- function(input_data){

  remove_cols <- nearZeroVar(input_data, names = TRUE, freqCut = 2, uniqueCut = 20)

  all_cols <- names(input_data)
  data_features <- input_data[, setdiff(all_cols, remove_cols)]

  return(data_features)

}
