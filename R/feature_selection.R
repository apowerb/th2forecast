#' Extraction des caractéristiques avec le package tsfeatures
#'
#' Une fonction permettant augmenter les caractéristiques
#'
#' @param ts_movil
#' @param features_input
#'
#' @return
#' @export
#'
#' @examples
th2_tsfeatures <- function(
    ts_movil,
    features_input = c("trend", "spike", "linearity", "curvature", "e_acf1", "entropy")) {
  ts <- ts(ts_movil)
  features <- tsfeatures::tsfeatures(ts)
  features <- features %>%
    dplyr::select(all_of(features_input))
  return(features)
}

#' Extraction des caractéristiques
#'
#' Une fonction permettant d'extraire les caractéristiques.
#'
#' @param dataset un dataframe
#'
#' @return a fonction renvoie un dataset avec les caractéristiques importantes
#' @export
#'
#' @examples
#' feature_selection(input_data)
feature_selection <- function(
    input_data,
    feature_target,
    list_features = c(),
    lags = 5,
    window = 5) {

  if (is.data.frame(input_data)) {

    if (nrow(input_data) == 0 || ncol(input_data) == 0) {
      return(warning("The *input_date* variable is empty"))
    }

    column_date <- sapply(input_data, function(x) inherits(x, "Date") || inherits(x, "POSIXct"))
    var_date_feature <- names(input_data)[column_date]

    lags <- lags

    if (length(list_features) > 0) {
      list_features <- c(var_date_feature, list_features, feature_target)
      data_features <- input_data %>% dplyr::select(all_of(list_features))
    }else {
      list_features <- c(var_date_feature, feature_target)
      data_features <- input_data %>% dplyr::select(all_of(list_features))
    }

    data_features["month"]  <- month(data_features[[var_date_feature]])

    if (inherits(data_features[[var_date_feature]], "POSIXct")) {
      data_features["hour"]  <- hour(data_features[[var_date_feature]])
    }

    data_features["dayofweek"] <- wday(data_features[[var_date_feature]])

    data_features["weekend"] <- ifelse(data_features$dayofweek %in% c(1, 7), 1, 0)

    for (i in 1:lags) {
      data_features[paste("lag_", i, sep = "")] <- lag(data_features[feature_target], i)
    }

    window <- window

    data_features["rolling_mean"] <- zoo::rollapplyr(input_data[feature_target], window, mean, fill = NA)
    data_features["rolling_std"] <- zoo::rollapplyr(input_data[feature_target], window, sd, fill = NA)

    st_features <- zoo::rollapplyr(input_data[feature_target], width = window, FUN = th2_tsfeatures, by.column = FALSE, fill = NA)

    data_features <- cbind(data_features, st_features)

    return(data_features)
  }else {
    return(warning("The *input_date* variable is not a data.frame"))
  }

}
