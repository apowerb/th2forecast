th_tsfeatures <- function(ts_movil) {
  ts <- ts(ts_movil)
  features <- tsfeatures(ts)
  features <- features %>% dplyr::select( c("trend", "spike", "linearity", "curvature", "e_acf1", "entropy"))
  return(features)
}

#' Extraction des caractéristiques
#'
#' Une fonction permettant d'extraire les caractéristiques les plus pertinentes.
#'
#' @param dataset un dataframe
#'
#' @return a fonction renvoie un dataset avec les caractéristiques importantes
#' @import caret
#' @import tsfeatures
#' @import zoo
#' @export
#'
#' @examples
#' feature_selection(input_data)
feature_selection <- function(input_data, feature_target, list_features = ""){

  if (is.data.frame(input_data))
  {

    if( nrow(input_data) == 0 || ncol(input_data) == 0  ){
      return(warning("The *input_date* variable is empty"))
    }

    # remove_cols <- nearZeroVar(input_data, names = TRUE, freqCut = 2, uniqueCut = 20)
    #
    # all_cols <- names(input_data)
    # data_features <- input_data[, setdiff(all_cols, remove_cols)]

    column_date <- sapply(input_data, function(x) inherits(x, "Date") || inherits(x, "POSIXct"))
    var_date_feature <- colnames(input_data[, column_date])
    #
    # all_cols <- names(input_data)
    #
    # data_features <- input_data[, setdiff(all_cols, var_date_feature)]
    #
    # data_desc = decompose(ts(data_features$meantemp, start = c(2013, 1), frequency = 360))
    # plot(data_desc, xlab='Year')
    #
    # ts_features <- tsfeatures(data_features, features = c("frequency", "stl_features", "entropy", "acf_features"))
    #
    # view(ts_features)

    lags = 5

    list_features <- c(var_date_feature,list_features, feature_target)
    data_features <- dplyr::select(input_data, list_features)

    data_features["month"]  <- month(data_features[[var_date_feature]])

    if ( inherits(data_features[[var_date_feature]], "POSIXct")  )
    {
      data_features["hour"]  <- hour(data_features[[var_date_feature]])
    }
    data_features["dayofweek"] <- wday(data_features[[var_date_feature]])


    data_features["weekend"] <- ifelse(data_features$dayofweek %in% c(1, 7), 1, 0)

    print(data_features[feature_target])

    for(i in 1:lags) {
      data_features[paste("lag_", i, sep = "")] <- lag(data_features[feature_target], i)
    }

    window <- 4

    data_features["rolling_mean"] <- rollmean(input_data[feature_target], window, na.pad = TRUE)
    data_features["rolling_std"] <- rollapply(input_data[feature_target], window, sd, na.pad = TRUE)
    # data_features$rolling_std <- rollapply(input_data$meantemp, window, tsfeatures, na.pad = TRUE)
    # data_features[[var_date_feature]] <- as.Date(data_features[[var_date_feature]])



    st_features <- rollapply(input_data[feature_target], width = 10, FUN = th_tsfeatures, by.column = FALSE, na.pad = TRUE)


    data_features <- cbind(data_features, st_features)

    # ts_features <- tsfeatures(data_features.T)

    return(data_features)
  }else
  {
    return(warning("The *input_date* variable is not a data.frame"))
  }

}
