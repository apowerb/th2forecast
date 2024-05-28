#' Détection et correction des anomalies
#'
#' @param input_data Dataframe
#' @param input_alpha numeric - controls the width of the "normal" range
#' @param max_anoms numeric - pourcentage maximum d'anomalies pouvant être identifiées.
#'
#' @return la même input_data mais sans anomalies
#'
#' @export
#'
#' @examples
anomaly_detection <- function(input_data, input_alpha = 0.05, max_anoms = 0.2) {
  if (is.data.frame(input_data)) {
    if (nrow(input_data) == 0 || ncol(input_data) == 0) {
      return(warning("The *input_date* variable is empty."))
    }

    date_variable <- sapply(input_data, function(x) inherits(x, "Date") || inherits(x, "POSIXct"))
    var_date_feature <- colnames(input_data[, date_variable])

    filter_targets <- colnames(input_data %>% dplyr::select(-all_of(var_date_feature)))

    for (variable in filter_targets) {
      col_clean <- input_data %>%
        anomalize::time_decompose(variable, merge = TRUE, message = FALSE) %>%
        anomalize::anomalize(remainder, alpha = input_alpha, max_anoms = max_anoms) %>%
        anomalize::clean_anomalies()

      input_data[variable] <- col_clean["observed_cleaned"]
    }

    return(input_data)
  } else {
    return(warning("The *input_date* variable is not a data.frame ."))
  }
}


#' Détection et correction du levels shift
#'
#' @param input_data Dataframe
#' @param method_ls la méthode du package à utiliser
#'
#' @return return le même input_data mais avec des corrections du levels shift
#'
#' @export
#'
#' @examples
outliers_detection <- function(input_data, method_ls = "cpt") {
  if (is.data.frame(input_data)) {
    if (nrow(input_data) == 0 || ncol(input_data) == 0) {
      return(warning("The *input_date* variable is empty."))
    }

    date_variable <- sapply(input_data, function(x) inherits(x, "Date") || inherits(x, "POSIXct"))
    var_date_feature <- colnames(input_data[, date_variable])

    filter_targets <- colnames(input_data %>% dplyr::select(-all_of(var_date_feature)))

    for (variable in filter_targets) {
      y <- ts(unlist(input_data[variable]))

      if (method_ls == "tso") {
        resul_out <- tsoutliers::tso(y,
          xreg = NULL, cval = 3.5, delta = 0.7,
          # types = c("AO", "LS", "TC"),
          types = c("LS"),
          maxit = 1, maxit.iloop = 4, maxit.oloop = 4, cval.reduce = 0.14286,
          discard.method = c("en-masse", "bottom-up"), discard.cval = NULL,
          # discard.method = c("bottom-up"), discard.cval = NULL,
          tsmethod = c("auto.arima", "arima"),
          # tsmethod = c("auto.arima"),
          args.tsmethod = NULL, logfile = NULL, check.rank = FALSE
        )

        input_data[variable] <- resul_out$yadj
      } else if (method_ls == "cpt") {
        cpt <- changepoint::cpt.meanvar(y)

        indexes_cpt <- c(1, cpt@cpts, length(y))

        fixed_series <- numeric(length(y))

        for (i in 1:(length(indexes_cpt) - 1)) {
          segment <- y[(indexes_cpt[i]):indexes_cpt[i + 1]]
          fixed_series[(indexes_cpt[i]):indexes_cpt[i + 1]] <- segment
        }

        input_data[variable] <- fixed_series
      } else {
        return(warning("The selected *method* does not exist."))
      }
    }
    return(input_data)
  } else {
    return(warning("The *input_date* variable is not a data.frame ."))
  }
}


#' Prétraitement d'une Dataset
#'
#' Une fonction pour nettoyer les données (suppression des valeurs manquantes, détection des valeurs redondantes et analyse des anomalies).
#'
#' @param input_data un dataframe
#'
#' @return la fonction renvoie un dataset propre
#'
#' @export
#'
#' @examples
#' preprocessing_data(input_data)
preprocessing_data <- function(input_data){
  if (is.data.frame(input_data) || is.list(input_data))
  {
    if(is.list(input_data)){
      input_data <- as.data.frame(input_data)
      input_data <- as_tibble(input_data)
    }
    if (nrow(input_data) == 0 || ncol(input_data) == 0) {
      return(warning("The *input_date* variable is empty."))
    }
    input_data <- janitor::clean_names(input_data)
    input_data <- na.omit(input_data)
    input_data <- janitor::remove_empty(input_data)
    output_data <- unique(input_data)
    output_data <- anomaly_detection(output_data, input_alpha = 0.05, max_anoms = 0.2)
    output_data <- outliers_detection(output_data, method_ls = "cpt")
  } else {

    number_miss <- naniar::n_miss(output_data)

    percent_miss <- naniar::prop_miss(output_data)

    number_complet <- naniar::n_complete(output_data)

    percent_complet <- naniar::prop_complete(output_data)

    detail_missing <- naniar::miss_var_summary(output_data)

    list("dataset_clean"=output_data, "numnber_missing" = number_miss)

  }else{
    return(warning("The *input_date* variable is not a data.frame ."))
  }
}
