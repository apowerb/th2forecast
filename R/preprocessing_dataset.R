#' Détection et correction des anomalies
#'
#' @param input_data Dataframe
#' @param input_alpha numeric - controls the width of the "normal" range
#' @param max_anoms numeric - pourcentage maximum d'anomalies pouvant être identifiées.
#'
#' @return la même input_data mais sans anomalies
#'
#' @import anomalize
#'
#' @export
#'
#' @examples
anomaly_detection <- function(input_data, input_alpha = 0.05, max_anoms = 0.2){

  if (is.data.frame(input_data))
  {
    if( nrow(input_data) == 0 || ncol(input_data) == 0  ){
      return(warning("The *input_date* variable is empty."))
    }

    date_variable <- sapply(input_data, function(x) inherits(x, "Date") || inherits(x, "POSIXct") )
    var_date_feature <- colnames(input_data[, date_variable])

    filter_targets <- colnames(input_data %>% select(-all_of(var_date_feature)))

    for (variable in filter_targets) {

      col_clean <- input_data %>%
        anomalize::time_decompose(variable, merge = TRUE, message = FALSE) %>%
        anomalize::anomalize(remainder, alpha = input_alpha, max_anoms = max_anoms) %>%
        anomalize::clean_anomalies()

      input_data[variable] <- col_clean["observed_cleaned"]
    }

    return(input_data)
  }else{
    return(warning("The *input_date* variable is not a data.frame ."))
  }

}


#' Détection et correction du levels shift
#'
#' @param input_data Dataframe
#' @param method_ls la méthode du package à utiliser
#'
#' @returnla le même input_data mais avec des corrections du levels shift
#'
#' @import tsoutliers
#' @import changepoint
#'
#' @export
#'
#' @examples
outliers_detection <- function(input_data, method_ls = "cpt"){
  if (is.data.frame(input_data))
  {

    if( nrow(input_data) == 0 || ncol(input_data) == 0  ){
      return(warning("The *input_date* variable is empty."))
    }

    date_variable <- sapply(input_data, function(x) inherits(x, "Date") || inherits(x, "POSIXct") )
    var_date_feature <- colnames(input_data[, date_variable])

    filter_targets <- colnames(input_data %>% select(-all_of(var_date_feature)))

    for (variable in filter_targets) {

      y <- ts(unlist(input_data[variable]))

      if (method_ls == "tso")
      {
        resul_out <- tso(y, xreg = NULL, cval = 3.5, delta = 0.7,
                         # types = c("AO", "LS", "TC"),
                         types = c("LS"),
                         maxit = 1, maxit.iloop = 4, maxit.oloop = 4, cval.reduce = 0.14286,
                         discard.method = c("en-masse", "bottom-up"), discard.cval = NULL,
                         # discard.method = c("bottom-up"), discard.cval = NULL,
                         tsmethod = c("auto.arima", "arima"),
                         # tsmethod = c("auto.arima"),
                         args.tsmethod = NULL, logfile = NULL, check.rank = FALSE)

        input_data[variable] <- resul_out$yadj
      }else if(method_ls == "cpt")
      {

        cpt <- cpt.meanvar(y)

        indices_cpt <- c(0, cpts(cpt), length(y))

        serie_corregida <- numeric(length(y))

        for (i in 1:(length(indices_cpt) - 1)) {
          segmento <- y[(indices_cpt[i] + 1):indices_cpt[i + 1]]
          serie_corregida[(indices_cpt[i] + 1):indices_cpt[i + 1]] <- segmento - mean(segmento)
        }

        input_data[variable] <- serie_corregida
      }else{
        return(warning("The selected *method* does not exist."))
      }


    }

    return(input_data)
  }else{
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

    output_data <- anomaly_detection(output_data, input_alpha = 0.05, max_anoms = 0.2)

    output_data <- outliers_detection(output_data, method_ls = "cpt")

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
