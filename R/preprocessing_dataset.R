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

    date_min <- input_data %>%
      dplyr::select(!!var_date_feature) %>%
      dplyr::summarise(min_date = min(input_data[[var_date_feature]])) %>%
      dplyr::mutate(year = lubridate::year(min_date), month = lubridate::month(min_date))

    for (variable in filter_targets) {
      y <- ts(input_data[[variable]], start = c(date_min$year, date_min$month), frequency = 365)

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

        # cpt <- fastcpd::fastcpd.meanvariance(input_data[[variable]])
        #
        # cpt@residuals <- (cpt@residuals + mean(cpt@data$x))

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


#' Sélection des jours fériés/holidays
#'
#' @param input_data Dataframe
#' @param model modèle qui requiert l'information
#' @param calendar calendrier à utiliser
#' @param region champ spécifique pour la France, possibilité de choisir une région
#'
#' @return liste des jours fériés
#' @export
#'
#' @examples
holidays_detection <- function(input_data, model, calendar = "calendar_france", region = "metropole", db_conn = NULL) {
  date_variable <- sapply(input_data, function(x) inherits(x, "Date") || inherits(x, "POSIXct"))
  var_date_feature <- colnames(input_data[, date_variable])

  if (calendar == "calendar_france") {
    url <- paste0("https://calendrier.api.gouv.fr/jours-feries/", region, ".json")

    holidays_req <- httr2::request(base_url = url) %>%
      httr2::req_method("GET")

    holidays_resp <- holidays_req %>%
      httr2::req_perform(verbosity = 0)

    list_holidays <- holidays_resp %>%
      httr2::resp_body_json()
  } else {
    tryCatch(
      {
        # list_holidays_years <- list()
        #
        # mim_date <- lubridate::year(min(input_data[[var_date_feature]]))
        #
        # list_years <- c(as.integer(mim_date) : (lubridate::year(lubridate::now()) + 1))

        # for (year in list_years) {
        #   url <- paste0("https://date.nager.at/api/v3/PublicHolidays/",year,"/", calendar)
        #   holidays_req <- httr2::request(base_url = url) %>%
        #     httr2::req_method("GET")
        #
        #   holidays_resp <- holidays_req %>%
        #     httr2::req_perform(verbosity = 0)
        #
        #   list_holidays <- holidays_resp %>%
        #     httr2::resp_body_json()
        #
        #   list_holidays_years <- c(list_holidays_years, list_holidays)
        # }
        calendar_country_bh <- calendars_businness_days(db_conn = db_conn, country_code = calendar)
        list_holidays <- list()

        for (i in 1:nrow(calendar_country_bh)) {
          list_holidays[calendar_country_bh[i, "_date"]] <- calendar_country_bh[i, "_name"]
        }
      },
      error = function(error) {
        print(error)
        print("Error returning output business holidays. Please check the input datasource configuration.")
        return(NULL)
      }
    )
  }

  if (model == "ml") {
    holidays <- names(list_holidays)

    bizdays::create.calendar(
      name = calendar, holidays = holidays, weekdays = c("sunday", "saturday")
    )

    bizdays::bizdays.options$set(default.calendar = calendar)

    holidays <- !(bizdays::is.bizday(input_data[[var_date_feature]]))
    holidays <- holidays %>% as.integer()

    return(holidays)
  } else {
    values <- c()

    for (i in list_holidays) {
      values <- c(values, i)
    }

    dataframe_holidays <- tibble::tibble(
      holiday = values,
      ds = as.Date(names(list_holidays)),
      lower_window = 0,
      upper_window = 1
    )

    return(dataframe_holidays)
  }
}


#' @export
meteo_feature <- function(input_data, region = NULL, temperature_unit = "celsius") {
  date_variable <- sapply(input_data, function(x) inherits(x, "Date") || inherits(x, "POSIXct"))
  var_date_feature <- colnames(input_data[, date_variable])

  min_date <- min(input_data[[var_date_feature]])
  max_date <- max(input_data[[var_date_feature]])

  df_meteo <- data.frame()

  co_ordinate <- openmeteo::geocode(region)
  co_ordinate <- c(co_ordinate$latitude, co_ordinate$longitude)

  if (min_date > as.Date(lubridate::now())) {
    temp_result <- openmeteo::climate_forecast(
      co_ordinate,
      min_date,
      max_date,
      daily = "temperature_2m_max",
      model = "MPI_ESM1_2_XR",
      response_units = list(temperature_unit = temperature_unit)
    )

    precipitation_result <- openmeteo::climate_forecast(
      co_ordinate,
      min_date,
      max_date,
      daily = "precipitation_sum",
      model = "MPI_ESM1_2_XR",
      response_units = list(precipitation_unit = "mm")
    )
  } else {
    temp_result <- openmeteo::weather_history(
      co_ordinate,
      start = min_date,
      end = max_date,
      daily = "temperature_2m_max",
      response_units = list(temperature_unit = temperature_unit)
    )

    precipitation_result <- openmeteo::weather_history(
      co_ordinate,
      start = min_date,
      end = max_date,
      daily = "precipitation_sum",
      response_units = list(precipitation_unit = "mm")
    )
  }

  df_meteo <- cbind(temp_result, precipitation_result[2])

  return(df_meteo)
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
preprocessing_data <- function(input_data) {
  if (is.data.frame(input_data) || is.list(input_data)) {
    if (is.list(input_data)) {
      input_data <- as.data.frame(input_data)
      input_data <- tibble::as_tibble(input_data)
    }
    if (nrow(input_data) == 0 || ncol(input_data) == 0) {
      return(warning("The *input_date* variable is empty."))
    }
    input_data <- input_data%>%
      janitor::clean_names()%>%
      na.omit()%>%
      janitor::remove_empty(which = c("cols"))

    output_data <- unique(input_data)
    output_data <- anomaly_detection(output_data, input_alpha = 0.05, max_anoms = 0.2)
    output_data <- outliers_detection(output_data, method_ls = "cpt")
    number_miss <- naniar::n_miss(output_data)
    percent_miss <- naniar::prop_miss(output_data)
    number_complet <- naniar::n_complete(output_data)
    percent_complet <- naniar::prop_complete(output_data)
    detail_missing <- naniar::miss_var_summary(output_data)

    list("dataset_clean" = output_data, "numnber_missing" = number_miss)
  } else {
    return(warning("The *input_date* variable is not a data.frame ."))
  }
}
