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
    # colnames(input_data)[...] and not colnames(input_data[, ...]): with a single
    # date column, a base data.frame drops to a vector and loses its name.
    var_date_feature <- colnames(input_data)[date_variable]

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

        # An isolated spike is itself a change in mean and variance, so
        # cpt.meanvar tends to cut it into its own tiny segment, where it can
        # never be 3 sd away from the mean: in a segment of n points no value
        # exceeds (n - 1) / sqrt(n) sd, which stays below 3 up to n = 10.
        # Short segments are merged into a neighbour first.
        indexes_cpt <- c(1, .merge_short_segments(length(y), cpt@cpts), length(y))

        fixed_series <- as.numeric(y)

        # changepoint::cpt.meanvar decoupe la serie en segments homogenes en
        # moyenne/variance : a l'interieur de chaque segment, les points qui
        # s'ecartent de plus de 3 ecarts-types de la moyenne du segment sont
        # des outliers et sont ramenes a cette moyenne. Auparavant ce bloc
        # recopiait le segment tel quel dans fixed_series : aucune valeur
        # n'etait jamais corrigee (bug, cf. test-preprocessing_dataset.R).
        for (i in 1:(length(indexes_cpt) - 1)) {
          idx <- (indexes_cpt[i]):indexes_cpt[i + 1]
          segment <- as.numeric(y[idx])
          segment_mean <- mean(segment)
          segment_sd <- stats::sd(segment)

          if (!is.na(segment_sd) && segment_sd > 0) {
            is_outlier <- abs(segment - segment_mean) > 3 * segment_sd
            segment[is_outlier] <- segment_mean
          }

          fixed_series[idx] <- segment
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


#' Merges changepoint segments too short for the 3 sd outlier test
#'
#' In a segment of n points, no value can be more than (n - 1) / sqrt(n)
#' standard deviations from the segment mean, which stays below 3 up to
#' n = 10. Changepoints are dropped, shortest segment first, until every
#' segment has at least `min_length` points; the boundary removed is the one
#' shared with the shorter neighbour.
#'
#' @param n series length
#' @param cpts changepoint positions (as in `cpt@cpts`, which ends with `n`)
#' @param min_length minimum segment length
#' @return the remaining changepoints, without `n`
#' @keywords internal
.merge_short_segments <- function(n, cpts, min_length = 11L) {
  cpts <- as.integer(cpts[cpts < n])
  repeat {
    bounds <- c(0L, cpts, as.integer(n))
    lengths <- diff(bounds)
    if (length(cpts) == 0 || all(lengths >= min_length)) break
    k <- which.min(lengths)
    drop <- if (k == 1) {
      1L
    } else if (k == length(lengths)) {
      length(cpts)
    } else if (lengths[k - 1] <= lengths[k + 1]) {
      k - 1L
    } else {
      k
    }
    cpts <- cpts[-drop]
  }
  cpts
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
    # Fetched once per process, with a timeout (see th2_fetch_holidays_fr()).
    list_holidays <- th2_fetch_holidays_fr(region)
  } else {
    # tryCatch() renvoie la valeur de son bloc (succes) ou celle du handler
    # `error` (echec) : il faut recuperer ce resultat dans `list_holidays`.
    # Auparavant le retour du tryCatch etait jete (appel en instruction nue) et
    # le handler d'erreur se contentait d'un print() + return(NULL) qui ne
    # sort que de la closure du handler, pas de holidays_detection() : en cas
    # d'echec (ex. pas de connexion DB), `list_holidays` n'etait alors jamais
    # defini et le `names(list_holidays)` plus bas plantait avec
    # "object 'list_holidays' not found" au lieu de degrader proprement vers
    # une liste de jours feries vide.
    list_holidays <- tryCatch(
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
        result_holidays <- list()

        for (i in 1:nrow(calendar_country_bh)) {
          result_holidays[calendar_country_bh[i, "_date"]] <- calendar_country_bh[i, "_name"]
        }

        result_holidays
      },
      error = function(error) {
        print(error)
        print("Error returning output business holidays. Please check the input datasource configuration.")
        list()
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
    input_data <- input_data %>%
      janitor::clean_names() %>%
      na.omit() %>%
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
