#' @name api_v1_features
NULL

# Package features exposed through API v1 on top of the statistical models:
# preprocessing (anomalies, outliers), French public holidays, the
# machine-learning engines and an average ensemble (see docs/API.md).

ML_MODELS <- c("linear", "mars", "random_forest", "xgboost")
ENSEMBLE_MODEL <- "ensemble"
PREPROCESSING_OPTIONS <- c("anomalies", "outliers")
HOLIDAY_COUNTRIES <- c("FR")
HOLIDAYS_CALENDAR_FR <- "calendar_france"
HOLIDAYS_TIMEOUT_SECONDS <- 5

.th2_holidays_cache <- new.env(parent = emptyenv())

#' @keywords internal
.th2_holidays_cache_clear <- function() {
  rm(list = ls(.th2_holidays_cache), envir = .th2_holidays_cache)
}

#' @keywords internal
.th2_holidays_request <- function(region, timeout) {
  url <- paste0("https://calendrier.api.gouv.fr/jours-feries/", region, ".json")
  httr2::request(url) |>
    httr2::req_timeout(timeout) |>
    httr2::req_perform(verbosity = 0) |>
    httr2::resp_body_json()
}

#' French public holidays from calendrier.api.gouv.fr, cached per process
#'
#' The service returns every holiday over about twenty years in one call, so
#' a single fetch per region serves all later requests.
#'
#' @param region zone of the service ("metropole", "alsace-moselle", ...)
#' @param timeout seconds before giving up
#' @return named list: date (YYYY-MM-DD) -> holiday name
#' @export
th2_fetch_holidays_fr <- function(region = "metropole", timeout = HOLIDAYS_TIMEOUT_SECONDS) {
  cached <- .th2_holidays_cache[[region]]
  if (!is.null(cached)) return(cached)
  holidays <- .th2_holidays_request(region, timeout)
  assign(region, holidays, envir = .th2_holidays_cache)
  holidays
}

#' Validates `preprocessing`, `holidays_country` and the extra models
#'
#' @param body request body
#' @param add_error `add_error(field, message)` of the validator
#' @return list(preprocessing = <named logical or NULL>, holidays_country = <"FR" or NULL>)
#' @export
api_v1_parse_package_features <- function(body, add_error) {
  preprocessing <- NULL
  raw <- body[["preprocessing"]]
  if (!is.null(raw)) {
    if (!is.list(raw) || is.null(names(raw)) || any(!nzchar(names(raw)))) {
      add_error("preprocessing", sprintf(
        "'preprocessing' doit être un objet, par exemple {\"anomalies\": true}. Options : %s.",
        paste(PREPROCESSING_OPTIONS, collapse = ", ")
      ))
    } else {
      preprocessing <- stats::setNames(logical(0), character(0))
      for (key in names(raw)) {
        field <- paste0("preprocessing.", key)
        value <- raw[[key]]
        if (!(key %in% PREPROCESSING_OPTIONS)) {
          add_error(field, sprintf(
            "Option de prétraitement inconnue : '%s'. Options : %s.", key, paste(PREPROCESSING_OPTIONS, collapse = ", ")
          ))
        } else if (!is.logical(value) || length(value) != 1 || is.na(value)) {
          add_error(field, sprintf("'%s' doit valoir true ou false.", field))
        } else {
          preprocessing[[key]] <- value
        }
      }
      if (!any(preprocessing)) preprocessing <- NULL
    }
  }

  holidays_country <- body[["holidays_country"]]
  if (!is.null(holidays_country) && !(length(holidays_country) == 1 && is.na(holidays_country))) {
    country <- if (is.character(holidays_country) && length(holidays_country) == 1) toupper(holidays_country) else NA_character_
    if (is.na(country) || !(country %in% HOLIDAY_COUNTRIES)) {
      add_error("holidays_country", sprintf(
        "Pays de jours fériés non pris en charge : '%s'. Valeurs acceptées : %s (ou null).",
        paste(holidays_country, collapse = ", "), paste(HOLIDAY_COUNTRIES, collapse = ", ")
      ))
      holidays_country <- NULL
    } else {
      holidays_country <- country
    }
  } else {
    holidays_country <- NULL
  }

  list(preprocessing = preprocessing, holidays_country = holidays_country)
}

#' Resolves the holiday calendar to hand to the engines
#'
#' @param holidays_country "FR" or NULL
#' @return list(calendar = <calendar name or NULL>, dates = <Date vector or NULL>,
#'   warning = <message or NULL>)
#' @export
api_v1_resolve_holidays <- function(holidays_country) {
  if (is.null(holidays_country)) return(list(calendar = NULL, dates = NULL, warning = NULL))
  fetched <- tryCatch(th2_fetch_holidays_fr(), error = function(e) conditionMessage(e))
  if (is.list(fetched)) {
    return(list(calendar = HOLIDAYS_CALENDAR_FR, dates = as.Date(names(fetched)), warning = NULL))
  }
  list(calendar = NULL, dates = NULL, warning = sprintf(
    "Calendrier des jours fériés (%s) injoignable : prévision calculée sans jours fériés (%s).",
    holidays_country, fetched
  ))
}

#' Applies the requested preprocessing to one series
#'
#' Uses `anomaly_detection()` and `outliers_detection(method_ls = "cpt")` on a
#' copy of the series. The original values stay in `history`.
#'
#' @param df data.frame with `date`, `value`
#' @param preprocessing named logical (`anomalies`, `outliers`)
#' @param fmt_num number formatter of the response
#' @param holiday_dates dates left untouched: with `holidays_country`, a dip on a
#'   holiday is the effect the models must learn, not an anomaly
#' @return list(df = <cleaned data.frame>, report = <response field>, warnings = <character>)
#' @export
api_v1_preprocess_series <- function(df, preprocessing, fmt_num = identity, holiday_dates = NULL) {
  original <- df$value
  current <- tibble::as_tibble(df[c("date", "value")])
  corrections <- list()
  counts <- list()
  warnings_out <- character(0)

  steps <- list(
    anomalies = list(kind = "anomaly", label = "anomalies", run = function(x) anomaly_detection(x)),
    outliers = list(kind = "outlier", label = "valeurs aberrantes", run = function(x) outliers_detection(x, method_ls = "cpt"))
  )
  for (key in names(steps)) {
    if (!(key %in% names(preprocessing)) || !isTRUE(preprocessing[[key]])) next
    step <- steps[[key]]
    before <- current$value
    # The package functions signal bad input with `return(warning(...))`,
    # which returns a string: anything but a data.frame counts as a failure.
    cleaned <- tryCatch(suppressWarnings(step$run(current)), error = function(e) e)
    if (!is.data.frame(cleaned) || !("value" %in% names(cleaned)) || nrow(cleaned) != nrow(current)) {
      reason <- if (inherits(cleaned, "condition")) conditionMessage(cleaned) else "résultat inattendu"
      warnings_out <- c(warnings_out, sprintf(
        "Correction des %s impossible sur cette série : %s. Série laissée telle quelle pour cette étape.", step$label, reason
      ))
      counts[[paste0(key, "_corrected")]] <- 0L
      next
    }
    after <- as.numeric(cleaned$value)
    on_holiday <- as.Date(current$date) %in% holiday_dates
    after[on_holiday] <- before[on_holiday]
    # anomalize recomposes the whole series, so untouched points come back
    # with a rounding drift (1165 -> 1165.0002): below this tolerance a point
    # keeps its exact value and is not reported as corrected.
    # A step returning NA for a point leaves that point as it was.
    drift <- abs(after - before) > 1e-6 * pmax(1, abs(before))
    unchanged <- is.na(drift) | !drift
    after[unchanged] <- before[unchanged]
    changed <- which(!unchanged)
    for (i in changed) {
      corrections[[length(corrections) + 1]] <- list(
        date = format(as.Date(current$date[i]), "%Y-%m-%d"),
        original = fmt_num(original[i]),
        corrected = fmt_num(after[i]),
        kind = step$kind
      )
    }
    counts[[paste0(key, "_corrected")]] <- length(changed)
    current$value <- after
  }

  out_df <- df
  out_df$value <- current$value
  list(df = out_df, report = c(counts, list(corrections = corrections)), warnings = warnings_out)
}

#' Fits one of the package's machine-learning engines (fitted workflow)
#'
#' @keywords internal
.api_v1_fit_ml_model <- function(model_name, train_df, holidays_calendar = NULL) {
  switch(model_name,
    linear = th2_linear_engine(train_df, "value", "date", fit_model = TRUE),
    mars = th2_mars_engine(train_df, "value", "date", fit_model = TRUE),
    random_forest = th2_random_forest_engine(train_df, "value", use_holidays = holidays_calendar, use_meteo = FALSE, fit_model = TRUE),
    xgboost = th2_xgboost_engine(train_df, "date", "value", use_holidays = holidays_calendar, use_meteo = FALSE, fit_model = TRUE),
    NULL
  )
}

#' Members of the average ensemble for a request
#'
#' The other requested models, or the statistical candidates when the ensemble
#' is requested alone.
#'
#' @keywords internal
.api_v1_ensemble_members <- function(requested_models, fitted_names) {
  members <- setdiff(requested_models, ENSEMBLE_MODEL)
  if (length(members) == 0) members <- CANDIDATE_MODELS
  intersect(members, fitted_names)
}

#' Builds the average ensemble from fitted members
#'
#' @keywords internal
.api_v1_build_ensemble <- function(fits, members) {
  if (length(members) < 2) return(NULL)
  tbl <- do.call(modeltime::modeltime_table, unname(fits[members]))
  tryCatch(modeltime.ensemble::ensemble_average(tbl, type = "mean"), error = function(e) NULL)
}

#' Adds the `is_holiday` flag (0/1) used by Prophet; no-op without holidays
#'
#' @keywords internal
.api_v1_add_holiday_flag <- function(df, holiday_dates) {
  if (length(holiday_dates) == 0) return(df)
  df$is_holiday <- as.integer(as.Date(df$date) %in% holiday_dates)
  df
}

#' Prophet with the holidays as an external regressor
#'
#' `th2_prophet_engine()` hands the holidays to Prophet through
#' `set_engine(holidays = ...)`, but modeltime's `prophet_fit_impl()` does not
#' pass that argument on: the fitted model has no holidays (measured with
#' modeltime 1.3.5). An `is_holiday` regressor reaches Prophet and works with
#' `modeltime_refit()` and `modeltime_forecast(new_data = ...)`. Same
#' changepoint settings as `th2_prophet_engine()`.
#'
#' @keywords internal
.api_v1_fit_prophet_holidays <- function(train_df, seasonal_yearly = "auto") {
  spec <- modeltime::prophet_reg(changepoint_num = 25, changepoint_range = 0.8, seasonal_yearly = seasonal_yearly) |>
    parsnip::set_engine("prophet")
  parsnip::fit(spec, value ~ date + is_holiday, data = train_df[c("date", "value", "is_holiday")])
}

#' Future dates of the horizon, at the series frequency
#'
#' @keywords internal
.api_v1_future_frame <- function(df, horizon, frequency) {
  unit <- c(day = "day", week = "week", month = "month", quarter = "quarter", year = "year")[[frequency]]
  last <- max(as.Date(df$date))
  dates <- seq(last, by = unit, length.out = horizon + 1L)[-1]
  data.frame(date = dates, value = NA_real_)
}
