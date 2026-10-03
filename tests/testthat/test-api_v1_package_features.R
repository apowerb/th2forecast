# API v1 exposure of the package's own features: preprocessing, holidays,
# machine-learning models and the ensemble. Network calls are mocked.

.feature_body <- function(n = 36, ...) {
  dates <- seq(as.Date("2021-01-01"), by = "month", length.out = n)
  set.seed(42)
  values <- 100 + seq_len(n) + 10 * sin(seq_len(n) / 12 * 2 * pi) + stats::rnorm(n, 0, 1)
  values[20] <- 400
  rows <- lapply(seq_along(dates), function(i) list(date = format(dates[i], "%Y-%m-%d"), sales = values[i]))
  body <- list(data = rows, date_var = "date", target_var = "sales", horizon = 3,
               models = list("naive"), confidence_levels = list(0.8))
  extra <- list(...)
  body[names(extra)] <- extra
  body
}

.limits <- list(max_rows = 5000, max_series = 10, max_horizon = 366)

.fake_holidays <- function(...) {
  list(`2021-01-01` = "1er janvier", `2021-12-25` = "Jour de Noël", `2022-01-01` = "1er janvier",
       `2022-12-25` = "Jour de Noël", `2023-01-01` = "1er janvier", `2023-12-25` = "Jour de Noël",
       `2024-01-01` = "1er janvier", `2024-12-25` = "Jour de Noël")
}

.series <- function(res) res$body$series[[1]]

test_that("the package's machine-learning models are accepted and return a full forecast", {
  for (m in c("linear", "mars", "random_forest", "xgboost")) {
    res <- api_v1_run_forecast(.feature_body(models = list(m)), .limits)
    expect_equal(res$status_code, 200L, info = m)
    s <- .series(res)
    expect_equal(s$model, m, info = m)
    expect_length(s$forecast, 3)
    expect_true(all(vapply(s$forecast, function(r) !is.null(r$lower_80) && !is.null(r$upper_80), logical(1))), info = m)
  }
})

test_that("machine-learning models also run on daily data", {
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 90)
  rows <- lapply(seq_along(dates), function(i) list(date = format(dates[i], "%Y-%m-%d"), sales = 50 + (i %% 7) * 3))
  body <- list(data = rows, date_var = "date", target_var = "sales", horizon = 7,
               models = list("xgboost"), confidence_levels = list(0.8))
  res <- api_v1_run_forecast(body, .limits)
  expect_equal(res$status_code, 200L)
  expect_equal(.series(res)$model, "xgboost")
  expect_length(.series(res)$forecast, 7)
})

test_that("auto keeps its three statistical candidates", {
  expect_setequal(th2forecast:::.api_v1_expand_models("auto"), c("arima", "prophet", "ets"))
})

test_that("ensemble averages the other requested models", {
  res <- api_v1_run_forecast(.feature_body(models = list("ensemble", "linear", "ets")), .limits)
  expect_equal(res$status_code, 200L)
  s <- .series(res)
  expect_true(s$model %in% c("ensemble", "linear", "ets"))
  expect_length(s$forecast, 3)

  forced <- api_v1_run_forecast(.feature_body(models = list("ensemble")), .limits)
  expect_equal(forced$status_code, 200L)
  expect_equal(.series(forced)$model, "ensemble")
})

test_that("anomaly correction cleans the spike, keeps the original history and lists the corrections", {
  res <- api_v1_run_forecast(.feature_body(preprocessing = list(anomalies = TRUE)), .limits)
  expect_equal(res$status_code, 200L)
  s <- .series(res)
  hist_values <- vapply(s$history, function(r) as.numeric(r$value), numeric(1))
  expect_true(any(abs(hist_values - 400) < 1e-6))
  expect_gte(s$preprocessing$anomalies_corrected, 1)
  spike <- Filter(function(c) c$date == "2022-08-01", s$preprocessing$corrections)
  expect_length(spike, 1)
  expect_equal(spike[[1]]$kind, "anomaly")
  expect_equal(as.numeric(spike[[1]]$original), 400, tolerance = 1e-6)
  expect_lt(as.numeric(spike[[1]]$corrected), 200)
})

test_that("outlier correction brings a point back to the mean of its segment", {
  # Level shift at day 151, outlier on day 60 (2024-02-29) inside the first segment.
  set.seed(3)
  values <- c(stats::rnorm(150, 100, 2), stats::rnorm(150, 130, 2))
  values[60] <- 125
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 300)
  rows <- lapply(seq_along(dates), function(i) list(date = format(dates[i], "%Y-%m-%d"), sales = values[i]))
  body <- list(data = rows, date_var = "date", target_var = "sales", horizon = 7, models = list("naive"),
               confidence_levels = list(0.8), preprocessing = list(outliers = TRUE))
  res <- api_v1_run_forecast(body, .limits)
  expect_equal(res$status_code, 200L)
  p <- .series(res)$preprocessing
  expect_gte(p$outliers_corrected, 1)
  expect_true(all(vapply(p$corrections, function(c) c$kind == "outlier", logical(1))))
  point <- Filter(function(c) c$date == "2024-02-29", p$corrections)
  expect_length(point, 1)
  expect_lt(abs(as.numeric(point[[1]]$corrected) - 100), 5)
})

test_that("without preprocessing the series carries no preprocessing field", {
  res <- api_v1_run_forecast(.feature_body(), .limits)
  expect_null(.series(res)$preprocessing)
})

test_that("holidays_country FR feeds prophet and the machine-learning models", {
  local_mocked_bindings(th2_fetch_holidays_fr = .fake_holidays, .package = "th2forecast")
  for (m in c("prophet", "random_forest")) {
    res <- api_v1_run_forecast(.feature_body(models = list(m), holidays_country = "FR"), .limits)
    expect_equal(res$status_code, 200L, info = m)
    s <- .series(res)
    expect_equal(s$model, m, info = m)
    expect_false(any(grepl("férié", unlist(s$warnings))), info = m)
  }
})

test_that("prophet learns the holiday dip and applies it to future holidays", {
  # Synthetic calendar: the 1st and the 15th of every month are holidays, with sales 40 lower.
  days <- seq(as.Date("2023-01-01"), as.Date("2025-12-31"), by = "day")
  holiday_days <- days[format(days, "%d") %in% c("01", "15")]
  fake <- stats::setNames(as.list(rep("Jour test", length(holiday_days))), format(holiday_days, "%Y-%m-%d"))
  local_mocked_bindings(th2_fetch_holidays_fr = function(...) fake, .package = "th2forecast")

  dates <- seq(as.Date("2023-01-01"), as.Date("2024-12-31"), by = "day")
  set.seed(7)
  values <- 100 + stats::rnorm(length(dates), 0, 1) - 40 * (dates %in% holiday_days)
  rows <- lapply(seq_along(dates), function(i) list(date = format(dates[i], "%Y-%m-%d"), sales = values[i]))
  body <- list(data = rows, date_var = "date", target_var = "sales", horizon = 20, models = list("prophet"),
               confidence_levels = list(0.8), holidays_country = "FR")
  res <- api_v1_run_forecast(body, .limits)
  expect_equal(res$status_code, 200L)
  fc <- .series(res)$forecast
  value_on <- function(day) as.numeric(Filter(function(r) r$date == day, fc)[[1]]$value)
  expect_lt(value_on("2025-01-01"), value_on("2025-01-02") - 20)
  expect_lt(value_on("2025-01-15"), value_on("2025-01-16") - 20)

  without <- api_v1_run_forecast(modifyList(body, list(holidays_country = NULL)), .limits)
  fc_without <- .series(without)$forecast
  expect_gt(as.numeric(fc_without[[1]]$value), value_on("2025-01-01") + 20)
})

test_that("an ensemble with prophet and a machine-learning model runs with holidays", {
  local_mocked_bindings(th2_fetch_holidays_fr = .fake_holidays, .package = "th2forecast")
  res <- api_v1_run_forecast(.feature_body(models = list("ensemble"), holidays_country = "FR"), .limits)
  expect_equal(res$status_code, 200L)
  body <- .feature_body(models = list("ensemble", "prophet", "random_forest"), holidays_country = "FR")
  res <- api_v1_run_forecast(body, .limits)
  expect_equal(res$status_code, 200L)
  expect_length(.series(res)$forecast, 3)
})

test_that("anomaly correction leaves holiday dips alone when holidays are requested", {
  dates <- seq(as.Date("2023-01-01"), as.Date("2024-12-31"), by = "day")
  holiday_days <- as.Date(c("2023-05-01", "2023-07-14", "2024-05-01", "2024-07-14"))
  fake <- stats::setNames(as.list(rep("Jour test", length(holiday_days))), format(holiday_days, "%Y-%m-%d"))
  local_mocked_bindings(th2_fetch_holidays_fr = function(...) fake, .package = "th2forecast")
  set.seed(11)
  values <- 100 + stats::rnorm(length(dates), 0, 1) - 60 * (dates %in% holiday_days)
  rows <- lapply(seq_along(dates), function(i) list(date = format(dates[i], "%Y-%m-%d"), sales = values[i]))
  body <- list(data = rows, date_var = "date", target_var = "sales", horizon = 7, models = list("naive"),
               confidence_levels = list(0.8), preprocessing = list(anomalies = TRUE))

  without <- api_v1_run_forecast(body, .limits)
  corrected_days <- vapply(.series(without)$preprocessing$corrections, function(c) c$date, character(1))
  expect_true(any(format(holiday_days, "%Y-%m-%d") %in% corrected_days))

  with <- api_v1_run_forecast(modifyList(body, list(holidays_country = "FR")), .limits)
  expect_equal(with$status_code, 200L)
  corrected_days <- vapply(.series(with)$preprocessing$corrections, function(c) c$date, character(1))
  expect_false(any(format(holiday_days, "%Y-%m-%d") %in% corrected_days))
})

test_that("an unreachable holiday calendar degrades to a warning, not an error", {
  local_mocked_bindings(th2_fetch_holidays_fr = function(...) stop("timeout"), .package = "th2forecast")
  res <- api_v1_run_forecast(.feature_body(models = list("prophet"), holidays_country = "FR"), .limits)
  expect_equal(res$status_code, 200L)
  expect_true(any(grepl("jours fériés", unlist(res$body$warnings))))
  expect_equal(.series(res)$model, "prophet")
})

test_that("invalid preprocessing, holidays_country and models are refused with a 400", {
  bad <- list(
    list(preprocessing = list(anomalies = "yes")),
    list(preprocessing = list(smoothing = TRUE)),
    list(preprocessing = "anomalies"),
    list(holidays_country = "DE"),
    list(models = list("arimax"))
  )
  fields <- c("preprocessing.anomalies", "preprocessing.smoothing", "preprocessing", "holidays_country", "models")
  for (i in seq_along(bad)) {
    res <- do.call(api_v1_run_forecast, list(do.call(.feature_body, bad[[i]]), .limits))
    expect_equal(res$status_code, 400L, info = fields[i])
    expect_true(fields[i] %in% vapply(res$body$errors, function(e) e$field, character(1)), info = fields[i])
  }
})

test_that("the French holiday fetch is cached per process", {
  calls <- 0
  th2forecast:::.th2_holidays_cache_clear()
  local_mocked_bindings(
    .th2_holidays_request = function(region, timeout) { calls <<- calls + 1; .fake_holidays() },
    .package = "th2forecast"
  )
  th2_fetch_holidays_fr()
  th2_fetch_holidays_fr()
  expect_equal(calls, 1)
  th2forecast:::.th2_holidays_cache_clear()
})
