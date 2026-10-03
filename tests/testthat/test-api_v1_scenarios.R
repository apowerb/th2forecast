# Scenarios on the R engine: the base forecast plus explicit adjustments
# (`percent` or `add`), applied pro rata to the days each period covers --
# the same contract as the Python engine (docs/API.md).

.monthly_body <- function(...) {
  dates <- seq(as.Date("2022-01-01"), by = "month", length.out = 24)
  rows <- lapply(seq_along(dates), function(i) list(
    date = format(dates[i], "%Y-%m-%d"), sales = 100 + i + 10 * sin(i / 12 * 2 * pi)
  ))
  c(list(data = rows, date_var = "date", target_var = "sales", horizon = 3,
         models = list("naive"), confidence_levels = list(0.8)), list(...))
}

.limits <- list(max_rows = 1000, max_series = 10, max_horizon = 366)

.values <- function(rows, field = "value") vapply(rows, function(r) as.numeric(r[[field]]), numeric(1))

test_that("coverage is the share of each period's days inside the range", {
  periods <- as.Date(c("2024-01-01", "2024-02-01", "2024-03-01"))
  share <- api_v1_coverage(periods, "month", as.Date("2024-01-01"), as.Date("2024-01-15"))
  expect_equal(share, c(15 / 31, 0, 0))
  share <- api_v1_coverage(periods, "month", as.Date("2024-01-20"), as.Date("2024-02-29"))
  expect_equal(share, c(12 / 31, 1, 0))
  week <- api_v1_coverage(as.Date("2024-01-01"), "week", as.Date("2024-01-03"), as.Date("2024-01-04"))
  expect_equal(week, 2 / 7)
  expect_equal(api_v1_coverage(as.Date("2024-01-31"), "day", as.Date("2024-01-31"), as.Date("2024-01-31")), 1)
})

test_that("a month-end period ends on the last day of its month", {
  expect_equal(api_v1_period_end(as.Date("2024-01-31"), "month"), as.Date("2024-02-28"))
  expect_equal(api_v1_period_end(as.Date("2024-01-01"), "quarter"), as.Date("2024-03-31"))
})

test_that("a percent scenario scales the forecast and its bands, with the difference", {
  body <- .monthly_body(scenarios = list(list(
    name = "Hausse", adjustments = list(list(start = "2024-01-01", end = "2024-03-31", percent = 10))
  )))
  out <- api_v1_run_forecast(body, .limits)
  expect_equal(out$status_code, 200L)
  s <- out$body$series[[1]]
  expect_length(s$scenarios, 1)
  scn <- s$scenarios[[1]]
  expect_equal(scn$name, "Hausse")
  expect_equal(.values(scn$forecast), .values(s$forecast) * 1.1, tolerance = 1e-5)
  expect_equal(.values(scn$forecast, "lower_80"), .values(s$forecast, "lower_80") * 1.1, tolerance = 1e-5)
  expect_equal(.values(scn$forecast, "upper_80"), .values(s$forecast, "upper_80") * 1.1, tolerance = 1e-5)
  expect_equal(vapply(scn$forecast, `[[`, "", "date"), vapply(s$forecast, `[[`, "", "date"))
  expect_equal(scn$difference$percent, 10)
  expect_equal(scn$difference$total, sum(.values(s$forecast)) * 0.1, tolerance = 1e-4)
})

test_that("an add scenario is applied pro rata to the covered days", {
  body <- .monthly_body(scenarios = list(list(
    name = "Promo", adjustments = list(list(start = "2024-01-01", end = "2024-01-15", add = 31))
  )))
  s <- api_v1_run_forecast(body, .limits)$body$series[[1]]
  delta <- .values(s$scenarios[[1]]$forecast) - .values(s$forecast)
  expect_equal(delta, c(15, 0, 0), tolerance = 1e-5)
})

test_that("an adjustment without end covers its start day only", {
  body <- .monthly_body(scenarios = list(list(
    name = "Jour", adjustments = list(list(start = "2024-02-10", add = 29))
  )))
  s <- api_v1_run_forecast(body, .limits)$body$series[[1]]
  delta <- .values(s$scenarios[[1]]$forecast) - .values(s$forecast)
  expect_equal(delta, c(0, 1, 0), tolerance = 1e-5)
})

test_that("scenarios arriving as a data.frame (simplified JSON) are read the same way", {
  json <- '[{"name": "Hausse", "adjustments": [{"start": "2024-01-01", "end": "2024-03-31", "percent": 10}]}]'
  body <- .monthly_body(scenarios = jsonlite::fromJSON(json))
  s <- api_v1_run_forecast(body, .limits)$body$series[[1]]
  expect_equal(s$scenarios[[1]]$difference$percent, 10)
})

test_that("events inside a scenario are reported as unsupported, the adjustments still apply", {
  body <- .monthly_body(scenarios = list(list(
    name = "Sans promo", events = list(),
    adjustments = list(list(start = "2024-01-01", end = "2024-03-31", percent = -5))
  )))
  s <- api_v1_run_forecast(body, .limits)$body$series[[1]]
  expect_equal(s$scenarios[[1]]$difference$percent, -5)
  expect_true(any(grepl("Sans promo", unlist(s$warnings)) & grepl("moteur R", unlist(s$warnings))))
})

test_that("request-level events are reported as ignored by the R engine", {
  body <- .monthly_body(events = list(list(name = "promo", dates = list("2023-12-05"))))
  out <- api_v1_run_forecast(body, .limits)
  expect_equal(out$status_code, 200L)
  expect_true(any(grepl("events", unlist(out$body$warnings)) & grepl("moteur R", unlist(out$body$warnings))))
})

test_that("without scenarios the series carries no scenarios field", {
  s <- api_v1_run_forecast(.monthly_body(), .limits)$body$series[[1]]
  expect_null(s$scenarios)
})

test_that("invalid scenarios are refused with a 400 naming the field", {
  bad <- list(
    list(list(adjustments = list())),
    list(list(name = "x", adjustments = list(list(start = "2024-01-01", percent = 5, add = 3)))),
    list(list(name = "x", adjustments = list(list(start = "2024-01-01")))),
    list(list(name = "x", adjustments = list(list(start = "2024-01-01", percent = -100)))),
    list(list(name = "x", adjustments = list(list(start = "pas une date", percent = 5)))),
    lapply(1:6, function(i) list(name = paste0("s", i))),
    "not a list"
  )
  for (scn in bad) {
    out <- api_v1_run_forecast(.monthly_body(scenarios = scn), .limits)
    expect_equal(out$status_code, 400L, info = paste(deparse(scn), collapse = ""))
    fields <- vapply(out$body$errors, function(e) as.character(e$field), "")
    expect_true(all(startsWith(fields, "scenarios")), info = paste(fields, collapse = ","))
  }
})

test_that("day-first dates are accepted in adjustments", {
  body <- .monthly_body(scenarios = list(list(
    name = "Promo", adjustments = list(list(start = "01/01/2024", end = "15/01/2024", add = 31))
  )))
  s <- api_v1_run_forecast(body, .limits)$body$series[[1]]
  expect_equal(.values(s$scenarios[[1]]$forecast) - .values(s$forecast), c(15, 0, 0), tolerance = 1e-5)
})
