test_that("api_v1_run_forecast renvoie une reponse 200 au format attendu (modeles naifs)", {
  rows <- lapply(1:24, function(i) list(
    date = sprintf("2022-%02d-01", ((i - 1) %% 12) + 1),
    sales = 100 + i + 10 * sin(i / 12 * 2 * pi)
  ))
  # Correction des annees pour obtenir des dates mensuelles croissantes valides.
  rows <- Map(function(row, i) {
    row$date <- format(seq(as.Date("2022-01-01"), by = "month", length.out = 24)[i], "%Y-%m-%d")
    row
  }, rows, seq_along(rows))

  body <- list(
    data = rows, date_var = "date", target_var = "sales",
    horizon = 3, models = list("naive"), confidence_levels = list(0.8)
  )
  limits <- list(max_rows = 1000, max_series = 10, max_horizon = 366)

  out <- api_v1_run_forecast(body, limits)

  expect_equal(out$status_code, 200L)
  expect_equal(out$body$status, "success")
  expect_equal(out$body$api_version, "1")
  expect_equal(out$body$frequency, "month")
  expect_length(out$body$series, 1)

  s <- out$body$series[[1]]
  expect_equal(s$model, "naive")
  expect_length(s$forecast, 3)
  expect_true(all(c("date", "value", "lower_80", "upper_80") %in% names(s$forecast[[1]])))
  expect_true(all(c("mape", "smape", "mase", "rmse", "holdout_points") %in% names(s$metrics)))
  expect_true(s$reliability %in% c("good", "fair", "poor", "unknown"))
})

test_that("api_v1_run_forecast produit une entree par groupe quand group_var est fourni", {
  make_rows <- function(store) {
    dates <- seq(as.Date("2022-01-01"), by = "month", length.out = 14)
    lapply(seq_along(dates), function(i) list(
      date = format(dates[i], "%Y-%m-%d"), sales = 50 + i + (if (store == "B") 100 else 0), store = store
    ))
  }
  body <- list(
    data = c(make_rows("A"), make_rows("B")),
    date_var = "date", target_var = "sales", group_var = "store",
    horizon = 2, models = list("snaive")
  )
  limits <- list(max_rows = 1000, max_series = 10, max_horizon = 366)

  out <- api_v1_run_forecast(body, limits)

  expect_equal(out$status_code, 200L)
  expect_length(out$body$series, 2)
  groups <- vapply(out$body$series, function(s) s$group, character(1))
  expect_setequal(groups, c("A", "B"))
})

test_that("api_v1_health renvoie un statut UP et une version", {
  h <- api_v1_health()
  expect_equal(h$status, "UP")
  expect_true(nzchar(h$version))
})
