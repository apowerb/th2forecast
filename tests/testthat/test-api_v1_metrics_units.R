test_that("les metriques mape/smape sont des fractions, pas des pourcentages", {
  dates <- seq(as.Date("2022-01-01"), by = "month", length.out = 12)
  sales <- c(rep(100, 10), 110, 120)
  rows <- lapply(seq_along(dates), function(i) list(date = format(dates[i], "%Y-%m-%d"), sales = sales[i]))

  body <- list(
    data = rows, date_var = "date", target_var = "sales",
    horizon = 2, models = list("naive"), confidence_levels = list(0.8)
  )
  limits <- list(max_rows = 1000, max_series = 10, max_horizon = 366)

  out <- api_v1_run_forecast(body, limits)
  expect_equal(out$status_code, 200L)

  s <- out$body$series[[1]]
  expect_equal(s$model, "naive")

  # Holdout = 2 derniers points (110, 120) ; le naive prevoit la derniere
  # valeur d'entrainement (100) sur tout l'horizon de test.
  # MAPE = mean(|110-100|/110, |120-100|/120) = mean(10/110, 20/120)
  expected_mape <- mean(c(10 / 110, 20 / 120))
  expect_equal(s$metrics$mape, expected_mape, tolerance = 1e-3)

  # Doit rester une fraction : jamais >= 1 sur ce cas (10 % d'erreur max).
  expect_lt(s$metrics$mape, 1)
  expect_lt(s$metrics$smape, 1)
  expect_lt(s$baseline$metrics$mape, 1)
  expect_lt(s$baseline$metrics$smape, 1)
})
