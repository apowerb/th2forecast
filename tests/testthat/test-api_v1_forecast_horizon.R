# Regression : les modeles sans dates (arima, ets) doivent prevoir a partir de la
# fin de la serie, pas de la fin du jeu d'entrainement du backtest.
truth <- function(t) 100 + 2 * t + 30 * sin(2 * pi * t / 12)

make_body <- function(model) {
  set.seed(1)
  dates <- seq(as.Date("2022-01-01"), by = "month", length.out = 48)
  rows <- lapply(1:48, function(i) list(
    date = format(dates[i], "%Y-%m-%d"), sales = truth(i) + stats::rnorm(1, 0, 2)
  ))
  list(
    data = rows, date_var = "date", target_var = "sales",
    horizon = 12, models = list(model), confidence_levels = list(0.8)
  )
}

for (model in c("arima", "ets")) {
  test_that(sprintf("%s prevoit les 12 mois qui suivent la serie", model), {
    limits <- list(max_rows = 1000, max_series = 10, max_horizon = 366)
    out <- api_v1_run_forecast(make_body(model), limits)
    s <- out$body$series[[1]]
    expect_equal(s$model, model)
    expect_equal(s$forecast[[1]]$date, "2026-01-01")

    values <- vapply(s$forecast, function(r) r$value, numeric(1))
    # Serie a verite connue : un decalage de 9 mois (le holdout) donne une erreur ~29.
    expect_lt(mean(abs(values - truth(49:60))), 5)
  })
}
