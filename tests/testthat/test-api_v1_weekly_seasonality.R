weekly_values <- function(n, seed = 1, seasonal = TRUE, noise = 0.06) {
  set.seed(seed)
  t <- seq_len(n)
  trend <- 500 + t
  season <- if (seasonal) 175 * sin(2 * pi * t / 52.18 - 1.2) else 0
  trend + season + stats::rnorm(n, 0, noise * trend)
}

test_that("une saisonnalite hebdomadaire reproductible d'une annee a l'autre est detectee", {
  expect_true(th2forecast:::.api_v1_stl_ets_applicable(weekly_values(156), 52L))
  expect_true(th2forecast:::.api_v1_stl_ets_applicable(weekly_values(208, seed = 2), 52L))
})

test_that("du bruit blanc hebdomadaire n'est jamais pris pour une saisonnalite", {
  for (seed in 1:30) {
    set.seed(seed)
    expect_false(th2forecast:::.api_v1_stl_ets_applicable(1000 + stats::rnorm(156, 0, 100), 52L))
  }
})

test_that("pas de decomposition sans deux cycles complets ni pour une periode que ets sait ajuster", {
  expect_false(th2forecast:::.api_v1_stl_ets_applicable(weekly_values(103), 52L))
  expect_false(th2forecast:::.api_v1_stl_ets_applicable(weekly_values(156), 12L))
})

test_that("prophet impose la saisonnalite annuelle des 1,5 an d'historique hebdomadaire", {
  expect_true(th2forecast:::api_v1_prophet_yearly(52L, 104L))
  expect_true(th2forecast:::api_v1_prophet_yearly(52L, 78L))
  expect_identical(th2forecast:::api_v1_prophet_yearly(52L, 51L), "auto")
  expect_identical(th2forecast:::api_v1_prophet_yearly(12L, 36L), "auto")
})

test_that("auto met le snaive en concurrence, un choix explicite de modeles ne l'ajoute pas", {
  fitted <- c("arima", "prophet", "ets", "snaive")
  auto_models <- th2forecast:::.api_v1_expand_models("auto")
  expect_setequal(th2forecast:::.api_v1_candidates(auto_models, fitted, "snaive", auto = TRUE), c("arima", "prophet", "ets", "snaive"))
  expect_setequal(th2forecast:::.api_v1_candidates(c("prophet", "ets"), fitted, "snaive", auto = FALSE), c("prophet", "ets"))
  expect_setequal(th2forecast:::.api_v1_candidates(auto_models, c("arima", "prophet", "ets", "naive"), "naive", auto = TRUE), c("arima", "prophet", "ets"))
})

test_that("ets hebdomadaire suit la saisonnalite annuelle sur 3 ans", {
  n <- 156L
  y <- weekly_values(n)
  df <- data.frame(date = seq(as.Date("2023-01-02"), by = "week", length.out = n), value = y)
  fit <- th2forecast:::.api_v1_fit_one_model("ets", df, seasonal_period = 52L)
  expect_false(is.null(fit))
  tbl <- modeltime::modeltime_table(fit)
  fc <- modeltime::modeltime_forecast(tbl, h = 26, actual_data = df)
  pred <- fc$.value[fc$.key == "prediction"]
  t <- n + seq_len(26)
  truth_trend <- 500 + t
  truth <- truth_trend + 175 * sin(2 * pi * t / 52.18 - 1.2)
  skill <- 1 - mean(abs(pred - truth)) / mean(abs(truth_trend - truth))
  expect_gt(skill, 0.5)
})
