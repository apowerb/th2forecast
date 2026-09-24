test_that("api_v1_check_auth desactive l'authentification si aucun token n'est configure", {
  withr_token <- Sys.getenv("TH2FORECAST_API_TOKEN")
  Sys.unsetenv("TH2FORECAST_API_TOKEN")
  on.exit(if (nzchar(withr_token)) Sys.setenv(TH2FORECAST_API_TOKEN = withr_token), add = TRUE)

  expect_true(api_v1_check_auth(NULL))
  expect_true(api_v1_check_auth("n'importe quoi"))
})

test_that("api_v1_check_auth exige un jeton Bearer correct quand configure", {
  old <- Sys.getenv("TH2FORECAST_API_TOKEN")
  Sys.setenv(TH2FORECAST_API_TOKEN = "secret-123")
  on.exit(Sys.setenv(TH2FORECAST_API_TOKEN = old), add = TRUE)

  expect_false(api_v1_check_auth(NULL))
  expect_false(api_v1_check_auth("Bearer mauvais-jeton"))
  expect_true(api_v1_check_auth("Bearer secret-123"))
})

test_that("api_v1_log_request produit une ligne JSON exploitable", {
  out <- capture.output(
    api_v1_log_request("POST /v1/forecast", 200L, 12.3, n_rows = 10, n_series = 1, models = c("prophet"))
  )
  parsed <- jsonlite::fromJSON(out)
  expect_equal(parsed$route, "POST /v1/forecast")
  expect_equal(parsed$status, 200)
  expect_equal(parsed$n_rows, 10)
})

test_that("api_v1_limits lit les variables d'environnement avec des defauts", {
  old <- Sys.getenv("TH2FORECAST_MAX_ROWS")
  Sys.unsetenv("TH2FORECAST_MAX_ROWS")
  on.exit(if (nzchar(old)) Sys.setenv(TH2FORECAST_MAX_ROWS = old), add = TRUE)

  l <- api_v1_limits()
  expect_equal(l$max_rows, 100000L)
  expect_equal(l$max_series, 200L)
  expect_equal(l$max_horizon, 366L)
})
