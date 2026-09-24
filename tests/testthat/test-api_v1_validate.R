test_that("api_v1_validate_request rejette une colonne date absente avec la liste des colonnes", {
  body <- list(
    data = list(list(dt = "2024-01-01", sales = 10)),
    date_var = "date", target_var = "sales", horizon = 3
  )
  res <- api_v1_validate_request(body, list(max_rows = 100, max_series = 10, max_horizon = 366))

  expect_false(res$ok)
  expect_equal(res$status, 400L)
  expect_equal(res$errors[[1]]$field, "date_var")
  expect_match(res$errors[[1]]$message, "dt, sales")
})

test_that("api_v1_validate_request rejette une cible non numerique", {
  body <- list(
    data = list(
      list(date = "2024-01-01", sales = "abc"),
      list(date = "2024-02-01", sales = "def")
    ),
    date_var = "date", target_var = "sales", horizon = 1
  )
  res <- api_v1_validate_request(body, list(max_rows = 100, max_series = 10, max_horizon = 366))

  expect_false(res$ok)
  expect_equal(res$errors[[1]]$field, "target_var")
})

test_that("api_v1_validate_request rejette des dates non parsables", {
  body <- list(
    data = list(list(date = "pas-une-date", sales = 1)),
    date_var = "date", target_var = "sales", horizon = 1
  )
  res <- api_v1_validate_request(body, list(max_rows = 100, max_series = 10, max_horizon = 366))

  expect_false(res$ok)
  expect_match(res$errors[[1]]$message, "non parsables")
})

test_that("api_v1_validate_request rejette un modele inconnu", {
  rows <- lapply(1:12, function(i) list(date = sprintf("2024-%02d-01", i), sales = i))
  body <- list(data = rows, date_var = "date", target_var = "sales", horizon = 2, models = list("magic"))
  res <- api_v1_validate_request(body, list(max_rows = 100, max_series = 10, max_horizon = 366))

  expect_false(res$ok)
  expect_equal(res$errors[[1]]$field, "models")
  expect_match(res$errors[[1]]$message, "magic")
})

test_that("api_v1_validate_request detecte les doublons de dates par serie", {
  rows <- list(
    list(date = "2024-01-01", sales = 1, store = "A"),
    list(date = "2024-01-01", sales = 2, store = "A"),
    list(date = "2024-02-01", sales = 3, store = "A")
  )
  body <- list(data = rows, date_var = "date", target_var = "sales", group_var = "store", horizon = 1)
  res <- api_v1_validate_request(body, list(max_rows = 100, max_series = 10, max_horizon = 366))

  expect_false(res$ok)
  expect_match(res$errors[[1]]$message, "Doublons de dates")
})

test_that("api_v1_validate_request rejette un horizon trop grand pour le nombre de points", {
  rows <- lapply(1:5, function(i) list(date = sprintf("2024-%02d-01", i), sales = i))
  body <- list(data = rows, date_var = "date", target_var = "sales", horizon = 12)
  res <- api_v1_validate_request(body, list(max_rows = 100, max_series = 10, max_horizon = 366))

  expect_false(res$ok)
  expect_equal(res$errors[[1]]$field, "horizon")
  expect_match(res$errors[[1]]$message, "Trop peu de points")
})

test_that("api_v1_validate_request renvoie 413 quand l'horizon depasse la limite", {
  dates <- seq(as.Date("2022-01-01"), by = "month", length.out = 24)
  rows <- lapply(seq_along(dates), function(i) list(date = format(dates[i], "%Y-%m-%d"), sales = i))
  body <- list(data = rows, date_var = "date", target_var = "sales", horizon = 500)
  res <- api_v1_validate_request(body, list(max_rows = 100, max_series = 10, max_horizon = 366))

  expect_false(res$ok)
  expect_equal(res$status, 413L)
})

test_that("api_v1_validate_request renvoie 413 quand le nombre de lignes depasse la limite", {
  dates <- seq(as.Date("2022-01-01"), by = "day", length.out = 20)
  rows <- lapply(seq_along(dates), function(i) list(date = format(dates[i], "%Y-%m-%d"), sales = i))
  body <- list(data = rows, date_var = "date", target_var = "sales", horizon = 2)
  res <- api_v1_validate_request(body, list(max_rows = 10, max_series = 10, max_horizon = 366))

  expect_false(res$ok)
  expect_equal(res$status, 413L)
})

test_that("api_v1_validate_request accepte une requete valide et normalise les defauts", {
  rows <- lapply(1:12, function(i) list(date = sprintf("2024-%02d-01", i), sales = i * 10))
  body <- list(data = rows, date_var = "date", target_var = "sales", horizon = 2)
  res <- api_v1_validate_request(body, list(max_rows = 100, max_series = 10, max_horizon = 366))

  expect_true(res$ok)
  expect_equal(res$request$horizon, 2L)
  expect_equal(res$request$confidence_levels, c(0.8, 0.95))
  expect_true(is.null(res$request$group_var))
  expect_s3_class(res$request$df$date, "Date")
})
