test_that("api_v1_detect_frequency reconnait une serie mensuelle", {
  dates <- as.Date(sprintf("2024-%02d-01", 1:12))
  expect_equal(api_v1_detect_frequency(dates), "month")
})

test_that("api_v1_detect_frequency reconnait une serie journaliere", {
  dates <- as.Date("2024-01-01") + 0:30
  expect_equal(api_v1_detect_frequency(dates), "day")
})

test_that("api_v1_detect_frequency reconnait une serie hebdomadaire", {
  dates <- as.Date("2024-01-01") + seq(0, 70, by = 7)
  expect_equal(api_v1_detect_frequency(dates), "week")
})

test_that("api_v1_seasonal_period associe la bonne periode par frequence", {
  expect_equal(api_v1_seasonal_period("month"), 12L)
  expect_equal(api_v1_seasonal_period("year"), 1L)
  expect_equal(api_v1_seasonal_period("day"), 7L)
})

test_that("api_v1_regularize_series comble les trous et interpole", {
  df <- data.frame(
    date = as.Date(c("2024-01-01", "2024-03-01")),
    value = c(10, 30)
  )
  out <- api_v1_regularize_series(df, "month")

  expect_equal(out$n_padded, 1L)
  expect_equal(nrow(out$df), 3L)
  expect_equal(out$df$value[out$df$date == as.Date("2024-02-01")], 20)
  expect_false(anyNA(out$df$value))
})

test_that("api_v1_regularize_series ne modifie rien quand il n'y a pas de trou", {
  df <- data.frame(date = as.Date(sprintf("2024-%02d-01", 1:6)), value = 1:6)
  out <- api_v1_regularize_series(df, "month")

  expect_equal(out$n_padded, 0L)
  expect_equal(nrow(out$df), 6L)
})
