library(testthat)

# 36 months of trend + seasonality, with a x3 spike in September 2024 (index 21).
.spike_series <- function() {
  i <- 0:35
  value <- round(1000 + 15 * i + 120 * sin(2 * pi * (i %% 12 + 1) / 12), 1)
  value[21] <- value[21] * 3
  tibble::tibble(date = seq(as.Date("2023-01-01"), by = "month", length.out = 36), value = value)
}

test_that("outliers_detection corrects an isolated spike that cpt isolates in a short segment", {
  # changepoint::cpt.meanvar puts the spike in a 3-point segment, where no
  # point can be more than (n - 1) / sqrt(n) = 1.15 sd from the mean: before
  # short segments were merged, the 3 sd test could never flag it.
  input <- .spike_series()
  result <- outliers_detection(input, method_ls = "cpt")
  changed <- which(abs(result$value - input$value) > 1e-6)
  expect_equal(changed, 21L)
  expect_lt(result$value[21], 1.5 * 1180)
})

test_that("outliers_detection accepts a base data.frame", {
  # With a single date column, `input_data[, date_variable]` dropped to a vector
  # and the date column name was lost.
  input <- as.data.frame(.spike_series())
  result <- outliers_detection(input, method_ls = "cpt")
  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), dim(input))
  expect_equal(result$date, input$date)
})

test_that("short changepoint segments are merged into a neighbour", {
  merged <- th2forecast:::.merge_short_segments(36L, c(12L, 20L, 22L))
  segment_lengths <- diff(c(0L, merged, 36L))
  expect_true(all(segment_lengths >= 11L))
  expect_equal(th2forecast:::.merge_short_segments(300L, 150L), 150L)
  expect_equal(th2forecast:::.merge_short_segments(8L, c(3L, 5L)), integer(0))
})

test_that("the preprocessing report counts only real corrections", {
  # anomalize recomposes the whole series: untouched points come back with a
  # rounding drift (1165 -> 1165.0002) that was reported as a correction.
  res <- api_v1_preprocess_series(.spike_series(), list(anomalies = TRUE))
  expect_equal(res$report$anomalies_corrected, 1L)
  expect_equal(vapply(res$report$corrections, function(c) c$date, ""), "2024-09-01")
  untouched <- setdiff(seq_len(36), 21L)
  expect_identical(res$df$value[untouched], .spike_series()$value[untouched])
})

test_that("the outliers step of the API corrects the isolated spike", {
  res <- api_v1_preprocess_series(.spike_series(), list(outliers = TRUE))
  expect_equal(res$report$outliers_corrected, 1L)
  expect_equal(res$report$corrections[[1]]$date, "2024-09-01")
})
