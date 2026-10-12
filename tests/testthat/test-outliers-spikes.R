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

# 24 months of trend with two spikes and a dip: a big spike inflates the
# standard deviation of its segment and hides itself and its neighbours.
.masked_series <- function() {
  value <- c(100, 104, 108, 111, 950, 118, 121, 125, 129, 5, 136, 140,
             143, 147, 151, 154, 158, 162, 900, 169, 173, 176, 180, 184)
  tibble::tibble(date = seq(as.Date("2024-01-01"), by = "month", length.out = 24), value = value)
}

test_that("several spikes and dips in a segment do not mask each other", {
  input <- .masked_series()
  result <- outliers_detection(input, method_ls = "cpt")
  changed <- which(abs(result$value - input$value) > 1e-6)
  expect_equal(changed, c(5L, 10L, 19L))
  # Replacement follows the neighbours, not a mean pulled up by the spike.
  expect_lt(abs(result$value[5] - 114.5), 10)
  expect_lt(abs(result$value[10] - 132.5), 10)
  expect_lt(abs(result$value[19] - 165.5), 10)
})

test_that("the outliers step of the API reports the three masked points", {
  res <- api_v1_preprocess_series(.masked_series(), list(outliers = TRUE))
  expect_equal(res$report$outliers_corrected, 3L)
  expect_equal(vapply(res$report$corrections, function(c) c$date, ""),
               c("2024-05-01", "2024-10-01", "2025-07-01"))
})

test_that("leave-one-out scores match a direct computation", {
  x <- c(10, 11, 9, 10, 12, 50)
  direct <- vapply(seq_along(x), function(i) abs(x[i] - mean(x[-i])) / stats::sd(x[-i]), numeric(1))
  expect_equal(th2forecast:::.deleted_z(x), direct, tolerance = 1e-9)
})
