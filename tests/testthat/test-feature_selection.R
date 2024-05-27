test_that("test feature_selection function", {
  # Test dataframe
  input_data <- data.frame(
    date = as.Date("2000-01-01") + 0:9,
    feature1 = rnorm(10, 0, 1),
    feature2 = rnorm(10, 0, 1),
    feature_target = rnorm(10, 0, 1)
  )

  result <- feature_selection(input_data, "feature_target")
  result2 <- feature_selection(input_data, "feature_target", c("feature1"))

  # Check that the result is a dataframe
  expect_is(result, "data.frame")

  # Check that the expected columns are present
  expected_cols <- c("date", "feature1", "feature_target", "month", "dayofweek", "weekend", "lag_1", "lag_2", "lag_3", "lag_4", "lag_5", "rolling_mean", "rolling_std", "trend", "spike", "linearity", "curvature", "e_acf1", "entropy")
  expect_equal(colnames(result2), expected_cols)

  # Check that the function returns a warning if input_data is empty
  expect_warning(feature_selection(data.frame(), "feature_target"))

  # Check that the function returns a warning if the input_data is not a dataframe
  expect_warning(feature_selection("not a dataframe", "feature_target"))
})
