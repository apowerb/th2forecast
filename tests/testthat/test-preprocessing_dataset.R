library(testthat)
library(naniar)
library(janitor)

test_that("test preprocessing_data function", {

  input_data <- m4_monthly %>% dplyr::filter(id == "M750")

  result <- preprocessing_data(input_data)

  # Check that the result is a list
  expect_is(result, "list")

  # Check that the list has two elements
  expect_equal(length(result), 2)

  # Check that the first element is a dataframe
  expect_is(result$dataset_clean, "data.frame")

  # Check that the dataframe has no NA values
  expect_true(!any(is.na(result$dataset_clean)))

  # Check that the second element is a integer
  expect_is(result$numnber_missing, "integer")

  # Check that the input_data is a dataframe
  expect_warning(preprocessing_data(1))

  # Check that the input_data is not empty
  input_data_empty <- data.frame()
  expect_warning(preprocessing_data(input_data_empty))

})



test_that("anomaly_detection works correctly", {
  # Test dataset
  # set.seed(123)
  # input_data <- data.frame(
  #   date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day"),
  #   value = rnorm(366)
  # )

  input_data <- m4_monthly %>% dplyr::filter(id == "M750")

  # Execute the function
  result <- anomaly_detection(input_data)

  # Check that the result has the same dimensions as the input data
  expect_equal(dim(result), dim(input_data))

  # Check that the date column is still the same
  expect_equal(result$date, input_data$date)

  # Check that the values are numeric
  expect_true(is.numeric(result$value))

  # Check that the input_data is a dataframe
  expect_warning(anomaly_detection(1))

  # Check that the input_data is not empty
  input_data_empty <- data.frame()
  expect_warning(anomaly_detection(input_data_empty))
})



library(testthat)

test_that("outliers_detection function test", {
  # Test dataset
  input_data <- m4_monthly %>% dplyr::filter(id == "M750")

  # Execute the function with the test data
  result <- outliers_detection(input_data, method_ls = "cpt")

  # Check that the output is a data.frame
  expect_is(result, "data.frame")

  # Check that the dimensions of the outlet are correct
  expect_equal(dim(result), dim(input_data))

  # Check that the columns of the output are as expected
  expect_equal(names(result), names(input_data))

  # Check that the values in the 'value' column have changed
  expect_false(all(input_data$value == result$value))

  # Check that the values in the 'date' column have not changed
  expect_true(all(input_data$date == result$date))

  # Check that the input_data is a dataframe
  expect_warning(outliers_detection(1))

  # Check that the input_data is not empty
  input_data_empty <- data.frame()
  expect_warning(outliers_detection(input_data_empty))

  expect_warning(outliers_detection(input_data, method_ls = "test_u"))

})


