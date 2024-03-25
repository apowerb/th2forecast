library(testthat)
library(naniar)
library(janitor)

test_that("test preprocessing_data function", {

  # test dataframe
  input_data <- data.frame(
    a = c(1, 2, NA, 4, 5),
    b = c("a", "b", "c", "d", NA),
    stringsAsFactors = FALSE
  )

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
