test_that("test feature_selection function", {
  # Test dataframe
  input_data <- data.frame(
    a = c(1, 2, 3, 4, 5),
    b = c(1, 1, 1, 1, 1),
    c = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  # Apply the feature_selection function
  result <- feature_selection(input_data)

  # Check that the result is a dataframe
  expect_is(result, "data.frame")

  # Check that the dataframe does not have column 'b'
  expect_false("b" %in% names(result))

  # Check that the dataframe has columns 'a' and 'c'
  expect_true(all(c("a", "c") %in% names(result)))

  # Check that the input_data is a dataframe
  expect_warning(preprocessing_data(1))

  # Check that the input_data is not empty
  input_data_empty <- data.frame()
  expect_warning(preprocessing_data(input_data_empty))
})
