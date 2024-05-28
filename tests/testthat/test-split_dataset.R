test_that("test split_dataset function", {
  # Test dataframe
  input_data <- data.frame(
    dteday = seq(as.Date("2020/1/1"), by = "month", length.out = 100),
    cnt = rnorm(100),
    other_var = rnorm(100),
    stringsAsFactors = FALSE
  )

  # Apply the split_dataset function
  result <- split_dataset(input_data, "dteday", "cnt")

  # Check that the result is a list
  expect_is(result, "list")

  # Check that the list has two elements
  expect_equal(length(result), 2)

  # Check that the first element is a list
  expect_s3_class(result$traintest, "ts_cv_split")

  # Check that the second element is a dataframe
  expect_is(result$data_selected, "data.frame")

  # Check that the dataframe has the columns 'dteday' and 'cnt'
  expect_true(all(c("dteday", "cnt") %in% names(result$data_selected)))

  # Errors

  # check if the selected items exist in the dataframe
  expect_warning(split_dataset(input_data, "dteday", "cntaaa"))
  expect_warning(split_dataset(input_data, "dteday", 56))

  # Check that the input_data is a dataframe
  expect_warning(split_dataset(0, "dteday", "cnt"))

  # Check that the input_data is not empty
  input_data_empty <- data.frame()
  expect_warning(split_dataset(input_data_empty, "dteday", "cnt"))

  # check that the dataframe is not smaller than 5
  input_data_few <- data.frame(
    dteday = seq(as.Date("2020/1/1"), by = "month", length.out = 3),
    cnt = rnorm(3),
    other_var = rnorm(3),
    stringsAsFactors = FALSE
  )
  expect_warning(split_dataset(input_data_few, "dteday", "cnt"))
})
