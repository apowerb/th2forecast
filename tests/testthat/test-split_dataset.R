test_that("test split_dataset function", {
  # Crear un dataframe de prueba
  input_data <- data.frame(
    dteday = seq(as.Date("2020/1/1"), by = "month", length.out = 100),
    cnt = rnorm(100),
    other_var = rnorm(100),
    stringsAsFactors = FALSE
  )

  # Aplicar la función split_dataset
  result <- split_dataset(input_data, "dteday", "cnt")

  # Comprobar que el resultado es una lista
  expect_is(result, "list")

  # Comprobar que la lista tiene dos elementos
  expect_equal(length(result), 2)

  # Comprobar que el primer elemento es una lista
  expect_s3_class(result$traintest, "ts_cv_split")

  # Comprobar que el segundo elemento es un dataframe
  expect_is(result$data_selected, "data.frame")

  # Comprobar que el dataframe tiene las columnas 'dteday' y 'cnt'
  expect_true(all(c("dteday", "cnt") %in% names(result$data_selected)))

  # Errors

  # check if the selected items exist in the dataframe
  expect_warning(split_dataset(input_data, "dteday", "cntaaa"))
  expect_warning(split_dataset(input_data, "dteday", 56))

  # Check that the input_data is a dataframe
  expect_warning(split_dataset(0, "dteday", "cnt"))

  # Check that the input_data is not empty
  input_data_empty = data.frame()
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
