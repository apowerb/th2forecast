test_that("test feature_selection function", {
  # Crear un dataframe de prueba
  input_data <- data.frame(
    a = c(1, 2, 3, 4, 5),
    b = c(1, 1, 1, 1, 1),
    c = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  # Aplicar la función feature_selection
  result <- feature_selection(input_data)

  # Comprobar que el resultado es un dataframe
  expect_is(result, "data.frame")

  # Comprobar que el dataframe no tiene la columna 'b'
  expect_false("b" %in% names(result))

  # Comprobar que el dataframe tiene las columnas 'a' y 'c'
  expect_true(all(c("a", "c") %in% names(result)))

  # Check that the input_data is a dataframe
  expect_warning(preprocessing_data(1))

  # Check that the input_data is not empty
  input_data_empty <- data.frame()
  expect_warning(preprocessing_data(input_data_empty))
})
