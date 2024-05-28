library(modeltime)
library(rsample)

test_that("test th2_resamples function", {
  # Create a test data set
  data_test <- m750

  # Apply the function to data set
  res <- th2_resamples(data_test, "date")

  # Check that the function returns a list
  expect_s3_class(res, c("time_series_cv", "rset", "tbl_df", "tbl", "data.frame"))

  # Check that the list contains the correct number of items.
  expect_equal(nrow(res), 4)
})

test_that("test th2_tune_model function", {
  # Create a test data set
  data_test <- m750

  # We generate resample sets
  resample_data <- th2_resamples(data_test, "date")

  # Generate model
  model_test <- modeltime::arima_reg(
    non_seasonal_ar = tune(),
    non_seasonal_differences = tune(),
    non_seasonal_ma = tune()
  ) %>%
    parsnip::set_engine(engine = "auto_arima")

  formula <- as.formula(paste("value", "~", "date"))

  model_test_fit <- workflows::workflow() %>%
    workflows::add_recipe(recipes::recipe(formula, data = data_test)) %>%
    workflows::add_model(model_test)

  # Define hyperparameters
  params_test <- list(non_seasonal_ar = seq(1, 2, 3), non_seasonal_differences = seq(0, 1, 2), non_seasonal_ma = seq(1, 2, 3))

  # Apply the function
  res <- th2_tune_model(resample_data, model_test_fit, params_test)

  # Checks that the class that the function returns
  expect_s3_class(res, c("tbl_df", "tbl", "data.frame"))

  # Checks that the result contains the correct columns
  expect_equal(colnames(res), c("non_seasonal_ar", "non_seasonal_differences", "non_seasonal_ma", ".config"))
})


test_that("test th2_ensemble_engine function", {
  # Create a test data set
  data_test <- m750

  data_features <- feature_selection(data_test, "value", c())
  data_features <- data_features[complete.cases(data_features), ]

  split_data <- split_dataset(data_features, "date", "value")$traintest

  # Define the models and type of assembly
  models_test <- c("prophet", "random_forest")
  ensamble_type_test <- "mean"

  # Applies the function to data, models and assembly type
  res <- th2_ensemble_engine(data_features, split_data, "date", "value", models_test, ensamble_type_test)

  # Checks that the function returns an object of the correct class
  expect_s3_class(res, c("modeltime_table", "tbl_df", "tbl", "data.frame"))
})
