library(modeltime)
library(rsample)
# tune::tune_grid() doit pouvoir retrouver time_series_cv() par son nom dans
# le search path (rsample::.get_split_args() -> .find_resampling_function()) :
# un simple modeltime.resample::time_series_cv() dans R/ensemble_models.R ne
# suffit pas, il faut que le package soit attache ici (meme contrainte que
# modeltime/parsnip, voir tests/testthat/setup.R).
library(modeltime.resample)

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
    non_seasonal_ar =parsnip::tune(),
    non_seasonal_differences =parsnip::tune(),
    non_seasonal_ma =parsnip::tune()
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
  # Create a test data set ; `id` (facteur constant ici) n'est pas reconnu
  # par step_th2_feature_engineering (cf. test-selection_training_models.R) :
  # on le retire avant de fournir les donnees a th2_ensemble_engine.
  data_test <- m750 %>% dplyr::select(-id)

  # Define the models and type of assembly
  models_test <- c("prophet", "random_forest")
  ensamble_type_test <- "mean"

  # th2_ensemble_engine(dataset_input, var_date, var_target, models,
  # list_features = c(), ensamble_type = "mean", use_holidays = NULL) fait
  # elle-meme feature_selection() + split_dataset() en interne (voir
  # R/ensemble_models.R) : elle attend les donnees brutes, pas un dataset deja
  # feature-engineere ni un split precalcule. L'appel precedent passait un
  # split rsample en 2e position (var_date) et la liste de modeles en
  # list_features, ce qui faisait echouer feature_selection() en cherchant des
  # colonnes "prophet"/"random_forest" inexistantes : signature obsolete
  # depuis le refactor qui a interiorise le split.
  res <- th2_ensemble_engine(data_test, "date", "value", models_test, ensamble_type = ensamble_type_test)

  # Checks that the function returns an object of the correct class
  expect_s3_class(res, c("modeltime_table", "tbl_df", "tbl", "data.frame"))
})
