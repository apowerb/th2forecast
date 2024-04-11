library(timetk)

data <- m4_monthly %>% dplyr::filter(id == "M750")
data <- split_dataset(data, "date", "value")$traintest

# # Test for th2_arima_engine
# test_that("th2_arima_engine returns an ARIMA model", {
#   model_arima <- th2_arima_engine(data, "value", "date", engine="auto_arima")
#   expect_is(model_arima, "model_fit")
# })

# Test for th2_prophet_engine
test_that("th2_prophet_engine returns a Prophet model", {
  model_prophet <- th2_prophet_engine(data, "value", "date", engine = "prophet")
  expect_is(model_prophet, "model_fit")

  expect_warning(th2_prophet_engine(data, "valor", "date", engine = "prophet"))
})

# Test for th2_linear_engine
test_that("th2_linear_engine returns a linear regression model", {
  model_linear <- th2_linear_engine(data, "value", "date", engine = "lm")
  expect_is(model_linear, "model_fit")

  expect_warning(th2_linear_engine(data, "valor", "date", engine = "lm"))
})

# Test for th2_mars_engine
test_that("th2_mars_engine returns a MARS model", {
  model_mars <- th2_mars_engine(
    data,
    "value",
    "date",
    engine = "earth",
    mars_features = "month")
  expect_is(model_mars, "workflow")

  expect_warning(th2_mars_engine(
    data,
    "valor",
    "date",
    engine = "earth",
    mars_features = "month")
    )
})

# Test for model_selection_train
test_that("model_selection_train returns a modeltime table", {

  # Test outputs
  model_table <- model_selection_train(
    data, c("prophet", "lr", "mars"), "value", "date"
    )

  # Test for classes of model_table
  expect_s3_class(model_table, c("mdl_time_tbl", "tbl_df", "tbl", "data.frame"))

  # Test errors
  expect_warning(
    model_selection_train(
      data, c("prophet", "lr", "mars"), "valores", "date")
    )

  expect_warning(
    model_selection_train(
      data, c(), "value", "date")
    )

  expect_warning(
    model_selection_train(
      data, 123, "value", "date")
    )

  expect_warning(
    model_selection_train(
      data, c("profeta", "lr", "marte"), "value", "date")
    )

  dataframe_test <- data.frame()
  expect_warning(
    model_selection_train(
      dataframe_test, c("prophet", "lr", "mars"), "value", "date")
    )

})
