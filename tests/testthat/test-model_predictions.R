library(timetk)
library(modeltime)
library(tidymodels)

data_row <- m4_monthly %>% filter(id == "M750")
data <- time_series_split(data_row, assess = "3 months", cumulative = TRUE)

#Prophet
model_prophet <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>%
  fit(value ~ date, training(data))

# Machine learning GLM
model_glment <- linear_reg(penalty = 0.01) %>%
  set_engine("glmnet") %>%
  fit(
    value ~ wday(date, label = TRUE)
    + month(date, label = TRUE)
    + as.numeric(date),
    training(data)
  )

model_table <- modeltime_table(
  model_prophet,
  model_glment
)

calib_tbl <- model_table %>%
  modeltime_calibrate(testing(data))

test_that("test model_predictions function", {
  # Check if the function returns an error when input data is NULL
  expect_warning(prediction_forecast(NULL, calib_tbl, h = "3 months"))

  # Check if the function returns an error when the model is NULL
  expect_error(prediction_forecast(data_row, NULL, h = "3 months"))

  # Check whether the function returns a tibble
  result <- prediction_forecast(data_row, calib_tbl, h = "3 months")
  expect_is(result, "tbl_df")

  # Check if the returned tibble has the expected columns
  result <- prediction_forecast(data_row, calib_tbl, h = "3 months")
  expect_named(result, c(".model_id", ".model_desc", ".key", ".index", ".value", ".conf_lo", ".conf_hi"))
})
