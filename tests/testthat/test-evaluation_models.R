library(timetk)
library(modeltime)
library(tidymodels)

data <- m4_monthly %>% filter(id == "M750")
data <- time_series_split(data, assess = "3 months", cumulative = TRUE)

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


test_that("test evaluation_models function", {
  result <- model_evaluation(data, model_table)

  # Check that the function returns a list
  expect_is(result, "list")

  # Check that the returned list has two elements
  expect_length(result, 2)

  # Check that the first element of the list is a tibble
  expect_is(result[[1]], "tbl_df")

  # Check that the second element of the list is a tibble
  expect_is(result[[2]], "tbl_df")

  # Check that the function fails correctly if the input data is not a tibble
  expect_error(model_evaluation("not a tibble", model_table))

  # Check that the function fails correctly if model_table is not a tibble
  expect_error(model_evaluation(input_data, "not a tibble"))

  # Check errors
  data_test_error <- data.frame()
  expect_error(model_evaluation(data_test_error, model_table))

})
