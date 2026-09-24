test_that("test feature_selection function", {
  # Test dataframe
  input_data <- data.frame(
    date = as.Date("2000-01-01") + 0:9,
    feature1 = rnorm(10, 0, 1),
    feature2 = rnorm(10, 0, 1),
    feature_target = rnorm(10, 0, 1)
  )

  result <- feature_selection(input_data, "feature_target")
  result2 <- feature_selection(input_data, "feature_target", c("feature1"))

  # Check that the result is a dataframe
  expect_is(result, "data.frame")

  # `expected_cols` datait d'une implementation plus simple de
  # feature_selection() (features mensuelles/jour-de-semaine "maison" +
  # colonnes tsfeatures trend/spike/...). L'implementation actuelle (voir
  # R/feature_selection.R) construit sa signature de date via
  # timetk::tk_get_timeseries_signature() (d'ou index.num/year/month.xts/...),
  # nomme les lags "<target>_lag<i>" et n'appelle jamais th2_tsfeatures() : ces
  # colonnes tsfeatures n'ont jamais existe dans la sortie reelle. On aligne
  # l'attendu sur le contrat reellement produit et exerce ailleurs (ex.
  # th2_random_forest_engine/th2_xgboost_engine via step_th2_feature_engineering).
  expected_cols <- c(
    "date", "feature_target", "feature1",
    "index.num", "year", "year.iso", "half", "quarter", "month", "month.xts",
    "day", "hour", "wday", "wday.xts", "mday", "qday", "yday", "mweek",
    "week", "week.iso", "week2", "week3", "week4", "mday7",
    paste0("feature_target_lag", 1:5),
    "rolling_mean", "rolling_std"
  )
  expect_equal(colnames(result2), expected_cols)

  # Check that the function returns a warning if input_data is empty
  expect_warning(feature_selection(data.frame(), "feature_target"))

  # Check that the function returns a warning if the input_data is not a dataframe
  expect_warning(feature_selection("not a dataframe", "feature_target"))
})
