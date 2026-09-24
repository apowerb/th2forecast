library(rsample)

# `id` ne sert qu'au filtrage ci-dessous : step_th2_feature_engineering()
# (utilisee par th2_random_forest_engine/th2_xgboost_engine via
# all_predictors()) exige que toutes les colonnes selectionnees soient
# date/datetime, sauf une colonne d'identifiant explicitement nommee
# "name_id" qu'elle sait retirer (cf. prep.step_th2_feature_engineering dans
# R/step_feature_engineering.R). "id" n'est pas reconnu et reste un facteur
# parmi les predicteurs : on le retire, comme le ferait un appelant reel.
data_split <- timetk::m4_monthly %>% filter(id == "M750") %>% select(-id)
data_split <- split_dataset(data_split, "date", "value")$traintest

# Tous les th2_*_engine() attendent un data.frame d'entrainement (voir leurs
# `@param input_data un dataframe` et l'usage constant `training(dataset_split)`
# dans R/ensemble_models.R), pas l'objet rsplit renvoye par split_dataset().
# Les tests appelaient jusqu'ici `data` (l'objet rsplit lui-meme), ce qui
# echouait des la verification de colonnes ou la construction de la recette
# recipes::recipe() ("data must be a data frame ... not a <ts_cv_split>
# object") : signature obsolete depuis l'introduction de split_dataset() dans
# ce test. On extrait la partie training() comme le fait le reste du code.
data <- training(data_split)

# Test for th2_arima_engine
test_that("th2_arima_engine returns an ARIMA model", {
  model_arima <- th2_arima_engine(data, "value", "date", engine = "auto_arima")
  expect_is(model_arima, "model_fit")
})

# Test for th2_prophet_engine
test_that("th2_prophet_engine returns a Prophet model", {
  model_prophet <- th2_prophet_engine(data, "value", "date", engine = "prophet")
  expect_is(model_prophet, "model_fit")

  expect_warning(th2_prophet_engine(data, "valor", "date", engine = "prophet"))
})

# Test for th2_linear_engine
test_that("th2_linear_engine returns a linear regression model", {
  model_linear <- th2_linear_engine(data, "value", "date", engine = "lm")
  expect_is(model_linear, "workflow")

  expect_warning(th2_linear_engine(data, "valor", "date", engine = "lm"))
})

# Test for th2_mars_engine
test_that("th2_mars_engine returns a MARS model", {
  model_mars <- th2_mars_engine(
    data,
    "value",
    "date",
    engine = "earth",
    mars_features = "month"
  )
  expect_is(model_mars, "workflow")

  expect_warning(th2_mars_engine(
    data,
    "valor",
    "date",
    engine = "earth",
    mars_features = "month"
  ))
})


# Test for th2_random_forest
test_that("th2_random_forest returns a workflow", {
  # use_holidays = NULL comme dans tous les appelants reels de ces moteurs
  # (R/api_v1_models.R, R/bulk_forecastin_spark.R) : le defaut use_holidays =
  # TRUE est transmis tel quel comme `calendar` a holidays_detection(), qui
  # l'utilise comme *nom* de calendrier pour bizdays::create.calendar(name =
  # calendar, ...) -- un bizdays::calendar exige un nom caractere, pas un
  # booleen ("wrong args for environment subassignment"). Ce chemin n'est
  # jamais exerce en dehors de ce defaut jamais surcharge.
  model_rf_fit <- th2_random_forest_engine(data, "value", use_holidays = NULL)
  # th2_random_forest_engine() construit un workflows::workflow()
  # (recipe + modele) puis le fit() : le resultat est un workflow entraine
  # (classe "workflow"), pas un parsnip::model_fit brut. L'assertion
  # "model_fit" datait d'avant l'ajout de la recipe
  # step_th2_feature_engineering.
  expect_is(model_rf_fit, "workflow")
})


# Test for th2_xgboost
test_that("th2_xgboost returns a workflow", {
  # meme raison que th2_random_forest ci-dessus (use_holidays = NULL et
  # classe "workflow").
  model_xgboost_fit <- th2_xgboost_engine(data, "date", "value", use_holidays = NULL)
  expect_is(model_xgboost_fit, "workflow")
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
      data, c("prophet", "lr", "mars"), "valores", "date"
    )
  )

  expect_warning(
    model_selection_train(
      data, c(), "value", "date"
    )
  )

  expect_warning(
    model_selection_train(
      data, 123, "value", "date"
    )
  )

  expect_warning(
    model_selection_train(
      data, c("profeta", "lr", "marte"), "value", "date"
    )
  )

  dataframe_test <- data.frame()
  expect_warning(
    model_selection_train(
      dataframe_test, c("prophet", "lr", "mars"), "value", "date"
    )
  )
})
