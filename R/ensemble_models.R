#' Génère des sous-groupes à partir d'un dataset
#'
#' @param dataset_input données d'entrée
#' @param var_date date variable
#'
#' @return renvoie une liste de sous-ensembles
#' @export
th2_resamples <- function(dataset_input, var_date) {
  dataset_date <- dataset_input
  dataset_date$date <- as.POSIXct(dataset_date[[var_date]])
  diff_date <- diff(dataset_date$date)

  unit_date <- attr(diff_date, "units")

  start_date <- min(dataset_date$date)
  end_date <- max(dataset_date$date)

  if (unit_date == "hours") {
    interval_dates <- lubridate::interval(start_date, end_date) / lubridate::hours(1)
  } else if (unit_date == "days") {
    interval_dates <- lubridate::interval(start_date, end_date) / lubridate::days(1)
  }

  assess_var <- paste(as.character(floor(interval_dates * 0.2)), unit_date)
  innitial_var <- paste(as.character(floor(interval_dates * 0.4)), unit_date)
  skip_var <- paste(as.character(floor(interval_dates * 0.1)), unit_date)

  resamples_tscv <- modeltime.resample::time_series_cv(
    data        = dataset_input,
    assess      = assess_var,
    initial     = innitial_var,
    skip        = skip_var,
    slice_limit = 4
  )

  return(resamples_tscv)
}


#' Effectue le réglage fin d'un modèle en fonction d'hyperparamètres
#'
#' @param resample_data sous-ensembles
#' @param model modèle entraîné
#' @param tuning_param liste des hyperparamètres
#'
#' @return un modèle affiné
#' @export
th2_tune_model <- function(resample_data, model, tuning_param) {
  # tuning_param arrive sous forme de liste nommee de valeurs candidates par
  # hyperparametre (ex. list(non_seasonal_ar = c(1, 2), ...)) ; tune::tune_grid
  # exige un entier positif ou une grille (data.frame) avec une colonne par
  # hyperparametre : on la materialise ici via expand.grid().
  grid <- if (is.data.frame(tuning_param)) {
    tuning_param
  } else {
    as.data.frame(do.call(expand.grid, tuning_param))
  }

  cv_results <- tune::tune_grid(
    model,
    grid = grid,
    metrics = yardstick::metric_set(yardstick::rmse),
    resamples = resample_data,
    control = tune::control_resamples(
      verbose = FALSE,
      save_pred = FALSE,
      allow_par = TRUE
    )
  )

  best_params <- cv_results %>%
    tune::select_best(metric = "rmse")
  # tune::select_best('rmse', maximise = FALSE)

  return(best_params)
}

#' Génère une chaîne de modèles ajustés
#'
#' @param dataset_input données d'entrée
#' @param dataset_split données pour l'évaluation et la formation
#' @param var_date date var
#' @param var_target targget
#' @param models liste des modèles
#' @param ensamble_type type d'assemblée
#'
#' @return renvoie un ensemble de plusieurs modèles entraînés
#' @export
#' @examples
th2_ensemble_engine <- function(dataset_input, var_date, var_target, models, list_features = c(), ensamble_type = "mean", use_holidays = NULL) {
  list_output_models <- list()
  error_models <- NULL

  data_features <- feature_selection(dataset_input, var_target, list_features, use_holidays = use_holidays)
  data_features <- data_features[complete.cases(data_features), ]

  dataset_split <- split_dataset(data_features, var_date, var_target)$traintest

  # th2_random_forest_engine()/th2_xgboost_engine() batissent leur propre
  # recipe de feature engineering (step_th2_feature_engineering) a partir des
  # colonnes brutes : leur donner les donnees deja passees par
  # feature_selection() (dataset_split/resample_data ci-dessous) fait
  # echouer prep() avec "All columns selected for the step should be date or
  # datetime" des que la recipe applique all_predictors() aux colonnes
  # derivees (lag/index.num/...). On leur garde donc un split et des
  # resamples "bruts", coherent avec leur usage partout ailleurs dans le
  # depot (ex. R/bulk_forecastin_spark.R, qui leur passe toujours des
  # donnees non feature-engineerees).
  raw_split <- split_dataset(dataset_input, var_date, var_target)$traintest

  # input_data <- dataset_input
  resample_data <- th2_resamples(data_features, var_date)
  resample_data_raw <- th2_resamples(dataset_input, var_date)

  for (model in models) {
    # tune -- for
    tuning_param <- ""
    training_model <- NULL
    label_model <- ""
    is_raw_engine <- model %in% c("random_forest", "xgboost")

    if (model == "arima") {
      training_model <- th2_arima_engine(rsample::training(dataset_split), var_target, var_date, fit_model = FALSE)
      formula <- as.formula(paste(var_target, "~", var_date))
      tuning_param <- list(non_seasonal_ar = seq(1, 2, 3), non_seasonal_differences = seq(0, 1, 2), non_seasonal_ma = seq(1, 2, 3))
      label_model <- "model_arima"
    } else if (model == "prophet") {
      training_model <- th2_prophet_engine(rsample::training(dataset_split), var_target, var_date, engine = "prophet", use_holidays = use_holidays, fit_model = FALSE)
      formula <- as.formula(paste(var_target, "~", var_date))
      tuning_param <- list(changepoint_num = seq(10, 15, 25), changepoint_range = seq(0.6, 0.7, 0.8))
      label_model <- "model_prophet"
    } else if (model == "lr") {
      training_model <- th2_linear_engine(rsample::training(dataset_split), var_target, var_date, fit_model = FALSE)
      # formula <- as.formula(paste(var_target, "~", "as.numeric(",var_date,") + factor(month(",var_date,", label = TRUE), ordered = FALSE)"))
      formula <- as.formula(paste(var_target, "~", var_date))
      tuning_param <- FALSE
      label_model <- "model_lm"
    } else if (model == "mars") {
      training_model <- th2_mars_engine(rsample::training(dataset_split), var_target, var_date, fit_model = FALSE)
      formula <- as.formula(paste(var_target, "~", var_date))
      tuning_param <- list(num_terms = seq(5, 10, 20), prod_degree = seq(2, 4, 10))
      label_model <- "model_mars"
    } else if (model == "random_forest") {
      training_model <- th2_random_forest_engine(rsample::training(raw_split), var_target, use_holidays = use_holidays, fit_model = FALSE)
      formula <- as.formula(paste(var_target, "~ ."))
      tuning_param <- list(min_n = seq(2, 4, 1), trees = seq(20, 200, 400))
      label_model <- "model_random_forest"
    } else if (model == "xgboost") {
      training_model <- th2_xgboost_engine(rsample::training(raw_split), var_date, var_target, use_holidays = use_holidays, fit_model = FALSE)
      formula <- as.formula(paste(var_target, "~ ."))
      tuning_param <- list(mtry = seq(2, 4, 10), trees = seq(50, 150, 400), min_n = seq(1, 5, 10), learn_rate = seq(0.01, 0.1, 0.2))
      label_model <- "model_xgboost"
    } else {
      error_models <- c(error_models, model)
    }

    best_params <- th2_tune_model(if (is_raw_engine) resample_data_raw else resample_data, training_model$fit, tuning_param)

    fitted_model <- training_model$model %>%
      tune::finalize_model(best_params) %>%
      parsnip::fit(formula, data = rsample::training(if (is_raw_engine) raw_split else dataset_split))

    list_output_models[[label_model]] <- fitted_model
  }

  model_table <- do.call(modeltime::modeltime_table, list_output_models)

  ensemble_fit <- model_table %>%
    modeltime.ensemble::ensemble_average(type = ensamble_type)

  calibration_tbl <- modeltime_table(
    ensemble_fit
  ) %>%
    modeltime::modeltime_calibrate(rsample::testing(dataset_split), quiet = FALSE)

  refit_tbl <- calibration_tbl %>%
    modeltime::modeltime_refit(data_features)

  return(refit_tbl)
}
