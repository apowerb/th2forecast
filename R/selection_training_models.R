# ======Models======

#' Création et entraînement du modèle Arima
#'
#' @param input_data Un dataframe qui contient les données à utiliser pour l'entraînement du modèle.
#' @param var_target Le nom de la colonne target dans le dataframe.
#' @param var_date Le nom de la colonne qui contient les dates dans le dataframe.
#' @param engine Le moteur à utiliser pour le modèle arime_reg. Par défaut, c'est "auto_arima".
#'
#' @return Un modèle Arima entraîné.
#' @export
#'
#' @examples th2_arima_engine(input_data, "value", "datetime", engine = "auto_arima")
th2_arima_engine <- function(input_data, var_target, var_date, engine = "auto_arima", p = 1, d = 1, q = 1, fit_model = TRUE) {
  if (!(var_date %in% colnames(input_data) && var_target %in% colnames(input_data))) {
    return(warning("Selected variables do not exist in the data."))
  } else {
    formula <- as.formula(paste(var_target, "~", var_date))

    set.seed(1234)
    model_arima <- modeltime::arima_reg(
      non_seasonal_ar = ifelse(fit_model == FALSE, tune(), p),
      non_seasonal_differences = ifelse(fit_model == FALSE, tune(), d),
      non_seasonal_ma = ifelse(fit_model == FALSE, tune(), q)
    ) %>%
      parsnip::set_engine(engine = engine)

    # %>%
    # parsnip::fit(formula, data = training(input_data))

    if (fit_model == TRUE) {
      model_arima_fit <- model_arima %>%
        parsnip::fit(formula, data = input_data)
    } else if (fit_model == "bulk") {
      recipe_arima <- recipes::recipe(formula, data = input_data)

      model_arima_fit <- workflows::workflow() %>%
        workflows::add_recipe(recipe_arima) %>%
        workflows::add_model(model_arima)

      model_arima_fit <- list("fit" = model_arima_fit, "model" = model_arima)
    } else {
      model_arima_fit <- workflows::workflow() %>%
        workflows::add_recipe(recipes::recipe(formula, data = input_data)) %>%
        workflows::add_model(model_arima)

      model_arima_fit <- list("fit" = model_arima_fit, "model" = model_arima)
    }
    return(model_arima_fit)
  }
}


#' Création et entraînement du modèle Prophète
#'
#' @param input_data Un dataframe qui contient les données à utiliser pour l'entraînement du modèle.
#' @param var_target Le nom de la colonne target dans le dataframe.
#' @param var_date Le nom de la colonne qui contient les dates dans le dataframe.
#' @param engine Le moteur à utiliser pour le modèle Prophet. Par défaut, c'est "prophet".
#'
#' @return Un modèle Prophet entraîné.
#' @export
#'
#' @examples th2_prophet_engine(input_data, "value", "datetime", engine = "prophet")
th2_prophet_engine <- function(input_data, var_target, var_date, engine = "prophet", changepoint_num = 25, changepoint_range = 0.8, use_holidays = TRUE, fit_model = TRUE, db_conn = NULL) {
  if (!(var_date %in% colnames(input_data) && var_target %in% colnames(input_data))) {
    return(warning("Selected variables do not exist in the data."))
  } else {
    formula <- as.formula(paste(var_target, "~", var_date))

    holidays_df <- NULL
    if (!is.null(use_holidays) && use_holidays != "in_data") {
      holidays_df <- holidays_detection(input_data, model = "prophet", calendar = use_holidays, db_conn = db_conn)
    }

    model_prophet <- modeltime::prophet_reg(
      changepoint_num = ifelse(fit_model == FALSE, tune(), changepoint_num),
      changepoint_range = ifelse(fit_model == FALSE, tune(), changepoint_range)
    ) %>%
      parsnip::set_engine(engine = engine, holidays = holidays_df)

    if (fit_model == TRUE) {
      model_prophet_fit <- model_prophet %>%
        parsnip::fit(formula, data = input_data)
    } else if (fit_model == "bulk") {
      recipe_prophet <- recipes::recipe(formula, data = input_data)

      model_prophet_fit <- workflows::workflow() %>%
        workflows::add_recipe(recipe_prophet) %>%
        workflows::add_model(model_prophet)

      model_prophet_fit <- list("fit" = model_prophet_fit, "model" = model_prophet)
    } else {
      model_prophet_fit <- workflows::workflow() %>%
        workflows::add_recipe(recipes::recipe(formula, data = input_data)) %>%
        workflows::add_model(model_prophet)

      model_prophet_fit <- list("fit" = model_prophet_fit, "model" = model_prophet)
    }

    return(model_prophet_fit)
  }
}

#' Création et entraînement du modèle de régression linéaire
#'
#' @param input_data Un dataframe qui contient les données à utiliser pour l'entraînement du modèle.
#' @param var_target Le nom de la colonne target dans le dataframe.
#' @param var_date Le nom de la colonne qui contient les dates dans le dataframe.
#' @param engine Le moteur à utiliser pour le modèle linear_reg. Par défaut, c'est "lm".
#'
#' @return Un modèle de régression linéaire entraîné.
#' @export
#'
#' @examples th2_linear_engine(input_data, "value", "datetime", engine = "lm")
th2_linear_engine <- function(input_data, var_target, var_date, engine = "lm", fit_model = TRUE) {
  if (!(var_date %in% colnames(input_data) && var_target %in% colnames(input_data))) {
    return(warning("Selected variables do not exist in the data."))
  } else {
    formula <- as.formula(paste(var_target, "~", var_date))

    model_linear <- parsnip::linear_reg() %>%
      parsnip::set_engine(engine = engine) %>%
      parsnip::set_mode("regression")

    recipe_spec <- recipes::recipe(formula, data = input_data) %>%
      recipes::step_date(var_date, features = "month", ordinal = FALSE) %>%
      recipes::step_mutate(date_num = as.numeric(!!sym(var_date))) %>%
      recipes::step_normalize(date_num) %>%
      recipes::step_rm(var_date)


    if (fit_model == TRUE) {
      model_linear_fit <- workflows::workflow() %>%
        workflows::add_recipe(recipe_spec) %>%
        workflows::add_model(model_linear) %>%
        parsnip::fit(input_data)
    } else {
      model_linear_fit <- workflows::workflow() %>%
        workflows::add_recipe(recipe_spec) %>%
        workflows::add_model(model_linear)

      model_linear_fit <- list("fit" = model_linear_fit, "model" = model_linear)
    }
    # print(model_linear_fit)

    return(model_linear_fit)
  }
}

#' Création et entraînement du modèle MARs
#'
#' @param input_data Un dataframe qui contient les données à utiliser pour l'entraînement du modèle.
#' @param var_target Le nom de la colonne target dans le dataframe.
#' @param var_date Le nom de la colonne qui contient les dates dans le dataframe.
#' @param engine Le moteur à utiliser pour le modèle mars Par défaut, c'est "earth".
#'
#' @return Un modèle MARs entraîné.
#' @export
#'
#' @examples th2_mars_engine(input_data, "value", "datetime", engine = "earth", mars_features = "month")
th2_mars_engine <- function(input_data, var_target, var_date, engine = "earth", mars_features = "doy", num_terms = 5, prod_degree = 5, fit_model = TRUE) {
  if (!(var_date %in% colnames(input_data) && var_target %in% colnames(input_data))) {
    return(warning("Selected variables do not exist in the data."))
  } else {
    formula <- as.formula(paste(var_target, "~", var_date))

    model_mars <- parsnip::mars(
      num_terms = ifelse(fit_model == FALSE, tune(), num_terms),
      prod_degree = ifelse(fit_model == FALSE, tune(), prod_degree)
    ) %>%
      parsnip::set_engine(engine) %>%
      parsnip::set_mode("regression")

    recipe_spec <- recipes::recipe(formula, data = input_data) %>%
      recipes::step_date(!!var_date, features = mars_features, ordinal = FALSE) %>%
      recipes::step_mutate(date_num = as.numeric(!!sym(var_date))) %>%
      recipes::step_normalize(date_num) %>%
      recipes::step_rm(!!var_date)

    if (fit_model == TRUE) {
      model_fit_mars <- workflows::workflow() %>%
        workflows::add_recipe(recipe_spec) %>%
        workflows::add_model(model_mars) %>%
        parsnip::fit(input_data)
    } else {
      model_fit_mars <- workflows::workflow() %>%
        workflows::add_recipe(recipe_spec) %>%
        workflows::add_model(model_mars)

      model_fit_mars <- list("fit" = model_fit_mars, "model" = model_mars)
    }

    return(model_fit_mars)
  }
}


#' Création et entraînement du modèle Random Forest
#'
#' @param input_data Un dataframe qui contient les données à utiliser pour l'entraînement du modèle.
#' @param var_target Le nom de la colonne target dans le dataframe.
#' @param trees Nombre d'arbres
#'
#' @return Un modèle Random Forest entraîné.
#' @export
#'
#' @examples
th2_random_forest_engine <- function(input_data, var_target, min_n = 5, trees = 500, use_holidays = TRUE, use_meteo = FALSE, fit_model = TRUE, all_data = "", lags = FALSE, db_conn = NULL) {
  model_rf <- parsnip::rand_forest(
    # min_n = ifelse(fit_model, min_n, tune()),
    # trees = ifelse(fit_model, trees, tune())
    min_n = ifelse(fit_model == FALSE, tune(), min_n),
    trees = ifelse(fit_model == FALSE, tune(), trees)
  ) %>%
    parsnip::set_engine("randomForest") %>%
    parsnip::set_mode("regression")

  # Fit a Random Forest model
  formula <- as.formula(paste(var_target, "~ ."))

  if (fit_model == TRUE) {
    # model_rf_fit <- model_rf %>%
    #   parsnip::fit(formula, data = input_data)


    set.seed(1)
    # model_xgboost_fit <- model_xgboost %>%
    #   parsnip::fit(formula, data = input_data)
    recipe_rf <- recipes::recipe(formula, data = input_data) %>%
      step_th2_feature_engineering(recipes::all_predictors(), feature_target = var_target, use_holidays = use_holidays, use_meteo = use_meteo, all_data = all_data, lags = lags, db_conn = db_conn)

    model_rf_fit <- workflows::workflow() %>%
      workflows::add_recipe(recipe_rf) %>%
      workflows::add_model(model_rf) %>%
      parsnip::fit(input_data)
  } else if (fit_model == "bulk") {
    recipe_rf <- recipes::recipe(formula, data = input_data) %>%
      step_th2_feature_engineering(recipes::all_predictors(), feature_target = var_target, use_holidays = use_holidays, use_meteo = use_meteo, all_data = all_data, lags = lags, db_conn = db_conn)

    model_rf_fit <- workflows::workflow() %>%
      workflows::add_recipe(recipe_rf) %>%
      workflows::add_model(model_rf)

    model_rf_fit <- list("fit" = model_rf_fit, "model" = model_rf)
  } else {
    recipe_rf <- recipes::recipe(formula, data = input_data) %>%
      step_th2_feature_engineering(recipes::all_predictors(), feature_target = var_target, use_holidays = use_holidays, use_meteo = use_meteo, all_data = all_data, lags = lags, db_conn = db_conn)

    model_rf_fit <- workflows::workflow() %>%
      workflows::add_recipe(recipe_rf) %>%
      workflows::add_model(model_rf)

    model_rf_fit <- list("fit" = model_rf_fit, "model" = model_rf)
  }

  return(model_rf_fit)
}


#' Création et entraînement du modèle XGBoost
#'
#' @param input_data Un dataframe qui contient les données à utiliser pour l'entraînement du modèle.
#' @param var_target Le nom de la colonne target dans le dataframe.
#' @param trees Nombre d'arbres
#'
#' @return Un modèle XGBoost entraîné.
#' @export
#' @examples
th2_xgboost_engine <- function(input_data, var_date, var_target, mtry = 2, trees = 200, min_n = 5, learn_rate = 0.1, use_holidays = TRUE, use_meteo = FALSE, fit_model = TRUE, all_data = "", lags = FALSE, db_conn = NULL) {
  model_xgboost <-
    parsnip::boost_tree(
      # mtry = ifelse(fit_model == FALSE, tune(), mtry),
      trees = ifelse(fit_model == FALSE, tune(), trees),
      min_n = ifelse(fit_model == FALSE, tune(), min_n),
      learn_rate = ifelse(fit_model == FALSE, tune(), learn_rate)
    ) %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("xgboost")


  # Fit a XGBoost model
  formula <- as.formula(paste(var_target, "~ ."))

  if (fit_model == TRUE) {
    set.seed(1)
    # model_xgboost_fit <- model_xgboost %>%
    #   parsnip::fit(formula, data = input_data)

    recipe_xgboost <- recipes::recipe(formula, data = input_data) %>%
      step_th2_feature_engineering(recipes::all_predictors(), feature_target = var_target, use_holidays = use_holidays, use_meteo = use_meteo, all_data = all_data, lags = lags, db_conn = db_conn) %>%
      recipes::step_rm(var_date)

    model_xgboost_fit <- workflows::workflow() %>%
      workflows::add_recipe(recipe_xgboost) %>%
      workflows::add_model(model_xgboost) %>%
      parsnip::fit(input_data)
  } else if (fit_model == "bulk") {
    recipe_xgboost <- recipes::recipe(formula, data = input_data) %>%
      step_th2_feature_engineering(recipes::all_predictors(), feature_target = var_target, use_holidays = use_holidays, use_meteo = use_meteo, all_data = all_data, lags = lags, db_conn = db_conn) %>%
      recipes::step_rm(var_date)
    model_xgboost_fit <- workflows::workflow() %>%
      workflows::add_recipe(recipe_xgboost) %>%
      workflows::add_model(model_xgboost)

    model_xgboost_fit <- list("fit" = model_xgboost_fit, "model" = model_xgboost)
  } else {
    recipe_xgboost <- recipes::recipe(formula, data = input_data) %>%
      step_th2_feature_engineering(recipes::all_predictors(), feature_target = var_target, use_holidays = use_holidays, use_meteo = use_meteo, all_data = all_data, lags = lags, db_conn = db_conn) %>%
      recipes::step_rm(var_date)

    model_xgboost_fit <- workflows::workflow() %>%
      workflows::add_recipe(recipe_xgboost) %>%
      workflows::add_model(model_xgboost)

    model_xgboost_fit <- list("fit" = model_xgboost_fit, "model" = model_xgboost)

    # th2_recipe_custom <- recipes::recipe(formula, input_data) %>%
    #   # feature_selection(feature_target = var_target) %>%
    #   step_rm(var_date)
    #
    # model_xgboost_fit <- workflows::workflow() %>%
    #     workflows::add_recipe(th2_recipe_custom) %>%
    #     workflows::add_model(model_xgboost)

    # model_xgboost_fit <- list("fit" = model_xgboost_fit, "model"= model_xgboost)
  }

  return(model_xgboost_fit)
}


#' @export
th2_arimax_engine <- function(input_data, var_date, var_target, external_data = NULL, exogenous_var = NULL, engine = "auto_arima", use_holidays = TRUE, fit_model = TRUE) {
  list_features <- c(var_date, exogenous_var)

  expl_var <- ""
  for (i in 1:length(list_features)) {
    expl_var <- paste(expl_var, paste(list_features[i], "+"))
  }
  expl_var <- substr(expl_var, 1, nchar(expl_var) - 1)

  formula <- as.formula(paste(var_target, "~ ."))

  set.seed(1234)
  model_arimax <- modeltime::arima_reg() %>%
    parsnip::set_engine(engine = engine)

  if (fit_model == TRUE) {
    model_arimax_fit <- model_arimax %>%
      parsnip::fit(formula, data = input_data)
  } else if (fit_model == "bulk") {
    recipe_arimax <- recipes::recipe(formula, data = dplyr::select(input_data, -name_id))
    # %>% step_th2_exogenous_variable(recipes::all_predictors(), feature_target = var_target, use_holidays = use_holidays, external_data = external_data,  exogenous_var = exogenous_var)

    model_arimax_fit <- workflows::workflow() %>%
      workflows::add_recipe(recipe_arimax) %>%
      workflows::add_model(model_arimax)
  }
  model_xgboost_fit <- list("fit" = model_arimax_fit, "model" = model_arimax)
}


#' @export
th2_ets_engine <- function(input_data, var_date, var_target, fit_model = TRUE) {
  formula <- as.formula(paste(var_target, "~", var_date))

  model_ets_fit <- modeltime::exp_smoothing() %>%
    parsnip::set_engine(engine = "ets") %>%
    parsnip::fit(formula, data = input_data)

  return(model_ets_fit)
}


# ======Selection and train models
#' Sélection et préparation des modèles
#'
#' @param input_data Un dataframe qui contient les données à utiliser pour l'entraînement du modèle.
#' @param list_models Une liste des modèles à entraîner.
#' @param var_target Le nom de la colonne cible dans le dataframe.
#' @param var_date Le nom de la colonne qui contient les dates dans le dataframe.
#'
#' @return Un tableau qui contient les modèles entraînés.
#' @export
#'
#' @examples model_selection_train(input_data, c("arima", "prophet", "lr", "mars"), "value", "datetime")
model_selection_train <- function(input_data, list_models, var_target, var_date, input_feature_data) {
  if (any(class(input_data) %in% c("tbl_df", "tbl", "data.frame")) || class(input_data) == "data.frame") {
    error_models <- NULL

    if (length(list_models) > 0 && is.character(list_models)) {
      if (!(var_date %in% colnames(input_data) && var_target %in% colnames(input_data))) {
        return(warning("Selected variables do not exist in the data."))
      }

      list_output_models <- list()

      for (model in list_models) {
        if (model == "arima") {
          model_arima <- th2_arima_engine(input_data, var_target, var_date)
          list_output_models[["model_arima"]] <- model_arima
        } else if (model == "prophet") {
          model_prophet <- th2_prophet_engine(input_data, var_target, var_date)
          list_output_models[["model_prophet"]] <- model_prophet
        } else if (model == "lr") {
          model_lm <- th2_linear_engine(input_data, var_target, var_date)
          list_output_models[["model_lm"]] <- model_lm
        } else if (model == "mars") {
          model_mars <- th2_mars_engine(input_data, var_target, var_date)
          list_output_models[["model_mars"]] <- model_mars
        } else if (model == "random_forest") {
          model_random_forest <- th2_random_forest_engine(input_data, var_target, 200)
          list_output_models[["model_random_forest"]] <- model_random_forest
        } else if (model == "xgboost") {
          model_xgboost <- th2_xgboost_engine(input_data, var_date, var_target, 15)
          list_output_models[["model_xgboost"]] <- model_xgboost
        } else {
          error_models <- c(error_models, model)
        }
      }

      if (length(error_models) > 0) {
        return(warning(paste("Models not found:", error_models)))
      }

      model_table <- do.call(modeltime_table, list_output_models)

      return(model_table)
    } else {
      return(warning("No model was selected."))
    }
  } else {
    return(warning("A data type *data.frame* was expected."))
  }
}
