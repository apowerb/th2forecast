#======Models======

#' Création et entraînement du modèle Arima
#'
#' @param input_data Un dataframe qui contient les données à utiliser pour l'entraînement du modèle.
#' @param var_target Le nom de la colonne target dans le dataframe.
#' @param var_date Le nom de la colonne qui contient les dates dans le dataframe.
#' @param engine Le moteur à utiliser pour le modèle arime_reg. Par défaut, c'est "auto_arima".
#'
#' @import modeltime
#' @import tidymodels
#'
#' @return Un modèle Arima entraîné.
#' @export
#'
#' @examples th2_arima_engine(input_data, "value", "datetime", engine="auto_arima")
th2_arima_engine <- function(input_data, var_target, var_date, engine="auto_arima"){
  if (!(var_date %in% colnames(training(input_data)) && var_target %in% colnames(training(input_data)))){
    return(warning("Selected variables do not exist in the data."))
  }else
  {
    formula <- as.formula(paste(var_target, "~", var_date))

    set.seed(1234)
    model_arima <- arima_reg() %>%
      set_engine(engine = engine) %>%
      fit(formula, data = training(input_data))
    return(model_arima)
  }
}


#' Création et entraînement du modèle Prophète
#'
#' @param input_data Un dataframe qui contient les données à utiliser pour l'entraînement du modèle.
#' @param var_target Le nom de la colonne target dans le dataframe.
#' @param var_date Le nom de la colonne qui contient les dates dans le dataframe.
#' @param engine Le moteur à utiliser pour le modèle Prophet. Par défaut, c'est "prophet".
#'
#' @import modeltime
#' @import tidymodels
#' @import prophet
#'
#' @return Un modèle Prophet entraîné.
#' @export
#'
#' @examples th2_prophet_engine(input_data, "value", "datetime", engine="prophet")
th2_prophet_engine <- function(input_data, var_target, var_date, engine = "prophet"){
  if (!(var_date %in% colnames(training(input_data)) && var_target %in% colnames(training(input_data)))){
    return(warning("Selected variables do not exist in the data."))
  }else
  {
    formula <- as.formula(paste(var_target, "~", var_date))

    model_prophet <- prophet_reg() %>%
      set_engine(engine = engine) %>%
      fit(formula, data = training(input_data))

    return(model_prophet)
  }
}

#' Création et entraînement du modèle de régression linéaire
#'
#' @param input_data Un dataframe qui contient les données à utiliser pour l'entraînement du modèle.
#' @param var_target Le nom de la colonne target dans le dataframe.
#' @param var_date Le nom de la colonne qui contient les dates dans le dataframe.
#' @param engine Le moteur à utiliser pour le modèle linear_reg. Par défaut, c'est "lm".
#'
#' @import modeltime
#' @import tidymodels
#'
#' @return Un modèle de régression linéaire entraîné.
#' @export
#'
#' @examples th2_linear_engine(input_data, "value", "datetime", engine="lm")
th2_linear_engine <- function(input_data, var_target, var_date, engine = "lm"){

  if (!(var_date %in% colnames(training(input_data)) && var_target %in% colnames(training(input_data)))){
    return(warning("Selected variables do not exist in the data."))
  }else
  {
    formula <- as.formula(paste(var_target, "~", "as.numeric(",var_date,") + factor(month(",var_date,", label = TRUE), ordered = FALSE)"))
    # formula <- as.formula(paste(var_target, "~ ."))

    model_linear <- linear_reg() %>%
      set_engine(engine = engine) %>%
      fit(formula, data = training(input_data))

    return(model_linear)
  }
}

#' Création et entraînement du modèle MARs
#'
#' @param input_data Un dataframe qui contient les données à utiliser pour l'entraînement du modèle.
#' @param var_target Le nom de la colonne target dans le dataframe.
#' @param var_date Le nom de la colonne qui contient les dates dans le dataframe.
#' @param engine Le moteur à utiliser pour le modèle mars Par défaut, c'est "earth".
#'
#' @import modeltime
#' @import tidymodels
#'
#' @return Un modèle MARs entraîné.
#' @export
#'
#' @examples th2_mars_engine(input_data, "value", "datetime", engine="earth", mars_features="month")
th2_mars_engine <- function(input_data, var_target, var_date, engine = "earth", mars_features="month"){

  if (!(var_date %in% colnames(training(input_data)) && var_target %in% colnames(training(input_data)))){
    return(warning("Selected variables do not exist in the data."))
  }else
  {
    formula <- as.formula(paste(var_target, "~", var_date))

    model_mars <- mars(mode = "regression") %>%
      set_engine(engine)

    recipe_spec <- recipe(formula, data = training(input_data)) %>%
      step_date(var_date, features = mars_features, ordinal = FALSE) %>%
      step_mutate(date_num = as.numeric(!!sym(var_date))) %>%
      step_normalize(date_num) %>%
      step_rm(var_date)

    model_fit_mars <- workflow() %>%
      add_recipe(recipe_spec) %>%
      add_model(model_mars) %>%
      fit(training(input_data))

    return(model_fit_mars)
  }

}


#' Création et entraînement du modèle Random Forest
#'
#' @param input_data
#' @param var_target
#' @param trees
#'
#' @import randomForest
#'
#' @return
#' @export
#'
#' @examples
th2_random_forest_engine <- function(input_data, var_target, trees = 500){

  model_rf <- rand_forest( trees = trees ) %>%
    set_engine("randomForest") %>%
    set_mode("regression")

  # Fit a Random Forest model
  formula <- as.formula(paste(var_target, "~ ."))

  model_rf_fit <- model_rf %>%
    fit(formula, data = training(input_data))

  return(model_rf_fit)

}


#' Création et entraînement du modèle XGBoost
#'
#' @param input_data
#' @param var_target
#' @param trees
#'
#' @return
#' @export
#'
#' @examples
th2_xgboost_engine <- function(input_data, var_target, trees = 15){

  model_xgboost <-
    boost_tree(trees = trees) %>%
    set_mode("regression") %>%
    set_engine("xgboost")


  # Fit a Random Forest model
  formula <- as.formula(paste(var_target, "~ ."))

  set.seed(1)
  model_xgboost_fit <- model_xgboost %>%
    fit(formula, data = training(input_data))

  return(model_xgboost_fit)

}


#======Selection and train models
#' Sélection et préparation des modèles
#'
#' @param input_data Un dataframe qui contient les données à utiliser pour l'entraînement du modèle.
#' @param list_models Une liste des modèles à entraîner.
#' @param var_target Le nom de la colonne cible dans le dataframe.
#' @param var_date Le nom de la colonne qui contient les dates dans le dataframe.
#'
#' @import modeltime
#' @import tidymodels
#' @import caTools
#'
#' @return Un tableau qui contient les modèles entraînés.
#' @export
#'
#' @examples model_selection_train(input_data, c("arima", "prophet", "lr", "mars"), "value", "datetime")
model_selection_train <- function (input_data, list_models, var_target, var_date, input_feature_data){

  if (setequal(class(input_data), c("ts_cv_split", "rsplit"))){
    error_models <- NULL

    if (length(list_models) > 0 && is.character(list_models))
    {

      if (!(var_date %in% colnames(training(input_data)) && var_target %in% colnames(training(input_data)))){
        return(warning("Selected variables do not exist in the data."))
      }

      list_output_models <- list()

      for (model in list_models){
        if(model == "arima"){
          model_arima <-  th2_arima_engine(input_data, var_target, var_date)
          list_output_models[["model_arima"]] <- model_arima
        } else if(model == "prophet"){
          model_prophet <-  th2_prophet_engine(input_data, var_target, var_date)
          list_output_models[["model_prophet"]] <- model_prophet
        } else  if(model == "lr"){
          model_lm <- th2_linear_engine(input_data, var_target, var_date)
          list_output_models[["model_lm"]] <- model_lm
        } else if(model == "mars"){
          model_mars <- th2_mars_engine(input_data, var_target, var_date)
          list_output_models[["model_mars"]] <- model_mars
        } else if(model == "random_forest"){
          model_random_forest <- th2_random_forest_engine(input_data, var_target, 200)
          list_output_models[["model_random_forest"]] <- model_random_forest
        }else if(model == "xgboost"){
          model_xgboost <- th2_xgboost_engine(input_data, var_target, 15)
          list_output_models[["model_xgboost"]] <- model_xgboost
        }else{
          error_models <- c(error_models, model)
        }
      }

      if(length(error_models) > 0){
        return(warning(paste("Models not found:", error_models)))
      }

      model_table <- do.call(modeltime_table, list_output_models)

      return(model_table)
    }else{
      return(warning("No model was selected."))
    }
  }else{
    return(warning("A data type *ts_cv_split* was expected."))
  }
}
