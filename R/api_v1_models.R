#' @name api_v1_models
NULL

#' Modele naive / snaive pour th2forecast (baseline API v1)
#'
#' @param input_data data.frame d'entrainement avec colonnes `var_date`/`var_target`
#' @param var_target nom de la colonne cible
#' @param var_date nom de la colonne date
#' @param engine "naive" ou "snaive"
#' @param seasonal_period periode saisonniere (utilisee par snaive uniquement)
#' @export
th2_naive_engine <- function(input_data, var_target, var_date, engine = "naive", seasonal_period = "auto") {
  formula <- stats::as.formula(paste(var_target, "~", var_date))
  spec <- parsnip::set_engine(
    modeltime::naive_reg(seasonal_period = seasonal_period),
    engine = engine
  )
  parsnip::fit(spec, formula, data = input_data)
}

#' @keywords internal
.api_v1_fit_one_model <- function(model_name, train_df, target_var = "value", date_var = "date", seasonal_period = 1L) {
  tryCatch({
    fit <- switch(model_name,
      arima   = th2_arima_engine(train_df, target_var, date_var, fit_model = TRUE),
      prophet = th2_prophet_engine(train_df, target_var, date_var, use_holidays = NULL, fit_model = TRUE),
      ets     = th2_ets_engine(train_df, date_var, target_var, fit_model = TRUE),
      naive   = th2_naive_engine(train_df, target_var, date_var, engine = "naive"),
      snaive  = th2_naive_engine(train_df, target_var, date_var, engine = "snaive", seasonal_period = seasonal_period),
      NULL
    )
    if (is.null(fit) || inherits(fit, "warning")) return(NULL)
    fit
  }, error = function(e) NULL)
}

#' Regle de fiabilite "reliability" (documentee dans docs/API.md)
#'
#' - "unknown" si les metriques de backtest sont indisponibles.
#' - "poor" si le modele ne bat pas la baseline (MASE >= baseline MASE).
#' - "fair" si le modele bat la baseline mais avec peu de points de holdout (< 6)
#'   ou un ratio MASE/baseline superieur a 0.8.
#' - "good" si le modele bat la baseline avec au moins 6 points de holdout et
#'   un ratio MASE/baseline inferieur ou egal a 0.8.
#'
#' @param model_mase MASE du modele retenu (peut etre NA)
#' @param baseline_mase MASE de la baseline (peut etre NA)
#' @param holdout_points nombre de points de holdout utilises pour le backtest
#' @export
api_v1_reliability <- function(model_mase, baseline_mase, holdout_points) {
  if (is.na(model_mase) || is.na(baseline_mase) || is.na(holdout_points) || holdout_points < 2) {
    return("unknown")
  }
  beats <- model_mase < baseline_mase
  if (!beats) return("poor")
  ratio <- if (baseline_mase > 0) model_mase / baseline_mase else 0
  if (holdout_points >= 6 && ratio <= 0.8) "good" else "fair"
}
