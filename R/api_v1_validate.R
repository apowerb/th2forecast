#' @name api_v1_validate
NULL

ALLOWED_MODELS <- c("prophet", "arima", "ets", "snaive", "naive", "auto")
ALLOWED_FREQUENCIES <- c("day", "week", "month", "quarter", "year")

#' Construit une erreur de champ pour la réponse API v1
#'
#' @param field nom du champ concerné (ou NULL)
#' @param message message en français, actionnable
#' @export
api_v1_error <- function(field, message) {
  list(field = field, message = message)
}

#' Construit une réponse d'erreur API v1
#'
#' @param errors liste de `api_v1_error()`
#' @export
api_v1_error_body <- function(errors) {
  list(status = "error", errors = errors)
}

#' @keywords internal
.api_v1_null <- function(x, default) if (is.null(x)) default else x

#' Valide et normalise la requête `/v1/forecast` (ou `/v1/jobs`)
#'
#' Ne fait AUCUN accès modèle : validation pure, retournable en 400.
#'
#' @param body corps JSON déjà désérialisé (liste R)
#' @param limits liste `list(max_rows=, max_series=, max_horizon=)`
#' @return liste `list(ok=TRUE, request=<liste normalisée>)` ou
#'   `list(ok=FALSE, status=400L|413L, errors=<liste api_v1_error>)`
#' @export
api_v1_validate_request <- function(body, limits) {
  errors <- list()
  add_error <- function(field, message) {
    errors[[length(errors) + 1]] <<- api_v1_error(field, message)
  }

  if (is.null(body) || !is.list(body)) {
    return(list(ok = FALSE, status = 400L, errors = list(
      api_v1_error(NULL, "Corps de requête JSON manquant ou invalide.")
    )))
  }

  data_rows <- body[["data"]]
  if (is.null(data_rows) || (is.list(data_rows) && length(data_rows) == 0)) {
    add_error("data", "Le champ 'data' est requis et doit contenir au moins une ligne.")
  }

  date_var <- body[["date_var"]]
  target_var <- body[["target_var"]]
  group_var <- .api_v1_null(body[["group_var"]], NULL)
  if (!is.null(group_var) && (is.na(group_var) || identical(group_var, ""))) group_var <- NULL

  if (is.null(date_var) || !nzchar(date_var)) add_error("date_var", "Le champ 'date_var' est requis.")
  if (is.null(target_var) || !nzchar(target_var)) add_error("target_var", "Le champ 'target_var' est requis.")

  horizon <- body[["horizon"]]
  if (is.null(horizon) || !is.numeric(horizon) || length(horizon) != 1 || horizon <= 0 || horizon != as.integer(horizon)) {
    add_error("horizon", "Le champ 'horizon' doit etre un entier strictement positif.")
    horizon <- NA_integer_
  } else {
    horizon <- as.integer(horizon)
  }

  models <- body[["models"]]
  if (is.null(models) || length(models) == 0) {
    models <- list("auto")
  }
  models <- unlist(models, use.names = FALSE)
  unknown_models <- setdiff(models, ALLOWED_MODELS)
  if (length(unknown_models) > 0) {
    add_error(
      "models",
      sprintf(
        "Modele(s) inconnu(s) : %s ; modeles disponibles : %s.",
        paste(unknown_models, collapse = ", "),
        paste(ALLOWED_MODELS, collapse = ", ")
      )
    )
  }

  frequency <- body[["frequency"]]
  if (!is.null(frequency) && !identical(frequency, "") && !(frequency %in% ALLOWED_FREQUENCIES)) {
    add_error(
      "frequency",
      sprintf(
        "Frequence '%s' inconnue ; valeurs acceptees : %s (ou null pour detection automatique).",
        frequency, paste(ALLOWED_FREQUENCIES, collapse = ", ")
      )
    )
    frequency <- NULL
  }
  if (identical(frequency, "")) frequency <- NULL

  confidence_levels <- body[["confidence_levels"]]
  if (is.null(confidence_levels) || length(confidence_levels) == 0) {
    confidence_levels <- c(0.8, 0.95)
  } else {
    confidence_levels <- unlist(confidence_levels, use.names = FALSE)
    if (!is.numeric(confidence_levels) || any(confidence_levels <= 0 | confidence_levels >= 1)) {
      add_error("confidence_levels", "Les niveaux de confiance doivent etre des nombres dans l'intervalle ouvert (0, 1).")
      confidence_levels <- c(0.8, 0.95)
    }
  }

  # Erreurs de structure -> on ne peut pas aller plus loin.
  if (length(errors) > 0) {
    return(list(ok = FALSE, status = 400L, errors = errors))
  }

  # Limites (413) : verifiees avant toute validation semantique couteuse.
  if (!is.na(horizon) && horizon > limits$max_horizon) {
    return(list(ok = FALSE, status = 413L, errors = list(
      api_v1_error("horizon", sprintf(
        "Horizon demande (%d) superieur a la limite autorisee (%d).", horizon, limits$max_horizon
      ))
    )))
  }

  df <- tryCatch(
    as.data.frame(lapply(api_v1_rows_to_columns(data_rows), unlist), stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) {
    return(list(ok = FALSE, status = 400L, errors = list(
      api_v1_error("data", "Impossible d'interpreter 'data' comme un tableau de lignes homogenes.")
    )))
  }

  if (nrow(df) > limits$max_rows) {
    return(list(ok = FALSE, status = 413L, errors = list(
      api_v1_error("data", sprintf(
        "Nombre de lignes (%d) superieur a la limite autorisee (%d).", nrow(df), limits$max_rows
      ))
    )))
  }

  available_cols <- names(df)
  if (!(date_var %in% available_cols)) {
    return(list(ok = FALSE, status = 400L, errors = list(
      api_v1_error("date_var", sprintf(
        "Colonne '%s' absente ; colonnes disponibles : %s.", date_var, paste(available_cols, collapse = ", ")
      ))
    )))
  }
  if (!(target_var %in% available_cols)) {
    return(list(ok = FALSE, status = 400L, errors = list(
      api_v1_error("target_var", sprintf(
        "Colonne '%s' absente ; colonnes disponibles : %s.", target_var, paste(available_cols, collapse = ", ")
      ))
    )))
  }
  if (!is.null(group_var) && !(group_var %in% available_cols)) {
    return(list(ok = FALSE, status = 400L, errors = list(
      api_v1_error("group_var", sprintf(
        "Colonne '%s' absente ; colonnes disponibles : %s.", group_var, paste(available_cols, collapse = ", ")
      ))
    )))
  }

  .safe_as_date <- function(x) {
    tryCatch(as.Date(x), error = function(e) as.Date(NA))
  }
  parsed_dates <- as.Date(vapply(
    as.character(df[[date_var]]),
    function(x) suppressWarnings(as.numeric(.safe_as_date(x))),
    numeric(1)
  ), origin = "1970-01-01")
  bad_idx <- which(is.na(parsed_dates) & !is.na(df[[date_var]]) & nzchar(as.character(df[[date_var]])))
  if (length(bad_idx) > 0) {
    bad_values <- unique(as.character(df[[date_var]][bad_idx]))
    return(list(ok = FALSE, status = 400L, errors = list(
      api_v1_error("data", sprintf(
        "Dates non parsables dans la colonne '%s' (format attendu YYYY-MM-DD) : %s.",
        date_var, paste(utils::head(bad_values, 5), collapse = ", ")
      ))
    )))
  }
  df[[date_var]] <- parsed_dates

  target_raw <- df[[target_var]]
  target_numeric <- suppressWarnings(as.numeric(as.character(target_raw)))
  if (any(is.na(target_numeric) & !is.na(target_raw))) {
    return(list(ok = FALSE, status = 400L, errors = list(
      api_v1_error("target_var", sprintf(
        "La colonne cible '%s' doit etre numerique.", target_var
      ))
    )))
  }
  df[[target_var]] <- target_numeric

  group_values <- if (!is.null(group_var)) as.character(df[[group_var]]) else rep(NA_character_, nrow(df))

  n_series <- length(unique(group_values))
  if (n_series > limits$max_series) {
    return(list(ok = FALSE, status = 413L, errors = list(
      api_v1_error("group_var", sprintf(
        "Nombre de series (%d) superieur a la limite autorisee (%d).", n_series, limits$max_series
      ))
    )))
  }

  dup_msgs <- character(0)
  for (g in unique(group_values)) {
    idx <- if (is.na(g)) which(is.na(group_values)) else which(group_values == g)
    d <- df[[date_var]][idx]
    dups <- unique(d[duplicated(d)])
    if (length(dups) > 0) {
      label <- if (is.na(g)) "" else sprintf(" pour la serie '%s'", g)
      dup_msgs <- c(dup_msgs, sprintf("%s : %s", label, paste(utils::head(as.character(dups), 5), collapse = ", ")))
    }
  }
  if (length(dup_msgs) > 0) {
    return(list(ok = FALSE, status = 400L, errors = list(
      api_v1_error("data", sprintf(
        "Doublons de dates detectes%s.", paste(dup_msgs, collapse = " ; ")
      ))
    )))
  }

  min_points <- max(10L, horizon + 1L)
  too_short <- character(0)
  for (g in unique(group_values)) {
    idx <- if (is.na(g)) which(is.na(group_values)) else which(group_values == g)
    if (length(idx) < min_points) {
      label <- if (is.na(g)) "la serie" else sprintf("la serie '%s'", g)
      too_short <- c(too_short, sprintf("%s (%d points)", label, length(idx)))
    }
  }
  if (length(too_short) > 0) {
    return(list(ok = FALSE, status = 400L, errors = list(
      api_v1_error("horizon", sprintf(
        "Trop peu de points pour l'horizon demande (%d) : %s. Il faut au moins %d points (max(10, horizon + 1)).",
        horizon, paste(too_short, collapse = ", "), min_points
      ))
    )))
  }

  list(ok = TRUE, request = list(
    df = df,
    date_var = date_var,
    target_var = target_var,
    group_var = group_var,
    horizon = horizon,
    frequency = frequency,
    models = models,
    confidence_levels = sort(unique(confidence_levels)),
    holidays_country = .api_v1_null(body[["holidays_country"]], NULL)
  ))
}

#' Transforme une liste de lignes (objets JSON) en liste de colonnes
#'
#' `jsonlite::fromJSON(simplifyDataFrame = TRUE)` fait deja ce travail dans
#' le cas general, mais plumber2 peut fournir une liste de listes nommees ;
#' cette fonction gere les deux cas de maniere robuste.
#'
#' @param data_rows liste de lignes ou data.frame deja simplifie
#' @export
api_v1_rows_to_columns <- function(data_rows) {
  if (is.data.frame(data_rows)) {
    return(as.list(data_rows))
  }
  if (length(data_rows) == 0) return(list())
  col_names <- unique(unlist(lapply(data_rows, names)))
  out <- stats::setNames(vector("list", length(col_names)), col_names)
  for (nm in col_names) {
    out[[nm]] <- vapply(data_rows, function(row) {
      v <- row[[nm]]
      if (is.null(v)) NA_character_ else as.character(v)
    }, character(1))
  }
  out
}
