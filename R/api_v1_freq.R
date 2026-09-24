#' @name api_v1_freq
NULL

#' Detecte la frequence d'une serie temporelle a partir de ses dates
#'
#' Se fonde sur la mediane des ecarts (en jours) entre dates uniques triees.
#'
#' @param dates vecteur de Date (peut contenir des doublons/desordre)
#' @return chaine parmi "day","week","month","quarter","year"
#' @export
api_v1_detect_frequency <- function(dates) {
  d <- sort(unique(dates))
  if (length(d) < 2) return("day")
  diffs <- as.numeric(diff(d))
  med <- stats::median(diffs)
  if (med <= 3) "day"
  else if (med <= 10) "week"
  else if (med <= 45) "month"
  else if (med <= 135) "quarter"
  else "year"
}

#' @keywords internal
.api_v1_freq_to_by <- function(frequency) {
  switch(frequency,
    day = "day", week = "week", month = "month", quarter = "quarter", year = "year",
    "day"
  )
}

#' Periode saisonniere associee a une frequence (pour le choix de baseline)
#'
#' @param frequency chaine de frequence ("day","week",...)
#' @export
api_v1_seasonal_period <- function(frequency) {
  switch(frequency,
    day = 7L, week = 52L, month = 12L, quarter = 4L, year = 1L, 1L
  )
}

#' Regularise une serie (une seule colonne date + une seule colonne valeur)
#'
#' Comble les trous au pas detecte/impose avec `timetk::pad_by_time()`, puis
#' interpole lineairement les valeurs manquantes introduites.
#'
#' @param df data.frame avec colonnes `date` (Date) et `value` (numeric)
#' @param frequency frequence ("day","week","month","quarter","year")
#' @return liste `list(df=<data.frame regularise>, n_padded=<int>)`
#' @export
api_v1_regularize_series <- function(df, frequency) {
  df <- df[order(df$date), , drop = FALSE]
  by_unit <- .api_v1_freq_to_by(frequency)

  padded <- tryCatch(
    timetk::pad_by_time(df, .date_var = date, .by = by_unit, .pad_value = NA_real_),
    error = function(e) df
  )

  n_padded <- nrow(padded) - nrow(df)
  if (n_padded < 0) n_padded <- 0L

  v <- padded$value
  if (anyNA(v)) {
    known <- which(!is.na(v))
    if (length(known) >= 2) {
      interpolated <- stats::approx(
        x = known, y = v[known], xout = seq_along(v), rule = 2
      )$y
      v <- interpolated
    } else {
      v[is.na(v)] <- if (length(known) == 1) v[known] else 0
    }
  }
  padded$value <- v

  list(df = padded, n_padded = as.integer(n_padded))
}
