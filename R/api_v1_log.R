#' @name api_v1_log
NULL

#' Emet une ligne de log JSON structuree sur stdout (aucune donnee utilisateur)
#'
#' @param route route appelee (ex: "POST /v1/forecast")
#' @param status_code code HTTP renvoye
#' @param duration_ms duree de traitement en millisecondes
#' @param n_rows nombre de lignes en entree (ou NA)
#' @param n_series nombre de series traitees (ou NA)
#' @param models modeles demandes (vecteur de chaines, ou NA)
#' @param request_id identifiant de correlation
#' @export
api_v1_log_request <- function(route, status_code, duration_ms, n_rows = NA, n_series = NA, models = NA, request_id = NULL) {
  entry <- list(
    ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    id = if (is.null(request_id)) .api_v1_new_job_id() else request_id,
    route = route,
    status = status_code,
    duration_ms = round(duration_ms),
    n_rows = if (is.na(n_rows)) NULL else n_rows,
    n_series = if (is.na(n_series)) NULL else n_series,
    models = if (identical(models, NA)) NULL else as.list(models)
  )
  cat(jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null"), "\n", sep = "")
}

#' Verifie l'authentification Bearer optionnelle
#'
#' Si `TH2FORECAST_API_TOKEN` est defini dans l'environnement, exige un en-tete
#' `Authorization: Bearer <token>` correspondant. Sinon, l'authentification est
#' desactivee (toutes les requetes passent).
#'
#' @param auth_header valeur brute de l'en-tete `Authorization` (ou NULL)
#' @return TRUE si autorise, FALSE sinon
#' @export
api_v1_check_auth <- function(auth_header) {
  expected <- Sys.getenv("TH2FORECAST_API_TOKEN", "")
  if (!nzchar(expected)) return(TRUE)
  if (is.null(auth_header) || !nzchar(auth_header)) return(FALSE)
  provided <- sub("^Bearer\\s+", "", auth_header)
  isTRUE(provided == expected)
}

#' Lit les limites configurees via variables d'environnement
#' @export
api_v1_limits <- function() {
  list(
    max_rows = as.integer(Sys.getenv("TH2FORECAST_MAX_ROWS", "100000")),
    max_series = as.integer(Sys.getenv("TH2FORECAST_MAX_SERIES", "200")),
    max_horizon = as.integer(Sys.getenv("TH2FORECAST_MAX_HORIZON", "366"))
  )
}
