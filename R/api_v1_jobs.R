#' @name api_v1_jobs
NULL

.api_v1_job_store <- new.env(parent = emptyenv())

#' @keywords internal
.api_v1_new_job_id <- function() {
  sprintf("%d-%s", as.integer(Sys.time()), paste(sample(c(letters, 0:9), 10, TRUE), collapse = ""))
}

#' Cree un job de prevision asynchrone (via mirai)
#'
#' Valide la requete de maniere synchrone (echec rapide sans job) puis, si
#' elle est valide, execute la prevision dans un processus `mirai` separe.
#'
#' @param body corps JSON deserialise
#' @param limits `list(max_rows=,max_series=,max_horizon=)`
#' @export
api_v1_job_create <- function(body, limits) {
  validated <- api_v1_validate_request(body, limits)
  if (!isTRUE(validated$ok)) {
    return(list(status_code = validated$status, body = api_v1_error_body(validated$errors)))
  }

  job_id <- .api_v1_new_job_id()

  has_mirai_daemons <- tryCatch(isTRUE(mirai::daemons_set()), error = function(e) FALSE)

  if (has_mirai_daemons) {
    m <- mirai::mirai(
      th2forecast::api_v1_run_forecast(body, limits),
      body = body, limits = limits
    )
    assign(job_id, list(status = "queued", mirai = m, result = NULL, error = NULL), envir = .api_v1_job_store)
  } else {
    # Repli synchrone documente : pas de daemon mirai disponible.
    res <- tryCatch(api_v1_run_forecast(body, limits), error = function(e) NULL)
    if (is.null(res)) {
      assign(job_id, list(status = "failed", mirai = NULL, result = NULL, error = "Echec inattendu du calcul."), envir = .api_v1_job_store)
    } else {
      assign(job_id, list(status = "succeeded", mirai = NULL, result = res$body, error = NULL), envir = .api_v1_job_store)
    }
  }

  list(status_code = 202L, body = list(job_id = job_id, status = "queued"))
}

#' Recupere l'etat/resultat d'un job
#'
#' @param job_id identifiant de job
#' @export
api_v1_job_get <- function(job_id) {
  if (!exists(job_id, envir = .api_v1_job_store, inherits = FALSE)) {
    return(list(status_code = 404L, body = api_v1_error_body(list(
      api_v1_error("job_id", sprintf("Job '%s' inconnu.", job_id))
    ))))
  }

  job <- get(job_id, envir = .api_v1_job_store, inherits = FALSE)

  if (job$status %in% c("succeeded", "failed")) {
    return(list(status_code = 200L, body = list(
      job_id = job_id, status = job$status, result = job$result, error = job$error
    )))
  }

  if (!is.null(job$mirai) && !mirai::unresolved(job$mirai)) {
    outcome <- job$mirai$data
    if (mirai::is_mirai_error(outcome) || inherits(outcome, "error")) {
      job$status <- "failed"
      job$error <- api_v1_error_body(list(api_v1_error(NULL, "Echec inattendu du calcul de prevision.")))
      job$result <- NULL
    } else {
      job$status <- "succeeded"
      job$result <- outcome$body
      job$error <- NULL
    }
    assign(job_id, job, envir = .api_v1_job_store)
  } else {
    job$status <- "running"
  }

  list(status_code = 200L, body = list(
    job_id = job_id, status = job$status, result = job$result, error = job$error
  ))
}
