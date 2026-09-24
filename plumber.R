library(plumber2)
library(th2forecast)

#* TH2 Forecasting API
#* API pour le service th2forecast (prevision de series temporelles)

#* @get /health
#* @serializer application/json;charset=utf-8 reqres::format_json(auto_unbox=TRUE, null="null")
function() {
  th2forecast::api_v1_health()
}

#* Lance une prevision synchrone
#* @post /v1/forecast
#* @parser json
#* @serializer application/json;charset=utf-8 reqres::format_json(auto_unbox=TRUE, null="null")
function(body, request, response) {
  t0 <- Sys.time()
  if (!th2forecast::api_v1_check_auth(request$get_header("Authorization"))) {
    response$status <- 401L
    return(th2forecast::api_v1_error_body(list(
      th2forecast::api_v1_error(NULL, "Authentification requise : en-tête 'Authorization: Bearer <token>' manquant ou invalide.")
    )))
  }

  limits <- th2forecast::api_v1_limits()
  out <- th2forecast::api_v1_run_forecast(body, limits)
  response$status <- out$status_code

  duration_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
  th2forecast::api_v1_log_request(
    route = "POST /v1/forecast", status_code = out$status_code, duration_ms = duration_ms,
    n_rows = tryCatch(if (is.data.frame(body$data)) nrow(body$data) else length(body$data), error = function(e) NA),
    n_series = tryCatch(length(out$body$series), error = function(e) NA),
    models = tryCatch(unlist(body$models), error = function(e) NA)
  )

  out$body
}

#* Cree un job de prevision asynchrone
#* @post /v1/jobs
#* @parser json
#* @serializer application/json;charset=utf-8 reqres::format_json(auto_unbox=TRUE, null="null")
function(body, request, response) {
  t0 <- Sys.time()
  if (!th2forecast::api_v1_check_auth(request$get_header("Authorization"))) {
    response$status <- 401L
    return(th2forecast::api_v1_error_body(list(
      th2forecast::api_v1_error(NULL, "Authentification requise : en-tête 'Authorization: Bearer <token>' manquant ou invalide.")
    )))
  }

  limits <- th2forecast::api_v1_limits()
  out <- th2forecast::api_v1_job_create(body, limits)
  response$status <- out$status_code

  duration_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
  th2forecast::api_v1_log_request(
    route = "POST /v1/jobs", status_code = out$status_code, duration_ms = duration_ms,
    n_rows = tryCatch(if (is.data.frame(body$data)) nrow(body$data) else length(body$data), error = function(e) NA)
  )

  out$body
}

#* Recupere l'etat/resultat d'un job
#* @get /v1/jobs/<id:string>
#* @serializer application/json;charset=utf-8 reqres::format_json(auto_unbox=TRUE, null="null")
function(id, request, response) {
  if (!th2forecast::api_v1_check_auth(request$get_header("Authorization"))) {
    response$status <- 401L
    return(th2forecast::api_v1_error_body(list(
      th2forecast::api_v1_error(NULL, "Authentification requise : en-tête 'Authorization: Bearer <token>' manquant ou invalide.")
    )))
  }

  out <- th2forecast::api_v1_job_get(id)
  response$status <- out$status_code
  out$body
}
