library(plumber2)
# modeltime enregistre ses moteurs (naive/snaive/arima/ets/prophet) aupres de
# parsnip d'une maniere qui exige que son namespace soit attache (pas
# seulement charge par modeltime::...) : voir docs/API.md, section
# "limites connues", et tests/testthat/setup.R pour la reproduction.
library(modeltime)
library(parsnip)
library(th2forecast)

n_workers <- as.integer(Sys.getenv("TH2FORECAST_WORKERS", "2"))
if (!is.na(n_workers) && n_workers > 0) {
  tryCatch({
    mirai::daemons(n_workers)
    mirai::everywhere({
      library(modeltime)
      library(parsnip)
      library(th2forecast)
    })
  }, error = function(e) {
    message("th2forecast: demarrage sans daemons mirai (jobs /v1/jobs executes en synchrone). Raison : ", conditionMessage(e))
  })
}

pa <- plumber2::api("plumber.R") |>
  plumber2::api_doc_add(
    plumber2::openapi(
      info = plumber2::openapi_info(
        title = "TH2 Forecasting API",
        description = "Prevision de series temporelles (contrat v1, voir docs/API.md)",
        version = as.character(utils::packageVersion("th2forecast"))
      ),
      tags = list(
        plumber2::openapi_tag(name = "forecast", description = "Prevision synchrone et asynchrone"),
        plumber2::openapi_tag(name = "health", description = "Sonde de sante")
      )
    )
  )

pa |> plumber2::api_run(port = as.integer(Sys.getenv("PORT", "8000")), host = "0.0.0.0")
