library("plumber2")
# Serve the API
pa <- api("plumber.R") |>
  api_doc_add(
    openapi(
      info = openapi_info(
        title = "TH2 Forecasting API",
        description = "API for th2Forecast package",
        version = "1.0.0"
      ),
      tags = list(
        openapi_tag(name = "forecast", description = "Forecasting operations"),
        openapi_tag(name = "health", description = "Health checks")
      )
    )
  )

pa |>
  api_run(port = 8000, host = "0.0.0.0")