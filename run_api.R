library(plumber2)
plumber2::api('plumber.R') |> plumber2::api_run(port=8000, host="0.0.0.0")
