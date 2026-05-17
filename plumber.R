# plumber.R
library(plumber2)
library(th2forecast)

# Create a new Plumber router from the th2forecast package
pr() %>%
  pr_api("th2forecast") %>%
  pr_run(port = 8000)
