# modeltime enregistre ses moteurs personnalises (naive/snaive, arima, ets,
# prophet...) aupres de parsnip d'une maniere qui exige que le namespace
# modeltime soit attache (library()), pas seulement charge par ::. Sans cela,
# parsnip::fit() echoue avec "could not find function '..._fit_impl'".
# Voir docs/API.md (section "limites connues") pour le detail de ce
# contournement, verifie par reproduction directe (avec/sans library()).
suppressPackageStartupMessages({
  library(modeltime)
  library(parsnip)
})
