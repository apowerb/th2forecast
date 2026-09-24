# th2forecast (development version)

## API v1 (`feat/api-v1-json`)

- **Ajout** : implémentation du contrat API v1 (`GET /health`,
  `POST /v1/forecast`, `POST /v1/jobs` + `GET /v1/jobs/{id}`) en JSON, avec
  authentification Bearer optionnelle (`TH2FORECAST_API_TOKEN`), limites
  configurables (`TH2FORECAST_MAX_ROWS`, `TH2FORECAST_MAX_SERIES`,
  `TH2FORECAST_MAX_HORIZON`), validation complète des entrées avec erreurs
  400 actionnables en français, détection de fréquence et régularisation
  des trous (`timetk`), intervalles conformal (`modeltime`), backtest et
  comparaison de modèles avec baseline naive/snaive.
- **Retrait** : l'ancien endpoint `POST /forecast` (entrée base64 R
  sérialisée, `R/plumber_th2_forecast.R`) est supprimé. Il était écrit pour
  l'API `plumber` v1 (`function(res, input_data, ...)` + tags `@param`)
  alors que le service tourne sous **plumber2**, dont le modèle de
  handler est différent (`body`/`query`/`response` en arguments nommés) :
  le fichier n'était donc jamais exécutable tel quel. Ses
  `tryCatch(..., error = function(e) {...; return(...)})` ne stoppaient de
  toute façon pas l'exécution de `forecast()` en cas d'erreur (un `return()`
  dans le callback `error` d'un `tryCatch` ne fait pas sortir la fonction
  appelante), ce qui aurait produit un `500` opaque sur toute entrée
  invalide même corrigé pour plumber2.
- **Correction critique** : `uvr.toml` déclarait `th2forecast` comme sa
  propre dépendance Git (`[dependencies.th2forecast] git =
  "apowerb/th2forecast"`), figée par `uvr.lock` sur un commit précis de
  GitHub. Résultat : `uvr sync` installait le package depuis un tarball
  GitHub distant, **jamais depuis les sources locales du dépôt** — tout
  changement local à `R/` était donc invisible dans l'image Docker
  construite. Corrigé : la dépendance auto-référentielle est retirée du
  manifeste ; le `Dockerfile` installe désormais `th2forecast` depuis les
  sources locales (`install.packages('.', repos = NULL, type = 'source')`)
  après `uvr sync` (qui n'installe plus que les dépendances tierces).
- **Correction** : `DESCRIPTION` déclarait `bizdays` et `echarts4r` dans
  `Imports` (utilisés par `R/evaluation_models.R`,
  `R/mod_basic_fcast_viewer.R`, `R/preprocessing_dataset.R`,
  `R/forecast_viewer_helpers.R`) sans que `uvr.toml` ne les installe : le
  package n'était donc **jamais réellement installable depuis les
  sources** avant ce correctif (masqué jusqu'ici par la dépendance
  auto-référentielle ci-dessus, qui contournait toute installation
  locale réelle). Ajoutés à `uvr.toml`/`uvr.lock`.
- **Correction** : licence `GLP-3` (typo) corrigée en
  `Apache License (== 2.0)` dans `DESCRIPTION`, conforme au fichier
  `LICENSE` réellement présent dans le dépôt (Apache-2.0), et non MIT.
- **Correction** : `Dockerfile` épinglait `uvr` via
  `.../main/install.sh` (non reproductible) ; épinglé sur le tag `v0.4.6`.
  Point d'entrée unique (`entrypoint.R`) ; `run_api.R` (doublon) et
  `main_service.R` (fichier corrompu en UTF-16, non exécutable) supprimés.
- **Ajout** : `.Rhistory` retiré du suivi Git (déjà dans `.gitignore` mais
  resté indexé depuis un commit antérieur).
- **Limite connue** : `/v1/jobs` utilise `mirai` pour une exécution
  asynchrone réelle (daemons créés dans `entrypoint.R`) ; en l'absence de
  daemon disponible au démarrage, le calcul est exécuté en synchrone au
  moment du `POST` (repli documenté dans `docs/API.md`).

# th2test (development version)

* Initial CRAN submission.
