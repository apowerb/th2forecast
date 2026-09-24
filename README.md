# th2Forecast

`th2Forecast` est un package R pour la prévision automatisée de séries
temporelles et l'évaluation de modèles de machine learning. Il fournit un
framework intégré couvrant l'ensemble du pipeline de prévision, de la
préparation des données à la visualisation des prédictions finales.

## Fonctionnalités principales

- **Prétraitement automatisé** : nettoyage des séries temporelles, gestion
  des valeurs manquantes, détection d'anomalies et de ruptures de niveau.
- **Ingénierie de features avancée** : modules spécialisés pour les
  variables retardées (lags) et l'intégration de données exogènes (jours
  fériés, météo).
- **Modèles variés** : ARIMA, Prophet, ETS, MARS, régression linéaire,
  Random Forest, XGBoost, baselines naive/snaive.
- **Interface interactive** : module Shiny pour le chargement de données,
  la configuration des modèles et la visualisation des performances.

## API HTTP (`plumber2`)

Le package expose une API REST (`plumber.R` + `entrypoint.R`, servie sur le
port `8000`) implémentant le contrat v1 décrit dans
[`docs/API.md`](docs/API.md).

### Démarrer le service

```bash
docker build -t th2forecast:dev .
docker run -d --name th2forecast -p 127.0.0.1:8000:8000 \
  -e TH2FORECAST_API_TOKEN=change-moi \
  th2forecast:dev
```

### Exemple `curl` (prévision synchrone)

```bash
curl -s http://127.0.0.1:8000/health

curl -s -X POST http://127.0.0.1:8000/v1/forecast \
  -H "Authorization: Bearer change-moi" \
  -H "Content-Type: application/json" \
  -d '{
    "data": [
      {"date": "2022-01-01", "sales": 100}, {"date": "2022-02-01", "sales": 108},
      {"date": "2022-03-01", "sales": 115}, {"date": "2022-04-01", "sales": 121},
      {"date": "2022-05-01", "sales": 130}, {"date": "2022-06-01", "sales": 128},
      {"date": "2022-07-01", "sales": 140}, {"date": "2022-08-01", "sales": 145},
      {"date": "2022-09-01", "sales": 150}, {"date": "2022-10-01", "sales": 158},
      {"date": "2022-11-01", "sales": 162}, {"date": "2022-12-01", "sales": 170}
    ],
    "date_var": "date",
    "target_var": "sales",
    "horizon": 3,
    "models": ["naive"],
    "confidence_levels": [0.8, 0.95]
  }'
```

Voir [`docs/API.md`](docs/API.md) pour le contrat complet (endpoints,
authentification, limites, format des erreurs, jobs asynchrones).

## Installation (usage package R, hors API)

```r
# install.packages("devtools")
devtools::install_github("apowerb/th2forecast")
```

## Interface Shiny

```r
library(th2forecast)
run_app()
```

## Tests

```bash
docker run --rm th2forecast:dev Rscript -e 'testthat::test_dir("tests/testthat")'
```

Test de bout en bout (contre un conteneur démarré) :

```bash
TH2FORECAST_BASE_URL=http://127.0.0.1:8000 TH2FORECAST_API_TOKEN=change-moi \
  bash tests/e2e/smoke.sh
```

## Licence

Apache License 2.0 — voir [`LICENSE`](LICENSE).
