# th2Forecast

`th2Forecast` is an R package for automated time series forecasting and
machine learning model evaluation. It provides an integrated framework that
covers the entire forecasting pipeline, from data preparation to the
visualization of the final predictions.

## Main features

- **Automated preprocessing**: time series cleaning, missing value
  handling, detection of anomalies and level shifts.
- **Advanced feature engineering**: dedicated modules for lagged variables
  (lags) and the integration of exogenous data (public holidays, weather).
- **A wide range of models**: ARIMA, Prophet, ETS, MARS, linear regression,
  Random Forest, XGBoost, naive/snaive baselines.
- **Interactive interface**: Shiny module for loading data, configuring
  models and visualizing performance.

## API HTTP (`plumber2`)

The package exposes a REST API (`plumber.R` + `entrypoint.R`, served on port
`8000`) implementing the v1 contract described in
[`docs/API.md`](docs/API.md).

### Start the service

```bash
docker build -t th2forecast:dev .
docker run -d --name th2forecast -p 127.0.0.1:8000:8000 \
  -e TH2FORECAST_API_TOKEN=change-me \
  th2forecast:dev
```

### `curl` example (synchronous forecast)

```bash
curl -s http://127.0.0.1:8000/health

curl -s -X POST http://127.0.0.1:8000/v1/forecast \
  -H "Authorization: Bearer change-me" \
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

See [`docs/API.md`](docs/API.md) for the full contract (endpoints,
authentication, limits, error format, asynchronous jobs).

## Installation (R package usage, without the API)

```r
# install.packages("devtools")
devtools::install_github("apowerb/th2forecast")
```

## Shiny interface

```r
library(th2forecast)
run_app()
```

## Tests

```bash
docker run --rm th2forecast:dev Rscript -e 'testthat::test_dir("tests/testthat")'
```

End-to-end test (against a running container):

```bash
TH2FORECAST_BASE_URL=http://127.0.0.1:8000 TH2FORECAST_API_TOKEN=change-me \
  bash tests/e2e/smoke.sh
```

## Image publishing

The CI (`.github/workflows/docker-build.yml`) builds and tests both images (R API `apowerb/th2forecast`,
Python engine `apowerb/th2forecast-py`) on every PR to `main`, then publishes them:

- push to `main`: image tagged with the commit SHA;
- tag `vX.Y.Z`: images tagged `X.Y.Z` and `latest`. This is the tag that deployments (Helm chart,
  apowerb-hosting compose) pin.

```bash
git tag vX.Y.Z && git push origin vX.Y.Z
```

## License

Apache License 2.0 — see [`LICENSE`](LICENSE).
