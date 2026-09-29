# th2forecast — Python engine (Chronos-2 + statsforecast)

HTTP service with the **same v1 contract** as the R API (`docs/API.md`): same routes, same fields,
same error messages, same codes (400, 401, 404, 413). The `tests/e2e/smoke.sh` smoke test
runs unchanged against both services.

## What differs from the R API

- `auto` = **fixed ensemble** of Chronos-2 + AutoETS + AutoARIMA + AutoTheta (average of the points and
  of the bounds). The `model` field of the response is then `ensemble`.
- `arima`, `ets`, `naive`, `snaive`, `prophet` remain available individually.
- Backtest with **rolling origins** (up to 3 windows) instead of a single split;
  metrics are pooled across windows, `holdout_points` counts all tested points.
- The final forecast is always **retrained on the whole series** (the defect fixed by PR #5
  cannot occur here).
- All the series of a `group_var` request are computed in batch (one statsforecast call and
  one Chronos-2 call per horizon).
- **Calibrated bands** (conformal on the backtest windows): the bands of the selected model
  are widened or tightened so as to contain the promised share of past actual values; the
  additional `calibration` field (see `docs/API.md`) gives the factor and the measured coverage.
- **Events and scenarios** (`events`, `scenarios`, see `docs/API.md`): promotions, closures,
  public holidays… become covariates learned from the history; a scenario replaces the future
  events or applies an explicit adjustment, and the response gives the difference from the baseline.
  Synthetic benchmark (20 daily series, promotions with an effect of 15 to 60, h = 28): MAE on
  promotion days 34.8 → 3.6, all days 6.4 → 3.3; effect estimated by the "no promo" scenario: 35.
- **Intermittent demand** (`demand`, see `docs/API.md`): each series is classified
  smooth/erratic/intermittent/lumpy (Syntetos-Boylan). `croston`, `tsb`, `imapa` are added to the
  available models; on an intermittent/lumpy series, `auto` switches the ensemble to
  `chronos2, tsb, imapa`, the bands are floored at 0 and the forecast values keep 2 decimals.
- **Hierarchy and MinT reconciliation** (`hierarchy`, `reconciliation`, see `docs/API.md`):
  aggregates (region, total…) above `group_var`, forecast as full-fledged series and then
  made coherent (`ŷ_rec = S (Sᵀ W⁻¹ S)⁻¹ Sᵀ W⁻¹ ŷ`, `W` = covariance of the backtest errors via
  the Schäfer-Strimmer shrinkage estimator); automatic fallback to the simple sum of the bottom
  series (`bottom_up`) if `W` is singular. Measured on Tourism (Australian domestic tourism, real data,
  quarterly, `Purpose > State > City`, 56 bottom series + 32 aggregates, `ets`, final holdout of
  8 quarters outside the backtest): mean MASE base → MinT — total 0.428 → 0.441, purpose 0.518 → 0.555,
  **state 0.652 → 0.602**, bottom 0.624 → 0.627. A mixed result, acknowledged as such: MinT clearly
  improves the intermediate "state" level but slightly degrades the total, the "purpose" level
  and the bottom level on this particular dataset — consistent with the literature (MinT guarantees
  coherence, not a strictly better MASE at every level); the internal backtest (leave-one-window-out) gives
  0.687 → 0.671 pooled over all nodes (script and outputs kept outside the repository, along with the
  task's evidence).
- Known difference: a missing date (`null`) is reported as unparsable.

## Measurements (monthly M3 benchmark, 100 series, h = 12, one request)

| Engine | Mean MASE | 80% coverage | 95% coverage | Duration |
| --- | --- | --- | --- | --- |
| R API fixed (PR #5) | 0.912 | 0.63 | — | — |
| Python `arima`, raw bands | 0.907 | 0.74 | 0.88 | 44 s |
| Python `arima`, calibrated bands | 0.907 | **0.79** | **0.94** | 46 s |
| Python `auto`, raw bands | **0.881** | 0.77 | 0.93 | 54 s |
| Python `auto`, calibrated bands | **0.881** | 0.78 | 0.93 | 63 s |

Measured on 2026-09-25 on a 4 vCPU VM without GPU; container memory ≈ 420 MB after the benchmark.
Since the ensemble's bands are already almost right at backtest (0.78 / 0.96), calibration
changes them little; it mainly corrects the models with bands that are too narrow (ARIMA ×1.28 at 80%).

## Run

```bash
docker build -t th2forecast-py python
docker run --rm -p 8000:8000 -e TH2FORECAST_API_TOKEN=... th2forecast-py
```

The Chronos-2 weights (Apache-2.0, revision pinned in the `Dockerfile`) are included in the image:
no network access at startup.

## Environment variables

| Variable | Default | Role |
| --- | --- | --- |
| `TH2FORECAST_API_TOKEN` | empty (no auth) | Expected Bearer token |
| `TH2FORECAST_MAX_ROWS` / `_MAX_SERIES` / `_MAX_HORIZON` | 100000 / 200 / 366 | Limits (413) |
| `TH2FORECAST_WORKERS` | 2 | Simultaneous asynchronous jobs (0 = synchronous) |
| `TH2FORECAST_PRELOAD` | 1 | Loads Chronos-2 at startup |
| `TH2FORECAST_TORCH_THREADS` | number of CPUs | torch threads |
| `TH2FORECAST_SF_JOBS` | 1 | statsforecast processes |
| `TH2FORECAST_CHRONOS_PATH` | `/opt/models/chronos-2` | Weights directory |

## Tests

```bash
docker run --rm th2forecast-py python -m pytest -q -p no:cacheprovider
```
