# th2forecast API v1

This document describes the v1 API contract (fixed on 2026-09-24) and the
implementation choices made for batch A.

## Authentication

If the `TH2FORECAST_API_TOKEN` environment variable is set, all routes
except `GET /health` require the `Authorization: Bearer <token>` header.
Otherwise, authentication is disabled (local/dev use only).

Response on failure: `401` with the standard error format (see below).

## Limits (environment variables, default values)

| Variable | Default | Effect when exceeded |
|---|---|---|
| `TH2FORECAST_MAX_ROWS` | 100000 | 413 |
| `TH2FORECAST_MAX_SERIES` | 200 | 413 |
| `TH2FORECAST_MAX_HORIZON` | 366 | 413 |

These limits apply to the business content of the already-deserialized
request (number of rows, number of distinct series, requested horizon):
plumber2 does not document a native mechanism for limiting the raw size in
bytes, so this choice is documented as a minor deviation from the wording
of the contract.

## Endpoints

- `GET /health` → `{"status":"UP","version":"<package version>"}`
- `POST /v1/forecast` (synchronous) → 200, response described below.
- `POST /v1/jobs` → 202 `{"job_id":"…","status":"queued"}`.
  `GET /v1/jobs/{id}` → `{"job_id","status":"queued|running|succeeded|failed","result":<response>|null,"error":<error>|null}` ;
  404 if `id` is unknown.
- **Deviation from the contract, documented**: the old `POST /forecast` (R
  base64, `plumber` v1) was **removed** rather than kept. The file
  `R/plumber_th2_forecast.R` was written for the plumber v1 API
  (`function(res, input_data, ...)`), incompatible with the installed
  plumber2 (see NEWS.md), and its `tryCatch(..., error = ...)` calls did not
  stop the execution of `forecast()` (an error-type return in a `tryCatch`
  callback does not make the enclosing function exit), which produced an
  opaque `500` on any invalid input. Replacing it properly would have
  duplicated all the validation logic already written for `/v1/forecast`
  with no benefit for batches B/C, which consume the v1 JSON exclusively.

### Asynchronous (`/v1/jobs`)

Implemented with the `mirai` package (daemons launched in `entrypoint.R` via
`mirai::daemons()`), recommended by the plumber2 documentation for
asynchronous execution. `POST /v1/jobs` validates the request
**synchronously** (fast failure with 400/413 without creating a job), then
delegates the computation to a separate `mirai` process if daemons are
configured. Documented fallback: if no mirai daemon is available at startup
(`TH2FORECAST_WORKERS=0` variable or failure of `mirai::daemons()`), the job
is run synchronously at `POST` time and immediately returned as
`"succeeded"`/`"failed"` on the first `GET` — the HTTP contract (202 then
`GET /v1/jobs/{id}`) is still honored, only the real parallelization is
lost in this fallback case.

### Request (JSON) — `POST /v1/forecast` and `POST /v1/jobs`

```json
{
  "data": [{"date": "2024-01-01", "sales": 120, "store": "A"}],
  "date_var": "date",
  "target_var": "sales",
  "group_var": null,
  "horizon": 12,
  "frequency": null,
  "models": ["prophet"],
  "confidence_levels": [0.8, 0.95],
  "holidays_country": null
}
```

- `data`: array of objects (rows).
- `frequency`: `null` (automatic detection) or an explicit value among
  `day`, `week`, `month`, `quarter`, `year`. An explicit frequency always
  takes precedence over detection.
- `models`: subset of `["prophet","arima","ets","snaive","naive","auto"]`. The R engine
  additionally accepts the package's machine-learning engines `"linear"`, `"mars"`,
  `"random_forest"`, `"xgboost"` and `"ensemble"` (see "R engine: package features" below).
  The Python engine (`python/`) additionally accepts `"croston"` (CrostonSBA), `"tsb"` (TSB, `alpha_d = alpha_p =
  0.1`) and `"imapa"` (IMAPA), dedicated to intermittent demand; see "`demand` field" below.

**Implementation note (documented choice, the contract is ambiguous on this point):**
the response schema returns **only one** `model` per series. The service
therefore systematically compares, at backtest (RMSE), all the requested
models (`"auto"` = `{arima, prophet, ets}` in addition to the explicit models
listed) and returns only the best one. The baseline (`snaive`/`naive`) is
always computed separately, independently of the requested models, for
`beats_baseline`/`reliability`; with `"auto"` on a seasonal frequency the
`snaive` baseline also competes for the win (R engine), so that a clearly
seasonal series is not answered with a flat forecast. On weekly data (period 52),
R `ets` runs on the STL-deseasonalised series from 2 years of history when the
seasonal profile repeats from one cycle to the next; below that, or on noise,
the forecast stays non seasonal.

### `200` response

Details:

- `frequency`: effective frequency used (detected or provided).
- Regularized history: calendar gaps at the detected/provided frequency step
  are filled by `timetk::pad_by_time()`, then the introduced missing values
  are linearly interpolated (`stats::approx(..., rule = 2)`). A `warnings[]`
  warning (at series level) states the number of filled points.
- Confidence intervals: **split conformal** via
  `modeltime::modeltime_forecast(..., conf_method = "conformal_split")`,
  computed on the residuals of the test set (holdout), one call per requested
  level in `confidence_levels`, merged into `lower_XX`/`upper_XX` columns.
- Final forecast: the selected model is **retrained on the whole series**
  (`modeltime::modeltime_refit()`) before `modeltime_forecast(h = horizon)`.
  Without this retraining, `arima` and `ets` forecast from the end of
  their training data (they ignore the requested dates): the forecast
  returned was that of the holdout, dated as the future. The
  intervals remain calibrated on the holdout residuals (split conformal),
  hence on a model trained with `holdout` fewer points than the final
  model. Regression test: `tests/testthat/test-api_v1_forecast_horizon.R`.

### Backtest / holdout split

`holdout = max(2, min(horizon, floor(0.2 * n)))`, bounded to a minimum of
`n - 3` training points. `min_points = max(10, horizon + 1)` is required in
validation (400 otherwise) to guarantee a usable split.

### Metrics and baseline

`modeltime::modeltime_accuracy()` (default metric set) on the test set:
`mape`, `smape`, `mase`, `rmse`. **Units**: `mape` and `smape`
are **fractions** (`0.08` = 8%), not percentages —
`yardstick::mape()`/`yardstick::smape()` return percentage points
(`8.0` for 8%), divided by 100 before being output in the
response (`mase`, `rmse` remain scale measures, not concerned).
`holdout_points` = number of points in the test set. Baseline: `snaive` if
the frequency has seasonality (`day` → 7, `week` → 52, `month` → 12,
`quarter` → 4), `naive` otherwise (`year`, no usable annual seasonal
cycle).

### `reliability` rule

Function `api_v1_reliability(model_mase, baseline_mase, holdout_points)`:

- `"unknown"`: metrics unavailable or `holdout_points < 2`.
- `"poor"`: the model does not beat the baseline (`model_mase >= baseline_mase`).
- `"good"`: the model beats the baseline **and** `holdout_points >= 6` **and**
  `model_mase / baseline_mase <= 0.8`.
- `"fair"`: the model beats the baseline but does not meet both
  conditions of `"good"`.

### Optional `calibration` field (Python engine only)

The Python engine (`python/`) adds a `calibration` field to each series; the R API does not
return it, so a client must tolerate its absence (or `null` for a failed series).

```json
"calibration": {
  "method": "split-conformal",
  "points": 14,
  "levels": {
    "80": {"calibrated": true, "pooled": false, "factor": 1.1237,
           "raw_coverage": 0.7143, "calibrated_coverage": 0.8571}
  }
}
```

- Score of each backtest point: the widening factor of the model's band that
  would have been needed to contain the actual value. `factor` = conformal quantile of order
  `ceil((n + 1) × level)` of these scores; the returned bands are those of the model multiplied
  by this factor around the forecast (widened if `factor > 1`, tightened otherwise).
- `pooled: true`: the series alone did not have enough points for this level (4 are needed for
  80%, 19 for 95%); its scores were supplemented with those of the other series in the request.
- `calibrated: false`: insufficient points even so; model bands unchanged.
- `raw_coverage`: share of the backtest actual values within the model's raw bands.
- `calibrated_coverage`: same measure for the calibrated bands, estimated out of sample
  (each window recalibrated without its own points); `null` if there is only one window.

### Optional `feedback` field and `calibration.adaptive` (ACI, Python engine only)

Request:

```json
"feedback": [
  {"group": "A", "level": "bottom",
   "points": [{"date": "2026-01-01", "value": 950, "lower_80": 800, "upper_80": 1100,
               "lower_95": 700, "upper_95": 1200}]}
]
```

- **Past** forecast bands (relayed by the core from its snapshots), one entry per node
  (`group`/`level` as in the response; `level` absent or `null` without a hierarchy). `points`:
  dates in the response format, with the `lower_XX`/`upper_XX` bounds of the levels to re-evaluate.
- The engine matches these dates against **its own regularized history** (same dates as
  `series[].history`); dates outside this history are silently ignored. Explicit 400
  (`field: "feedback"`) if the request shape is invalid; an entry whose
  `(group, level)` matches no node is ignored with a global warning (not a 400).
- For each level `L` present (at least 4 matched points, otherwise no adaptation for this
  level): `err_t = 1` if the regularized actual value for this date fell outside `[lower_L, upper_L]`,
  otherwise `0`, in date order. ACI (Gibbs & Candès, 2021):
  `α_1 = 1 − L`, `α_{t+1} = α_t + γ((1 − L) − err_t)`, `γ = 0.05`,
  `level_used = clip(1 − α_final, L, 0.995)` — never tightening below the requested level (few
  points, asymmetry of risk between a band that is too narrow and one that is too wide).
- The existing conformal calibration then takes the quantile of its scores (same scores, same
  pooling across series) at order `level_used` instead of `L`. If this quantile does not exist
  (too few scores for this level, even pooled), it falls back to the factor of level `L`
  multiplied by `z(level_used) / z(L)` (Gaussian approximation of a symmetric interval,
  documented in the code).
- **Hierarchy**: applied node by node (bottom and aggregates), **before** reconciliation — like the
  rest of the calibration, it is a property of the node, not of the reconciled view.
- Response, in the existing `calibration` object (same location as the other fields):

```json
"calibration": {
  "method": "split-conformal", "points": 14, "levels": {"80": {...}},
  "adaptive": {"80": {"target": 0.8, "points": 9, "observed": 0.667, "level_used": 0.9}}
}
```

  `target` = requested level, `points` = number of feedback points matched for this level,
  `observed` = coverage observed on these points (`1 - mean(err_t)`), `level_used` = level
  actually calibrated (rounded to 3 decimals). Absent (`calibration.adaptive` does not exist) if
  no feedback is applicable to this series; **without the `feedback` field in the request, the
  response is strictly unchanged**.

### `demand` field (Python engine only)

The Python engine classifies each series on its regularized history using the
Syntetos-Boylan (2005) method, and always returns this field, including for a failed series:

```json
"demand": {"type": "intermittent", "adi": 2.4, "cv2": 0.31, "zero_share": 0.58}
```

- `adi` (average inter-demand interval) = number of periods / number of non-zero periods; `null` if
  there is no non-zero value. `cv2` = (standard deviation / mean)² of the non-zero values; `null` if fewer
  than two non-zero values. `zero_share` = share of zero periods. All three are rounded to 4
  decimals.
- `type`, ADI thresholds = 1.32 and CV² = 0.49: `smooth` (ADI < 1.32, CV² < 0.49), `erratic` (ADI <
  1.32, CV² ≥ 0.49), `intermittent` (ADI ≥ 1.32, CV² < 0.49), `lumpy` (ADI ≥ 1.32, CV² ≥ 0.49).
  A negative value in the history, or fewer than two non-zero values, always classifies the
  series as `smooth` (CV² unreliable in these cases) even if `adi`/`cv2` remain computable.
- On an `intermittent`/`lumpy` series, `models: ["auto"]` replaces the default ensemble
  (`chronos2, ets, arima, theta`) with `chronos2, tsb, imapa` (with declared events:
  `chronos2` alone, as for the default ensemble). The series' `model` field remains
  `"ensemble"` in both cases; `demand.type` indicates which one was used.
- `croston`/`tsb`/`imapa` have no native band: the raw band is
  `point ± z(level) × standard deviation of the training values`, floored at 0. On an
  `intermittent`/`lumpy` series, the lower bound of the final band (even calibrated) is always floored at
  0. Forecast values are never rounded to the integer (2 decimals), even if the history
  is integer. A `warnings[]` warning systematically accompanies these series; an
  explicit model (`prophet`, `arima`, ...) requested on an intermittent series is run anyway,
  with a warning suggesting `auto`, `tsb` or `imapa`.

### R engine: package features (`preprocessing`, `holidays_country`, machine-learning models)

Request (all optional, off by default):

```json
"preprocessing": {"anomalies": true, "outliers": true},
"holidays_country": "FR",
"models": ["random_forest", "xgboost", "prophet", "ensemble"]
```

- **`preprocessing.anomalies`**: `anomaly_detection()` (anomalize: seasonal decomposition, then
  anomalous points replaced by a cleaned value). It looks at the residual once trend and
  seasonality are removed, so it is the step that catches a point unusual *for its season*.
  Only points whose value actually changes are reported: the recomposition's rounding drift
  on untouched points is ignored and those points keep their exact value.
- **`preprocessing.outliers`**: `outliers_detection(method = "cpt")`: the series is split into
  segments of homogeneous mean/variance (changepoint), and inside each segment the points more
  than 3 standard deviations from the segment mean are brought back to that mean. A level shift
  itself is kept: it separates two segments. Segments shorter than 11 points are merged into a
  neighbour first: changepoint tends to isolate a spike in a segment of a few points, where no
  point can be 3 standard deviations away. It does not remove seasonality, so a spike smaller
  than the seasonal swing can be missed; `anomalies` is the step for that.
- Preprocessing runs per series, after gap filling and before the holdout split: models,
  backtest metrics and the forecast use the cleaned series. **`history` keeps the original
  values** sent by the client. If a step fails on a series (too short, for instance), the series
  is kept as is for that step and gets a warning. With `holidays_country`, holiday dates are
  never corrected: a dip on a holiday is the effect the models learn, not an anomaly.
- **`holidays_country`**: `"FR"` (case-insensitive) or `null`; any other value is a 400. The
  French public holidays (metropolitan France) come from `calendrier.api.gouv.fr`, fetched once
  per process with a 5-second timeout. They feed Prophet, as an `is_holiday`
  regressor (0/1), and `random_forest`/`xgboost`, as the non-working-day feature of
  `feature_selection()`. Prophet does not get them through `th2_prophet_engine(use_holidays = ...)`:
  that path hands a holidays table to `set_engine()`, and modeltime's `prophet_fit_impl()` does not
  pass it on to Prophet (measured with modeltime 1.3.5: the fitted model has no holidays and the
  forecast is identical with and without them). Other callers of `th2_prophet_engine()` are
  affected the same way. If the calendar cannot be reached, the
  forecast is computed without holidays and the response carries a warning; it never fails.
  **The service needs outbound HTTPS to `calendrier.api.gouv.fr` for this option.**
- **Machine-learning models**: `linear` (linear regression on the date and the month), `mars`
  (MARS on the day of year), `random_forest` and `xgboost` (on the calendar features of
  `feature_selection()`). They are never part of `auto`, which stays `{arima, prophet, ets}`:
  they must be requested by name.
- **`ensemble`**: mean of the other requested models (or of `arima`, `prophet` and `ets` when
  `ensemble` is requested alone), built with `modeltime.ensemble::ensemble_average()`. It
  competes at backtest like any other model. With fewer than two fitted members, a warning is
  returned and the ensemble is skipped.

Response, per series (only if `preprocessing` is requested):

```json
"preprocessing": {
  "anomalies_corrected": 1,
  "outliers_corrected": 0,
  "corrections": [{"date": "2022-08-01", "original": 400, "corrected": 113.4, "kind": "anomaly"}]
}
```

Not exposed, on purpose: the Shiny modules, Rmd reports and charts (user interface); holidays
read from a database and `input_data_fetch()`/`output_data_fetch()` (they need database
credentials); Spark (`th2_bulk_forecasting_spark()`); TimeGPT (paid external API); bulk
forecasting (already covered by `group_var`); `th2_tune_model()`, `th2_benchmarking()` and
`th2_rolling_forecast_stablizer()` (evaluation tools; the API already backtests);
`th2_arimax_engine()` (without future values of external regressors it is ARIMA);
`meteo_feature()` (sends a location and dates to open-meteo.com; separate change).

### Optional `events` and `scenarios` fields (`scenarios` adjustments: both engines; `events`: Python engine only)

Request:

```json
"events": [
  {"name": "promo", "ranges": [{"start": "2025-12-01", "end": "2025-12-15"}], "dates": ["2024-12-05"],
   "groups": ["A"]}
],
"scenarios": [
  {"name": "Sans promo de décembre", "events": []},
  {"name": "Hausse de prix", "adjustments": [{"start": "2026-03-01", "end": "2026-06-30", "percent": -8}]}
]
```

- **Event**: dates or ranges over the history **and** the horizon; `groups` (optional) restricts
  the event to certain series. Each period receives the share of its days covered (0 to 1), used
  as a known covariate by Chronos-2, ARIMA (ARIMAX) and Prophet (regressor). With events,
  `auto` = Chronos-2 + ARIMA ensemble (ETS and Theta do not exploit them).
- An event **with no precedent in a series' history** is ignored for that series (warning):
  its effect cannot be learned; simulate it with an adjustment. The same goes for an event that
  affects almost every period equally (gap between its extreme shares ≤ 20% of the largest,
  e.g. "the 1st of each month" on monthly data): its effect is confounded with the level.
- **Scenario**: `events` (optional) replaces the **future** events (empty list = none); unknown
  names in `events` are ignored with a warning. `adjustments` then applies an explicit
  effect, `percent` (> -100) or `add`, pro rata to the covered days of each period.
  Scenario bands receive the same calibration as the base forecast.
- Limits: 20 events, 400 ranges per event, 5 scenarios, 20 adjustments per scenario.

Response, per series (only if the request contains them):

```json
"events": [{"name": "promo", "history_share": 0.11, "used": true}],
"scenarios": [{"name": "Sans promo de décembre", "forecast": [...],
               "difference": {"total": -1840, "percent": -6.2}}]
```

`history_share`: share of history periods affected by the event. `difference`:
scenario total minus base forecast total over the horizon.

**R engine**: `scenarios` with `adjustments` follow the same contract (same limits, same
validation, same response shape, bands shifted like the forecast). Events are not supported:
a request-level `events` field is ignored with a response warning, and a scenario's `events`
is ignored with a series warning (only its `adjustments` are applied). Dates in `start`/`end`
accept `YYYY-MM-DD` and `DD/MM/YYYY`. A period ends the day before the next one (a month
covers all its days, so `percent: 10` from the 15th to the 31st of January applies 17/31 of
10% to January). `difference.percent` is `null` when the base total is 0. A series whose
forecast fails returns `scenarios: []`.

### Optional `hierarchy` and `reconciliation` fields (Python engine only)

Request:

```json
"group_var": "store",
"hierarchy": ["region"],
"reconciliation": "mint"
```

- **`hierarchy`**: columns of `data`, from the highest level to the lowest, **above**
  `group_var` (which remains the lowest level). Requires `group_var`. A root "Total" level is
  always added. Each group (value of `group_var`) must have a single value per hierarchy
  column across its whole history, otherwise 400 (`field: "hierarchy"`). Absent/`null`/`[]`: no
  hierarchy, response strictly identical to before (no `level` field per series, no root
  `reconciliation` field).
- **`reconciliation`**: `"mint"` (default as soon as `hierarchy` is provided), `"bottom_up"` or
  `"none"`. `"mint"` estimates the covariance of the backtest errors with the Schäfer-Strimmer
  shrinkage estimator (like `hierarchicalforecast`'s `MinTrace(method="mint_shrink")`, it shrinks the
  **correlation** toward the identity, not the raw covariance) then reconciles with
  `ŷ_rec = S (Sᵀ W⁻¹ S)⁻¹ Sᵀ W⁻¹ ŷ` (numpy only, no added dependency). Automatic fallback to
  `bottom_up` (with a warning at root level) if the matrix is singular or if too few
  backtest points are available to estimate it. `"bottom_up"` ignores the aggregates' own forecast
  and recomputes them as the sum of the reconciled bottom series. `"none"` leaves each
  node (bottom and aggregates) forecast independently; the result is then **not** guaranteed to be coherent
  (`coherent: false`) — useful for comparison, or when the client only cares about the per-level view.
- Each series (bottom + aggregates) goes through the **same** pipeline as without a hierarchy: backtest with
  rolling origins, model selection by RMSE, conformal calibration. Reconciliation only comes in
  **downstream**, on the final point and the bands, never on `model`/`metrics`/`calibration` (which
  remain the performance of the model specific to that node, useful for judging whether reconciliation
  was worthwhile at that spot).
- **Bands**: shifted by the same delta as the point (`lower/upper += reconciled_point - raw_point`),
  then floored at 0 if all the node's historical values are ≥ 0. This is a documented
  approximation (no "exact" reconciliation of quantiles, which has no simple closed-form solution):
  conformal calibration then applies normally on the shifted bands.
- **Scenarios**: reconciled with the **same** matrix (same `W`) as the base forecast, THEN
  the scenario's `adjustments` are applied to the **bottom** series only, and the aggregates are
  **recomputed by summation** of the adjusted bottom series (not re-reconciled) — otherwise an adjustment on a
  store would not be reflected in the scenario total.
- **Events on an aggregate**: each aggregate receives, period by period, the **unweighted
  average of its DIRECT children** (propagated level by level, not the average of all
  the leaves of the subtree — a two-level aggregate with one child strongly favored by an event and
  another indifferent to the numerous children of the former must not dilute the first). Events
  not applicable to a series (filtered by the event's `groups`) count as 0 in
  this average. The same learning rule (significant gap over the history, § events)
  then applies to the aggregate as to a bottom series.
- **Limits**: `max_series` counts **all** the series in the response, aggregates included (not
  only the bottom groups); the 413 error states this (French message: "agrégats de hiérarchie compris").
- Label collision: if a hierarchy column value coincides with `"Total"`, with a
  value of ANOTHER hierarchy column, or with a value of `group_var`, explicit 400 (two
  nodes of different levels would be indistinguishable in the response, which identifies a node only
  by `group` + `level`). The same label reused by two different PARENTS at the same level
  (e.g. the state "nsw" under two different travel purposes) is not a collision: it is a normal
  hierarchy, just not deducible from the label alone outside the context of the request.
- The bottom series must cover exactly the same dates after regularization (same frequency grid):
  otherwise 400 (`field: "hierarchy"`), which names the series concerned — summing the
  aggregates requires it.

Response, per series (only if the request contains `hierarchy`):

```json
"level": "total",
"group": "Total"
```

`level`: `"total"` | `"<hierarchy column>"` | `"bottom"`. `group`: `"Total"` for the root,
the column value for an intermediate aggregate, the `group_var` value for a
bottom series. Order of the series in `series[]`: total, then each intermediate level (from highest to
lowest, values in order of first appearance among the bottom series), then the bottom.

Response root (only if `hierarchy`):

```json
"reconciliation": {
  "method": "mint_shrink",
  "coherent": true,
  "backtest": {"mase_base": 0.91, "mase_reconciled": 0.87, "points": 36}
}
```

`method`: method **actually** used (`"mint_shrink"` | `"bottom_up"` | `"none"`) — may
differ from the requested `reconciliation` in case of fallback. `coherent`: `true` if each aggregate is
guaranteed equal to the sum of its children (`"mint_shrink"`/`"bottom_up"`), `false` otherwise (`"none"`).
`backtest` (absent if `method: "none"`): mean MASE, pooled over all nodes and all backtest
windows, **before** (`mase_base`, independent forecasts) and **after**
(`mase_reconciled`) reconciliation; `W` is re-estimated at each window excluding THAT window
("leave-one-window-out") so as not to be judged on the errors that were used to self-correct.

**Implementation note (documented choice, the contract is ambiguous on this point):** a
scenario adjustment always re-reconciles/aggregates the aggregates by summing the bottom series, including for a scenario
without any `adjustments` (the operation is a no-op in that case, so it has no observable effect, but
it simplifies the implementation by avoiding separate code for this case).

### Errors

Same format as the contract: `400/401/404/413`
`{"status":"error","errors":[{"field":..., "message":"..."}]}` — `field` is
`null` (JSON) when the error does not concern a specific field (not `{}`:
all endpoints serialize as `application/json;charset=utf-8` via
`reqres::format_json(auto_unbox = TRUE, null = "null")`, which renders R
`NULL` as JSON `null` rather than plumber2's default — `{}` — see
`plumber.R`). Messages are in French, accented (UTF-8), actionable
(available columns listed, available models listed, etc.). No
invalid input produces a `500`: every foreseeable error is
intercepted upstream of model fitting
(`api_v1_validate_request()`), and an isolated fitting failure on one series
(in a multi-group case) is reported as a warning at the level of
that series rather than making the whole request fail.

## Logs

One JSON line per request on `stdout` (`api_v1_log_request()`):
`ts, id, route, status, duration_ms, n_rows, n_series, models`. No
user data (no `data` values, no IP, no token).

## Known limitation (documented workaround)

`modeltime` registers its custom model implementations
(`naive_reg`, `arima_reg`, `exp_smoothing`, `prophet_reg`, ...) with
`parsnip` in a way that in practice requires the `modeltime` namespace to be
**attached** (`library(modeltime)`) and not just loaded via
`modeltime::...`: without this, `parsnip::fit()` fails with
`could not find function '..._fit_impl'`. Verified by direct reproduction
(identical call with and without a prior `library(modeltime)`). `entrypoint.R`
and `tests/testthat/setup.R` therefore explicitly attach `modeltime` and
`parsnip` (including in the `mirai` daemons via `mirai::everywhere()`).
This behavior affects all the `th2_*_engine()` functions of the package,
not just the API v1 code.
