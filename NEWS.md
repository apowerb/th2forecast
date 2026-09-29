# th2forecast (development version)

## API v1 (`feat/api-v1-json`)

- **Added**: implementation of the API v1 contract (`GET /health`,
  `POST /v1/forecast`, `POST /v1/jobs` + `GET /v1/jobs/{id}`) in JSON, with
  optional Bearer authentication (`TH2FORECAST_API_TOKEN`), configurable
  limits (`TH2FORECAST_MAX_ROWS`, `TH2FORECAST_MAX_SERIES`,
  `TH2FORECAST_MAX_HORIZON`), full input validation with actionable 400
  errors in French, frequency detection and gap regularization
  (`timetk`), conformal intervals (`modeltime`), backtest and model
  comparison with a naive/snaive baseline.
- **Removed**: the old `POST /forecast` endpoint (serialized R base64 input,
  `R/plumber_th2_forecast.R`) is deleted. It was written for the `plumber`
  v1 API (`function(res, input_data, ...)` + `@param` tags) whereas the
  service runs under **plumber2**, whose handler model is different
  (`body`/`query`/`response` as named arguments): the file could therefore
  never be run as is. Its `tryCatch(..., error = function(e) {...;
  return(...)})` calls did not stop the execution of `forecast()` on error
  anyway (a `return()` in the `error` callback of a `tryCatch` does not make
  the calling function exit), which would have produced an opaque `500` on
  any invalid input even if it had been fixed for plumber2.
- **Critical fix**: `uvr.toml` declared `th2forecast` as its own Git
  dependency (`[dependencies.th2forecast] git =
  "apowerb/th2forecast"`), pinned by `uvr.lock` to a specific GitHub commit.
  As a result, `uvr sync` installed the package from a remote GitHub
  tarball, **never from the repository's local sources** — any local change
  to `R/` was therefore invisible in the built Docker image. Fixed: the
  self-referential dependency is removed from the manifest; the `Dockerfile`
  now installs `th2forecast` from the local sources
  (`install.packages('.', repos = NULL, type = 'source')`) after `uvr sync`
  (which now only installs third-party dependencies).
- **Fix**: `DESCRIPTION` declared `bizdays` and `echarts4r` in `Imports`
  (used by `R/evaluation_models.R`, `R/mod_basic_fcast_viewer.R`,
  `R/preprocessing_dataset.R`, `R/forecast_viewer_helpers.R`) without
  `uvr.toml` installing them: the package was therefore **never actually
  installable from the sources** before this fix (masked until now by the
  self-referential dependency above, which bypassed any real local
  installation). Added to `uvr.toml`/`uvr.lock`.
- **Fix**: license `GLP-3` (typo) corrected to
  `Apache License (== 2.0)` in `DESCRIPTION`, consistent with the `LICENSE`
  file actually present in the repository (Apache-2.0), and not MIT.
- **Fix**: `Dockerfile` pinned `uvr` via
  `.../main/install.sh` (not reproducible); now pinned to the `v0.4.6` tag.
  Single entry point (`entrypoint.R`); `run_api.R` (duplicate) and
  `main_service.R` (file corrupted in UTF-16, not runnable) deleted.
- **Added**: `.Rhistory` removed from Git tracking (already in `.gitignore`
  but still indexed since an earlier commit).
- **Known limitation**: `/v1/jobs` uses `mirai` for real asynchronous
  execution (daemons created in `entrypoint.R`); when no daemon is
  available at startup, the computation is run synchronously at `POST` time
  (fallback documented in `docs/API.md`).

# th2test (development version)

* Initial CRAN submission.
