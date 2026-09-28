"""Service HTTP th2forecast (contrat v1, identique à l'API R plumber2)."""
from __future__ import annotations

import json
import math
import os
import random
import string
import sys
import threading
import time
from concurrent.futures import ThreadPoolExecutor
from contextlib import asynccontextmanager
from datetime import datetime, timezone

import anyio
from fastapi import FastAPI, Request
from fastapi.responses import JSONResponse

from . import __version__
from . import contract as c
from .engine import Engine
from .forecast import run_forecast

AUTH_MESSAGE = "Authentification requise : en-tête 'Authorization: Bearer <token>' manquant ou invalide."
ENGINE = Engine()
WORKERS = int(os.environ.get("TH2FORECAST_WORKERS", "2"))
_pool = ThreadPoolExecutor(max_workers=WORKERS) if WORKERS > 0 else None
_jobs: dict[str, dict] = {}
_jobs_lock = threading.Lock()


def new_id() -> str:
    return "%d-%s" % (time.time(), "".join(random.choices(string.ascii_lowercase + string.digits, k=10)))


def log_request(route, status, duration_ms, n_rows=None, n_series=None, models=None) -> None:
    """Une ligne JSON par requête sur stdout, sans aucune donnée utilisateur."""
    entry = {
        "ts": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%S.%f")[:-3] + "Z",
        "id": new_id(), "route": route, "status": status, "duration_ms": round(duration_ms),
    }
    if n_rows is not None:
        entry["n_rows"] = n_rows
    if n_series is not None:
        entry["n_series"] = n_series
    if models is not None:
        entry["models"] = models
    print(json.dumps(entry, ensure_ascii=False), file=sys.stdout, flush=True)


def authorized(request: Request) -> bool:
    expected = os.environ.get("TH2FORECAST_API_TOKEN", "")
    if not expected:
        return True
    header = request.headers.get("authorization", "")
    provided = header[7:].strip() if header.lower().startswith("bearer ") else header
    return bool(header) and provided == expected


def _finite(x):
    """NaN et infinis deviennent null (comme les NA de R), jamais une réponse invalide."""
    if isinstance(x, float):
        return x if math.isfinite(x) else None
    if isinstance(x, dict):
        return {k: _finite(v) for k, v in x.items()}
    if isinstance(x, list):
        return [_finite(v) for v in x]
    return x


def reply(status: int, body: dict) -> JSONResponse:
    return JSONResponse(status_code=status, content=_finite(body), media_type="application/json;charset=utf-8")


async def read_json(request: Request):
    try:
        return json.loads(await request.body())
    except (ValueError, UnicodeDecodeError):
        return None


def _describe(body):
    data = body.get("data") if isinstance(body, dict) else None
    models = body.get("models") if isinstance(body, dict) else None
    return (len(data) if isinstance(data, list) else None,
            models if isinstance(models, list) else ([models] if isinstance(models, str) else None))


@asynccontextmanager
async def lifespan(_: FastAPI):
    if os.environ.get("TH2FORECAST_PRELOAD", "1") == "1":
        ENGINE.chronos()
    yield


app = FastAPI(title="th2forecast", version=__version__, lifespan=lifespan)


@app.get("/health")
def health():
    return reply(200, {"status": "UP", "version": __version__})


@app.post("/v1/forecast")
async def forecast(request: Request):
    t0 = time.perf_counter()
    if not authorized(request):
        return reply(401, c.error_body([c.error(None, AUTH_MESSAGE)]))
    body = await read_json(request)
    status, out = await anyio.to_thread.run_sync(run_forecast, body, c.limits_from_env(), ENGINE)
    n_rows, models = _describe(body)
    log_request("POST /v1/forecast", status, (time.perf_counter() - t0) * 1000, n_rows,
                len(out["series"]) if status == 200 else None, models)
    return reply(status, out)


def _job(job_id: str, body, limits) -> None:
    with _jobs_lock:
        _jobs[job_id]["status"] = "running"
    try:
        _, out = run_forecast(body, limits, ENGINE)
        update = {"status": "succeeded", "result": out, "error": None}
    except Exception:
        update = {"status": "failed", "result": None, "error": "Échec inattendu du calcul de prévision."}
    with _jobs_lock:
        _jobs[job_id].update(update)


@app.post("/v1/jobs")
async def create_job(request: Request):
    t0 = time.perf_counter()
    if not authorized(request):
        return reply(401, c.error_body([c.error(None, AUTH_MESSAGE)]))
    body = await read_json(request)
    limits = c.limits_from_env()
    try:
        c.validate(body, limits)
    except c.Invalid as e:
        log_request("POST /v1/jobs", e.status, (time.perf_counter() - t0) * 1000, _describe(body)[0])
        return reply(e.status, c.error_body(e.errors))

    job_id = new_id()
    with _jobs_lock:
        _jobs[job_id] = {"status": "queued", "result": None, "error": None}
    if _pool is None:
        _job(job_id, body, limits)
    else:
        _pool.submit(_job, job_id, body, limits)
    log_request("POST /v1/jobs", 202, (time.perf_counter() - t0) * 1000, _describe(body)[0])
    return reply(202, {"job_id": job_id, "status": "queued"})


@app.get("/v1/jobs/{job_id}")
def get_job(job_id: str, request: Request):
    if not authorized(request):
        return reply(401, c.error_body([c.error(None, AUTH_MESSAGE)]))
    with _jobs_lock:
        job = dict(_jobs[job_id]) if job_id in _jobs else None
    if job is None:
        return reply(404, c.error_body([c.error("job_id", "Job '%s' inconnu." % job_id)]))
    return reply(200, {"job_id": job_id, **job})
