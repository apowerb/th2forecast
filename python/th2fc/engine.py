"""Moteur de prévision : statsforecast, prophet et Chronos-2, backtest en origines glissantes.

Toutes les séries d'une requête sont traitées ensemble : un seul appel statsforecast et un
seul appel Chronos-2 par couple (horizon, saisonnalité), fenêtres de backtest comprises.
"""
from __future__ import annotations

import logging
import math
import os
import threading
from dataclasses import dataclass, field

import numpy as np
import pandas as pd

log = logging.getLogger("th2fc")

ENSEMBLE = ("chronos2", "ets", "arima", "theta")
STATS = {"ets", "arima", "theta", "naive", "snaive"}
MAX_WINDOWS = 3


@dataclass
class Task:
    """Une prévision à produire : une série (ou le début d'une série) et un horizon."""
    key: tuple  # (indice de série, fenêtre) ; fenêtre = None pour la prévision finale
    y: np.ndarray
    dates: list
    h: int
    season: int
    models: set[str]
    out: dict = field(default_factory=dict)  # modèle -> (point, {niveau: (bas, haut)})


def quantile_pair(level: float) -> tuple[float, float]:
    return round((1 - level) / 2, 6), round((1 + level) / 2, 6)


def pct(level: float):
    x = round(level * 100, 6)
    return int(x) if float(x).is_integer() else x


class Engine:
    def __init__(self, chronos_path: str | None = None):
        self._chronos = None
        self._chronos_path = chronos_path or os.environ.get("TH2FORECAST_CHRONOS_PATH", "/opt/models/chronos-2")
        self._lock = threading.RLock()  # chronos() est aussi appelé sous ce verrou
        self.n_jobs = int(os.environ.get("TH2FORECAST_SF_JOBS", "1"))

    # -- Chronos-2 ------------------------------------------------------------
    def chronos(self):
        with self._lock:
            if self._chronos is None:
                import torch
                from chronos import BaseChronosPipeline

                torch.set_num_threads(int(os.environ.get("TH2FORECAST_TORCH_THREADS", os.cpu_count() or 1)))
                self._chronos = BaseChronosPipeline.from_pretrained(self._chronos_path, device_map="cpu")
            return self._chronos

    def _run_chronos(self, tasks: list[Task], levels: list[float]) -> None:
        qs = sorted({0.5} | {q for lv in levels for q in quantile_pair(lv)})
        for h in sorted({t.h for t in tasks}):
            batch = [t for t in tasks if t.h == h]
            ctx = pd.concat([
                pd.DataFrame({"id": str(i), "timestamp": pd.date_range("2000-01-01", periods=len(t.y), freq="D"), "target": t.y})
                for i, t in enumerate(batch)
            ])
            with self._lock:
                pred = self.chronos().predict_df(ctx, prediction_length=h, quantile_levels=qs,
                                                 id_column="id", timestamp_column="timestamp", target="target")
            cols = {round(float(c), 6): c for c in pred.columns if _is_float(c)}
            for i, t in enumerate(batch):
                p = pred[pred["id"] == str(i)]
                bounds = {lv: (p[cols[quantile_pair(lv)[0]]].to_numpy(), p[cols[quantile_pair(lv)[1]]].to_numpy()) for lv in levels}
                t.out["chronos2"] = (p[cols[0.5]].to_numpy(), bounds)

    # -- statsforecast ---------------------------------------------------------
    def _run_stats(self, tasks: list[Task], levels: list[float]) -> None:
        from statsforecast import StatsForecast
        from statsforecast.models import AutoARIMA, AutoETS, AutoTheta, Naive, SeasonalNaive

        for h, season in sorted({(t.h, t.season) for t in tasks}):
            batch = [t for t in tasks if (t.h, t.season) == (h, season)]
            wanted = set().union(*(t.models & STATS for t in batch))
            if not wanted:
                continue
            builders = {
                "ets": lambda: AutoETS(season_length=season, alias="ets"),
                "arima": lambda: AutoARIMA(season_length=season, alias="arima"),
                "theta": lambda: AutoTheta(season_length=season, alias="theta"),
                "naive": lambda: Naive(alias="naive"),
                "snaive": lambda: SeasonalNaive(season_length=season, alias="snaive"),
            }
            models = [builders[m]() for m in sorted(wanted)]
            df = pd.concat([pd.DataFrame({"unique_id": str(i), "ds": np.arange(len(t.y)), "y": t.y})
                            for i, t in enumerate(batch)])
            lv = [pct(x) for x in levels]
            sf = StatsForecast(models=models, freq=1, n_jobs=self.n_jobs, fallback_model=Naive(alias="fallback"))
            fc = sf.forecast(df=df, h=h, level=lv)
            for i, t in enumerate(batch):
                f = fc[fc["unique_id"] == str(i)]
                for m in sorted(wanted & t.models):
                    bounds = {x: (f[f"{m}-lo-{pct(x)}"].to_numpy(), f[f"{m}-hi-{pct(x)}"].to_numpy()) for x in levels}
                    t.out[m] = (f[m].to_numpy(), bounds)

    # -- prophet ------------------------------------------------------------------
    def _run_prophet(self, tasks: list[Task], levels: list[float], frequency: str, step) -> None:
        from prophet import Prophet

        logging.getLogger("cmdstanpy").setLevel(logging.WARNING)
        for t in tasks:
            if "prophet" not in t.models:
                continue
            m = Prophet(uncertainty_samples=500)
            m.fit(pd.DataFrame({"ds": pd.to_datetime(t.dates), "y": t.y}))
            future = pd.DataFrame({"ds": pd.to_datetime([step(t.dates[-1], frequency, k) for k in range(1, t.h + 1)])})
            point = m.predict(future)["yhat"].to_numpy()
            samples = m.predictive_samples(future)["yhat"]
            bounds = {lv: tuple(np.quantile(samples, quantile_pair(lv), axis=1)) for lv in levels}
            t.out["prophet"] = (point, bounds)

    # -- orchestration ----------------------------------------------------------
    def run(self, tasks: list[Task], levels: list[float], frequency: str, step) -> None:
        chronos_tasks = [t for t in tasks if "chronos2" in t.models]
        if chronos_tasks:
            self._run_chronos(chronos_tasks, levels)
        self._run_stats(tasks, levels)
        self._run_prophet(tasks, levels, frequency, step)
        for t in tasks:
            if "ensemble" in t.models:
                parts = [t.out[m] for m in ENSEMBLE if m in t.out]
                point = np.mean([p[0] for p in parts], axis=0)
                bounds = {lv: (np.mean([p[1][lv][0] for p in parts], axis=0), np.mean([p[1][lv][1] for p in parts], axis=0))
                          for lv in levels}
                t.out["ensemble"] = (point, bounds)


def _is_float(c) -> bool:
    try:
        float(c)
        return True
    except (TypeError, ValueError):
        return False


def components(model: str) -> set[str]:
    return set(ENSEMBLE) | {"ensemble"} if model == "ensemble" else {model}


def windows(n: int, horizon: int) -> tuple[int, list[int]]:
    """Taille de fenêtre de backtest et origines (nombre de points d'entraînement) de chaque fenêtre."""
    h_cv = max(2, min(horizon, math.floor(0.2 * n)))
    h_cv = min(h_cv, max(2, n - 3))
    min_train = max(8, math.ceil(n / 2))
    k = max(1, min(MAX_WINDOWS, (n - min_train) // h_cv))
    return h_cv, [n - h_cv * (k - w) for w in range(k)]


def metrics(errors: list[tuple[np.ndarray, np.ndarray, np.ndarray]]) -> dict:
    """Métriques poolées sur les fenêtres : (réel, prévu, entraînement) par fenêtre."""
    y = np.concatenate([e[0] for e in errors])
    p = np.concatenate([e[1] for e in errors])
    e = p - y
    nz = y != 0
    den = np.abs(y) + np.abs(p)
    scaled = []
    for yt, pt, tr in errors:
        scale = np.mean(np.abs(np.diff(tr))) if len(tr) > 1 else 0.0
        scaled.append(np.abs(pt - yt) / scale if scale > 0 else np.full(len(yt), np.nan))
    s = np.concatenate(scaled)
    return {
        "mape": float(np.mean(np.abs(e[nz] / y[nz]))) if nz.any() else None,
        "smape": float(np.mean(2 * np.abs(e[den > 0]) / den[den > 0])) if (den > 0).any() else None,
        "mase": float(np.nanmean(s)) if np.isfinite(s).any() else None,
        "rmse": float(np.sqrt(np.mean(e ** 2))),
    }
