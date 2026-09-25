"""Contrat HTTP v1 : validation, fréquence et régularisation, alignés sur R/api_v1_*.R."""
from __future__ import annotations

import math
import os
import re
from dataclasses import dataclass
from datetime import date, timedelta

import numpy as np
import pandas as pd

ALLOWED_MODELS = ["prophet", "arima", "ets", "snaive", "naive", "auto"]
ALLOWED_FREQUENCIES = ["day", "week", "month", "quarter", "year"]
SEASONAL_PERIOD = {"day": 7, "week": 52, "month": 12, "quarter": 4, "year": 1}
_DATE_RE = re.compile(r"^\s*(\d{4})[-/](\d{1,2})[-/](\d{1,2})")


def error(field: str | None, message: str) -> dict:
    return {"field": field, "message": message}


def error_body(errors: list[dict]) -> dict:
    return {"status": "error", "errors": errors}


def limits_from_env() -> dict:
    return {
        "max_rows": int(os.environ.get("TH2FORECAST_MAX_ROWS", "100000")),
        "max_series": int(os.environ.get("TH2FORECAST_MAX_SERIES", "200")),
        "max_horizon": int(os.environ.get("TH2FORECAST_MAX_HORIZON", "366")),
    }


@dataclass
class Request:
    df: pd.DataFrame  # colonnes : date (datetime.date), value (float), group (str | None)
    horizon: int
    frequency: str | None
    models: list[str]
    confidence_levels: list[float]
    has_group: bool


class Invalid(Exception):
    def __init__(self, status: int, errors: list[dict]):
        self.status, self.errors = status, errors


def _as_character(v) -> str | None:
    """Équivalent de as.character() sur une valeur JSON (None = NA)."""
    if v is None:
        return None
    if isinstance(v, bool):
        return "TRUE" if v else "FALSE"
    if isinstance(v, float):
        if math.isnan(v):
            return None
        return str(int(v)) if v.is_integer() else repr(v)
    return str(v)


def _parse_date(s: str | None) -> date | None:
    m = _DATE_RE.match(s or "")
    if not m:
        return None
    try:
        return date(int(m.group(1)), int(m.group(2)), int(m.group(3)))
    except ValueError:
        return None


def _as_numeric(s: str | None) -> float | None:
    if s is None:
        return math.nan
    try:
        return float(s.strip())
    except ValueError:
        return None


def validate(body, limits: dict) -> Request:
    if not isinstance(body, dict):
        raise Invalid(400, [error(None, "Corps de requête JSON manquant ou invalide.")])

    errors: list[dict] = []
    rows = body.get("data")
    if rows is None or (isinstance(rows, list) and len(rows) == 0):
        errors.append(error("data", "Le champ 'data' est requis et doit contenir au moins une ligne."))

    date_var, target_var = body.get("date_var"), body.get("target_var")
    group_var = body.get("group_var") or None
    if not isinstance(date_var, str) or not date_var:
        errors.append(error("date_var", "Le champ 'date_var' est requis."))
    if not isinstance(target_var, str) or not target_var:
        errors.append(error("target_var", "Le champ 'target_var' est requis."))

    horizon = body.get("horizon")
    if isinstance(horizon, bool) or not isinstance(horizon, (int, float)) or horizon <= 0 or horizon != int(horizon):
        errors.append(error("horizon", "Le champ 'horizon' doit être un entier strictement positif."))
        horizon = None
    else:
        horizon = int(horizon)

    models = body.get("models")
    if models is None or models == [] or models == "":
        models = ["auto"]
    if isinstance(models, str):
        models = [models]
    models = [str(m) for m in models]
    unknown = [m for m in dict.fromkeys(models) if m not in ALLOWED_MODELS]
    if unknown:
        errors.append(error("models", "Modèle(s) inconnu(s) : %s ; modèles disponibles : %s."
                            % (", ".join(unknown), ", ".join(ALLOWED_MODELS))))

    frequency = body.get("frequency")
    if frequency == "":
        frequency = None
    if frequency is not None and frequency not in ALLOWED_FREQUENCIES:
        errors.append(error("frequency", "Fréquence '%s' inconnue ; valeurs acceptées : %s (ou null pour détection automatique)."
                            % (frequency, ", ".join(ALLOWED_FREQUENCIES))))
        frequency = None

    levels = body.get("confidence_levels")
    if levels is None or levels == []:
        levels = [0.8, 0.95]
    else:
        levels = levels if isinstance(levels, list) else [levels]
        if not all(isinstance(x, (int, float)) and not isinstance(x, bool) and 0 < x < 1 for x in levels):
            errors.append(error("confidence_levels", "Les niveaux de confiance doivent être des nombres dans l'intervalle ouvert (0, 1)."))
            levels = [0.8, 0.95]

    if errors:
        raise Invalid(400, errors)

    if horizon > limits["max_horizon"]:
        raise Invalid(413, [error("horizon", "Horizon demandé (%d) supérieur à la limite autorisée (%d)."
                                  % (horizon, limits["max_horizon"]))])

    if not isinstance(rows, list) or not all(isinstance(r, dict) for r in rows):
        raise Invalid(400, [error("data", "Impossible d'interpréter 'data' comme un tableau de lignes homogènes.")])
    columns = list(dict.fromkeys(k for r in rows for k in r))
    if not columns:
        raise Invalid(400, [error("data", "Impossible d'interpréter 'data' comme un tableau de lignes homogènes.")])

    if len(rows) > limits["max_rows"]:
        raise Invalid(413, [error("data", "Nombre de lignes (%d) supérieur à la limite autorisée (%d)."
                                  % (len(rows), limits["max_rows"]))])

    for field, col in (("date_var", date_var), ("target_var", target_var), ("group_var", group_var)):
        if col is not None and col not in columns:
            raise Invalid(400, [error(field, "Colonne '%s' absente ; colonnes disponibles : %s."
                                      % (col, ", ".join(columns)))])

    raw_dates = [_as_character(r.get(date_var)) for r in rows]
    dates = [_parse_date(s) for s in raw_dates]
    bad = list(dict.fromkeys(s if s is not None else "" for s, d in zip(raw_dates, dates) if d is None))
    if bad:
        raise Invalid(400, [error("data", "Dates non parsables dans la colonne '%s' (format attendu YYYY-MM-DD) : %s."
                                  % (date_var, ", ".join(bad[:5])))])

    values = [_as_numeric(_as_character(r.get(target_var))) for r in rows]
    if any(v is None for v in values):
        raise Invalid(400, [error("target_var", "La colonne cible '%s' doit être numérique." % target_var)])

    groups = [_as_character(r.get(group_var)) for r in rows] if group_var else [None] * len(rows)
    df = pd.DataFrame({"date": dates, "value": values, "group": groups})
    order = list(dict.fromkeys(groups))

    if len(order) > limits["max_series"]:
        raise Invalid(413, [error("group_var", "Nombre de séries (%d) supérieur à la limite autorisée (%d)."
                                  % (len(order), limits["max_series"]))])

    def label(g):
        return "" if g is None else " pour la série '%s'" % g

    dups = []
    for g in order:
        d = df.loc[_mask(df, g), "date"]
        repeated = list(dict.fromkeys(d[d.duplicated()]))
        if repeated:
            dups.append("%s : %s" % (label(g), ", ".join(x.isoformat() for x in repeated[:5])))
    if dups:
        raise Invalid(400, [error("data", "Doublons de dates détectés%s." % " ; ".join(dups))])

    min_points = max(10, horizon + 1)
    short = []
    for g in order:
        n = int(_mask(df, g).sum())
        if n < min_points:
            short.append("Historique trop court%s : %d points pour un horizon de %d. Il faut au moins %d points, ou réduire l'horizon à %d au plus."
                         % (label(g), n, horizon, min_points, max(n - 1, 0)))
    if short:
        raise Invalid(400, [error("horizon", " ; ".join(short))])

    return Request(df=df, horizon=horizon, frequency=frequency, models=models,
                   confidence_levels=sorted(set(float(x) for x in levels)), has_group=bool(group_var))


def _mask(df: pd.DataFrame, g) -> pd.Series:
    return df["group"].isna() if g is None else df["group"] == g


def detect_frequency(dates) -> str:
    d = sorted(set(dates))
    if len(d) < 2:
        return "day"
    med = float(np.median([(b - a).days for a, b in zip(d, d[1:])]))
    if med <= 3:
        return "day"
    if med <= 10:
        return "week"
    if med <= 45:
        return "month"
    if med <= 135:
        return "quarter"
    return "year"


def step(d: date, frequency: str, k: int) -> date:
    """Date située k pas après d (mois en fin de mois bornés, comme DateOffset)."""
    if frequency == "day":
        return d + timedelta(days=k)
    if frequency == "week":
        return d + timedelta(weeks=k)
    months = {"month": 1, "quarter": 3, "year": 12}[frequency] * k
    return (pd.Timestamp(d) + pd.DateOffset(months=months)).date()


def regularize(dates: list[date], values: list[float], frequency: str) -> tuple[list[date], np.ndarray, int]:
    """Comble les trous au pas de fréquence puis interpole linéairement (approx(rule = 2))."""
    known = dict(zip(dates, values))
    start, end = min(dates), max(dates)
    grid, k = [], 0
    while (d := step(start, frequency, k)) <= end:
        grid.append(d)
        k += 1
    full = sorted(set(grid) | set(dates))
    n_padded = len(full) - len(dates)
    v = np.array([known.get(d, math.nan) for d in full], dtype=float)
    ok = ~np.isnan(v)
    if not ok.all():
        idx = np.arange(len(v))
        if ok.sum() >= 2:
            v = np.interp(idx, idx[ok], v[ok])
        else:
            v[~ok] = v[ok][0] if ok.any() else 0.0
    return full, v, n_padded
