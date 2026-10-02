"""Contrat HTTP v1 : validation, fréquence et régularisation, alignés sur R/api_v1_*.R."""
from __future__ import annotations

import math
import re
from dataclasses import dataclass, field
from datetime import date, timedelta

import numpy as np
import pandas as pd

from .env import env_int

ALLOWED_MODELS = ["prophet", "arima", "ets", "snaive", "naive", "croston", "tsb", "imapa", "auto"]
ALLOWED_FREQUENCIES = ["day", "week", "month", "quarter", "year"]
SEASONAL_PERIOD = {"day": 7, "week": 52, "month": 12, "quarter": 4, "year": 1}
# Année-d'abord ISO (non ambigu, prioritaire) ; une heure éventuelle en suffixe
# est ignorée car la regex n'ancre que le préfixe.
_DATE_ISO_RE = re.compile(r"^\s*(\d{4})[-/](\d{1,2})[-/](\d{1,2})")
# Jour-d'abord européen (06/01/2026) : seulement si l'ISO ne correspond pas.
# Année à 4 chiffres exigée — deviner le siècle d'une année à 2 chiffres serait
# trop risqué.
# NB : cette tolérance jour-d'abord est propre au chemin Python (image déployée
# `th2forecast-py`). R/api_v1_validate.R garde `as.Date` (ISO strict) ; les deux
# implémentations divergent donc sur ce point précis, à réaligner si l'image R
# est un jour servie.
_DATE_DMY_RE = re.compile(r"^\s*(\d{1,2})[/-](\d{1,2})[/-](\d{4})")


def error(field: str | None, message: str) -> dict:
    return {"field": field, "message": message}


def error_body(errors: list[dict]) -> dict:
    return {"status": "error", "errors": errors}


def limits_from_env() -> dict:
    return {
        "max_rows": env_int("TH2FORECAST_MAX_ROWS", 100000),
        "max_series": env_int("TH2FORECAST_MAX_SERIES", 200),
        "max_horizon": env_int("TH2FORECAST_MAX_HORIZON", 366),
    }


@dataclass
class Request:
    df: pd.DataFrame  # colonnes : date (datetime.date), value (float), group (str | None)
    horizon: int
    frequency: str | None
    models: list[str]
    confidence_levels: list[float]
    has_group: bool
    events: list = field(default_factory=list)  # context.Event
    scenarios: list = field(default_factory=list)  # context.Scenario
    context_warnings: list[str] = field(default_factory=list)
    hierarchy_cols: list[str] = field(default_factory=list)  # colonnes de hiérarchie, du plus haut au plus bas
    reconciliation: str = "none"  # "mint" | "bottom_up" | "none"
    hierarchy: object = None  # hierarchy.Hierarchy | None (arbre + matrice S, structure seulement)
    feedback: list[dict] = field(default_factory=list)  # bandes prévues passées, pour l'ACI (contrat §1)


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
    # ISO année-d'abord d'abord (non ambigu), sinon jour-d'abord J/M/AAAA
    # (défaut européen). L'heure éventuelle en suffixe est ignorée.
    m = _DATE_ISO_RE.match(s or "")
    if m:
        y, mo, d = int(m.group(1)), int(m.group(2)), int(m.group(3))
    else:
        m = _DATE_DMY_RE.match(s or "")
        if not m:
            return None
        d, mo, y = int(m.group(1)), int(m.group(2)), int(m.group(3))
    try:
        return date(y, mo, d)
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

    from . import hierarchy as hie  # import tardif : hierarchy dépend de ce module

    hierarchy_cols, reconciliation = hie.parse_fields(body.get("hierarchy"), body.get("reconciliation"), group_var, errors)

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

    cols_to_check = [("date_var", date_var), ("target_var", target_var), ("group_var", group_var)]
    cols_to_check += [("hierarchy", h) for h in hierarchy_cols]
    for field, col in cols_to_check:
        if col is not None and col not in columns:
            raise Invalid(400, [error(field, "Colonne '%s' absente ; colonnes disponibles : %s."
                                      % (col, ", ".join(columns)))])

    raw_dates = [_as_character(r.get(date_var)) for r in rows]
    dates = [_parse_date(s) for s in raw_dates]
    bad = list(dict.fromkeys(s if s is not None else "" for s, d in zip(raw_dates, dates) if d is None))
    if bad:
        raise Invalid(400, [error("data", "Dates non parsables dans la colonne '%s' (formats acceptés : YYYY-MM-DD ou JJ/MM/AAAA) : %s."
                                  % (date_var, ", ".join(bad[:5])))])

    values = [_as_numeric(_as_character(r.get(target_var))) for r in rows]
    if any(v is None for v in values):
        raise Invalid(400, [error("target_var", "La colonne cible '%s' doit être numérique." % target_var)])

    groups = [_as_character(r.get(group_var)) for r in rows] if group_var else [None] * len(rows)
    df = pd.DataFrame({"date": dates, "value": values, "group": groups,
                       **{h: [_as_character(r.get(h)) for r in rows] for h in hierarchy_cols}})
    order = list(dict.fromkeys(groups))

    hierarchy = hie.build(df, order, hierarchy_cols)  # 400 : parent non unique, collision de libellé
    n_total = len(hierarchy.nodes) if hierarchy else len(order)
    if n_total > limits["max_series"]:
        suffix = ", agrégats de hiérarchie compris" if hierarchy else ""
        raise Invalid(413, [error("group_var", "Nombre de séries (%d%s) supérieur à la limite autorisée (%d)."
                                  % (n_total, suffix, limits["max_series"]))])

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

    from . import context  # import tardif : context dépend de ce module

    ctx_errors, ctx_warnings = [], []
    events = context.parse_events(body.get("events"), ctx_errors)
    scenarios = context.parse_scenarios(body.get("scenarios"), {e.name for e in events}, ctx_errors, ctx_warnings)
    if ctx_errors:
        raise Invalid(400, [error("scenarios" if m.startswith("scenarios") or m.startswith("'scenarios'") else "events", m)
                            for m in ctx_errors])

    fb_errors: list = []
    feedback = parse_feedback(body.get("feedback"), fb_errors)
    if fb_errors:
        raise Invalid(400, fb_errors)

    return Request(df=df, horizon=horizon, frequency=frequency, models=models,
                   confidence_levels=sorted(set(float(x) for x in levels)), has_group=bool(group_var),
                   events=events, scenarios=scenarios, context_warnings=ctx_warnings,
                   hierarchy_cols=hierarchy_cols, reconciliation=reconciliation, hierarchy=hierarchy,
                   feedback=feedback)


def parse_feedback(raw, errors: list) -> list[dict]:
    """Feedback (contrat §1) : bandes prévues passées, appariées plus tard par le moteur aux dates
    de son historique régularisé. Validation de forme seulement ; l'appariement et l'ACI sont du
    ressort de forecast.py (dates/séries dépendent du résultat du backtest)."""
    if raw is None:
        return []
    if not isinstance(raw, list):
        errors.append(error("feedback", "Le champ 'feedback' doit être une liste."))
        return []
    out = []
    for i, item in enumerate(raw):
        if not isinstance(item, dict) or "group" not in item:
            errors.append(error("feedback", "Élément %d de 'feedback' invalide : attend un objet avec au "
                                "moins 'group' et 'points'." % i))
            continue
        level = item.get("level")
        if level is not None and not isinstance(level, str):
            errors.append(error("feedback", "Élément %d de 'feedback' : 'level' doit être une chaîne ou null." % i))
            continue
        raw_points = item.get("points")
        if not isinstance(raw_points, list) or not raw_points:
            errors.append(error("feedback", "Élément %d de 'feedback' : 'points' doit être une liste non vide." % i))
            continue

        points, bad = [], False
        for j, p in enumerate(raw_points):
            if not isinstance(p, dict):
                errors.append(error("feedback", "Élément %d de 'feedback', point %d : objet attendu." % (i, j)))
                bad = True
                continue
            d = _parse_date(_as_character(p.get("date")))
            if d is None:
                errors.append(error("feedback", "Élément %d de 'feedback', point %d : date invalide "
                                    "(formats acceptés : YYYY-MM-DD ou JJ/MM/AAAA)." % (i, j)))
                bad = True
                continue
            bounds, ok = {}, True
            for k, v in p.items():
                if k == "date":
                    continue
                if not (isinstance(v, (int, float)) and not isinstance(v, bool)):
                    errors.append(error("feedback", "Élément %d de 'feedback', point %d : '%s' doit être numérique."
                                        % (i, j, k)))
                    ok = False
            if not ok:
                bad = True
                continue
            tags_lo = {k[len("lower_"):] for k in p if k.startswith("lower_")}
            tags_hi = {k[len("upper_"):] for k in p if k.startswith("upper_")}
            for tag in tags_lo & tags_hi:
                bounds[tag] = (float(p["lower_" + tag]), float(p["upper_" + tag]))
            points.append({"date": d, "bounds": bounds})
        if bad:
            continue
        out.append({"group": item.get("group"), "level": level, "points": points})
    return out


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
