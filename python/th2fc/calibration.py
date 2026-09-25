"""Calibration conforme des bandes sur les fenêtres de backtest.

Pour chaque point testé, le score est le facteur d'élargissement de la bande du modèle qu'il
aurait fallu pour contenir la valeur réelle (1 = pile sur la borne). Le quantile conforme de ces
scores donne le facteur appliqué à la bande finale : couverture garantie en moyenne si les
erreurs passées ressemblent aux futures (échangeabilité), quelle que soit la qualité du modèle.
"""
from __future__ import annotations

import math

import numpy as np

EPS = 1e-9


def scores(y, point, lower, upper) -> np.ndarray:
    below = (point - y) / np.maximum(point - lower, EPS)
    above = (y - point) / np.maximum(upper - point, EPS)
    return np.maximum(below, above)


def conformal_factor(s: np.ndarray, level: float) -> float | None:
    """Quantile conforme d'ordre ceil((n + 1) * level) ; None si trop peu de points pour ce niveau."""
    n = len(s)
    k = math.ceil((n + 1) * level - 1e-9)
    if n == 0 or k > n:
        return None
    return float(np.sort(s)[k - 1])


def window_scores(windows: list[tuple], level: float) -> list[np.ndarray]:
    """windows : (réel, prévu, {niveau: (bas, haut)}) par fenêtre de backtest."""
    return [scores(y, p, b[level][0], b[level][1]) for y, p, b in windows]


def calibrate(own: dict, levels: list[float], pool: dict | None = None) -> dict:
    """own : {niveau: [scores par fenêtre]} de la série ; pool : {niveau: scores des autres séries}.

    Facteur tiré des scores de la série, complétés par ceux des autres séries de la requête quand
    la série seule n'en a pas assez pour ce niveau. La couverture calibrée est estimée hors
    échantillon : chaque fenêtre de la série est recalculée sans ses propres scores.
    """
    out = {}
    for lv in levels:
        per_window = own[lv]
        others = (pool or {}).get(lv, np.array([]))
        mine = np.concatenate(per_window) if per_window else np.array([])

        def factor(s_own):
            f = conformal_factor(s_own, lv)
            return f if f is not None else conformal_factor(np.concatenate([s_own, others]), lv)

        held_out = []
        for w, s in enumerate(per_window):
            rest = [x for i, x in enumerate(per_window) if i != w]
            f = factor(np.concatenate(rest) if rest else np.array([]))
            if f is not None:
                held_out.append(s <= f)
        out[lv] = {
            "factor": factor(mine),
            "pooled": conformal_factor(mine, lv) is None and len(others) > 0,
            "raw_coverage": float(np.mean(mine <= 1)) if len(mine) else None,
            "calibrated_coverage": float(np.mean(np.concatenate(held_out))) if held_out else None,
        }
    return out


def apply(point, bounds: dict, calibration: dict) -> dict:
    """Bandes finales élargies (ou resserrées) du facteur de chaque niveau ; inchangées sans facteur."""
    out = {}
    for lv, (lo, hi) in bounds.items():
        f = calibration.get(lv, {}).get("factor")
        out[lv] = (lo, hi) if f is None else (point - f * np.maximum(point - lo, 0), point + f * np.maximum(hi - point, 0))
    return out
