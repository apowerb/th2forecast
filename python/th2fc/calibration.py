"""Calibration conforme des bandes sur les fenêtres de backtest.

Pour chaque point testé, le score est le facteur d'élargissement de la bande du modèle qu'il
aurait fallu pour contenir la valeur réelle (1 = pile sur la borne). Le quantile conforme de ces
scores donne le facteur appliqué à la bande finale : couverture garantie en moyenne si les
erreurs passées ressemblent aux futures (échangeabilité), quelle que soit la qualité du modèle.
"""
from __future__ import annotations

import math
from statistics import NormalDist

import numpy as np

EPS = 1e-9
_ACI_GAMMA = 0.05


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


def adaptive_level(errs: list[int], level: float, gamma: float = _ACI_GAMMA) -> float:
    """ACI (Gibbs & Candès 2021) : alpha_1 = 1-level, alpha_{t+1} = alpha_t + gamma*((1-level)-err_t).

    err_t = 1 si le réel est sorti de la bande passée du niveau, sinon 0. On n'élargit que si le
    réel a débordé : jamais de resserrement sous le niveau demandé (peu de points, asymétrie du
    risque d'une bande trop étroite face à une bande trop large)."""
    alpha = 1.0 - level
    for e in errs:
        alpha = alpha + gamma * ((1.0 - level) - e)
    return min(max(1.0 - alpha, level), 0.995)


def _z(level: float) -> float:
    """Quantile gaussien approximatif d'un intervalle symétrique de couverture `level`."""
    return NormalDist().inv_cdf(0.5 + level / 2)


def calibrate(own: dict, levels: list[float], pool: dict | None = None, adaptive: dict | None = None) -> dict:
    """own : {niveau: [scores par fenêtre]} de la série ; pool : {niveau: scores des autres séries}.

    Facteur tiré des scores de la série, complétés par ceux des autres séries de la requête quand
    la série seule n'en a pas assez pour ce niveau. La couverture calibrée est estimée hors
    échantillon : chaque fenêtre de la série est recalculée sans ses propres scores.

    `adaptive` (optionnel) : {niveau demandé -> level_used ACI (contrat §1)}. Quand présent pour un
    niveau, le quantile est pris à level_used au lieu du niveau demandé, sur les MÊMES scores (même
    mise en commun) ; si le quantile n'existe pas à level_used (trop peu de scores), repli sur le
    facteur du niveau demandé multiplié par le rapport des quantiles gaussiens z(level_used)/z(niveau)
    (approximation documentée, contrat §1)."""
    out = {}
    for lv in levels:
        per_window = own[lv]
        others = (pool or {}).get(lv, np.array([]))
        mine = np.concatenate(per_window) if per_window else np.array([])
        lv_q = (adaptive or {}).get(lv, lv)

        def factor(s_own, lv_q=lv_q):
            f = conformal_factor(s_own, lv_q)
            if f is not None:
                return f
            f = conformal_factor(np.concatenate([s_own, others]), lv_q)
            if f is not None:
                return f
            if lv_q == lv:
                return None
            base = conformal_factor(s_own, lv)
            if base is None:
                base = conformal_factor(np.concatenate([s_own, others]), lv)
            return None if base is None else base * _z(lv_q) / _z(lv)

        held_out = []
        for w, s in enumerate(per_window):
            rest = [x for i, x in enumerate(per_window) if i != w]
            f = factor(np.concatenate(rest) if rest else np.array([]))
            if f is not None:
                held_out.append(s <= f)
        out[lv] = {
            "factor": factor(mine),
            "pooled": conformal_factor(mine, lv_q) is None and len(others) > 0,
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
