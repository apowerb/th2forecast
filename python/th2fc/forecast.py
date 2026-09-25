"""Point d'entrée de /v1/forecast : fonction pure (corps JSON -> code HTTP, corps de réponse)."""
from __future__ import annotations

import logging
import time

import numpy as np

from . import calibration as cal
from . import context as ctx
from . import contract as c
from . import demand as dmd
from . import hierarchy as hie
from .engine import Engine, Task, components, metrics, windows

log = logging.getLogger("th2fc")
NA_METRICS = {"mape": None, "smape": None, "mase": None, "rmse": None}


# Un événement n'est appris que s'il distingue des périodes : l'écart entre sa part la plus forte et
# la plus faible dépasse 20 % de la plus forte. Sinon (promo « le 1er de chaque mois » sur des mois),
# la covariable est une quasi-constante colinéaire à la constante du modèle.
LEARNABLE_SPREAD = 0.2

def reliability(model_mase, baseline_mase, holdout_points) -> str:
    if model_mase is None or baseline_mase is None or holdout_points is None or holdout_points < 2:
        return "unknown"
    if model_mase >= baseline_mase:
        return "poor"
    ratio = model_mase / baseline_mase if baseline_mase > 0 else 0
    return "good" if holdout_points >= 6 and ratio <= 0.8 else "fair"


def candidates(models: list[str]) -> list[str]:
    return list(dict.fromkeys("ensemble" if m == "auto" else m for m in models))


def _season(n: int, season: int) -> int:
    """Saisonnalité utilisée si l'historique en couvre au moins une et demie."""
    return season if n >= season + max(4, season // 2) else 1


def _run_isolated(engine: Engine, tasks: list[Task], levels, frequency) -> None:
    """Lance le lot ; en cas d'échec, relance tâche par tâche pour isoler la série fautive."""
    try:
        engine.run(tasks, levels, frequency, c.step)
    except Exception:
        log.exception("échec du lot, reprise série par série")
        for t in tasks:
            t.out.clear()
            try:
                engine.run([t], levels, frequency, c.step)
            except Exception:
                log.exception("échec de la série %s", t.key[0])
                t.out.clear()


def _nested(point, bounds: dict, levels: list[float]) -> dict:
    """Bornes qui encadrent la prévision et s'emboîtent d'un niveau au suivant."""
    out, lo_prev, hi_prev = {}, point, point
    for lv in levels:
        lo = np.minimum(bounds[lv][0], lo_prev)
        hi = np.maximum(bounds[lv][1], hi_prev)
        out[lv], lo_prev, hi_prev = (lo, hi), lo, hi
    return out


def _failed_series(group, warnings: list[str], demand: dict) -> dict:
    return {
        "group": group, "model": None, "history": [], "forecast": [],
        "metrics": {**NA_METRICS, "holdout_points": 0},
        "baseline": {"model": None, "metrics": dict(NA_METRICS)},
        "beats_baseline": False, "reliability": "unknown", "calibration": None, "demand": demand,
        "warnings": warnings,
    }


def _tag(level: float) -> str:
    return "%02d" % round(level * 100)


def _calibration_body(calibration: dict, points: int) -> dict:
    def r(x):
        return None if x is None else round(x, 4)

    return {
        "method": "split-conformal",
        "points": points,
        "levels": {_tag(lv): {"calibrated": v["factor"] is not None, "pooled": v["pooled"], "factor": r(v["factor"]),
                              "raw_coverage": r(v["raw_coverage"]), "calibrated_coverage": r(v["calibrated_coverage"])}
                   for lv, v in calibration.items()},
    }


# -- Hiérarchie : construction des séries agrégées et réconciliation MinT -------------------------

def _fixed_event_matrix(events, group, periods, frequency) -> np.ndarray:
    """Comme `context.matrix`, mais sans exclure les événements non applicables à `group` (colonnes à
    zéro plutôt que retirées) : nécessaire pour moyenner terme à terme avec les autres séries filles
    lors de la propagation vers un agrégat (`hierarchy.propagate_mean`)."""
    cols = []
    for e in events:
        applicable = e.groups is None or (group is not None and str(group) in e.groups)
        cols.append(ctx.coverage(periods, frequency, e.ranges) if applicable else np.zeros(len(periods)))
    return np.column_stack(cols) if cols else np.zeros((len(periods), 0))


def _events_for_series(names: list[str], x: np.ndarray, n: int, where: str, warn: list) -> tuple[tuple, np.ndarray, list]:
    """Filtre les événements appris (écart suffisant sur l'historique) ; identique à la règle
    utilisée pour les séries du bas, factorisée pour être réutilisée par les agrégats."""
    keep = []
    for j, name in enumerate(names):
        hist = x[:n, j]
        if np.ptp(hist) > LEARNABLE_SPREAD * np.max(hist):
            keep.append(j)
        elif np.max(hist) == 0:
            warn.append("L'événement '%s' n'a aucun précédent dans l'historique%s : son effet ne peut pas être appris ; "
                        "pour le simuler, utilisez un ajustement explicite dans un scénario." % (name, where))
        else:
            warn.append("L'événement '%s' touche presque également chaque période de l'historique%s : son effet se "
                        "confond avec le niveau de la série et n'est pas appris ; déclarez-le à une fréquence plus fine "
                        "ou utilisez un ajustement explicite." % (name, where))
    event_info = [{"name": name, "history_share": round(float(np.mean(x[:n, j] > 0)), 4), "used": j in keep}
                  for j, name in enumerate(names)]
    return tuple(names[j] for j in keep), x[:, keep], event_info


def _check_alignment(bottom_series: list) -> None:
    """400 explicite (nomme les séries fautives) si les séries du bas ne couvrent pas les mêmes
    dates après régularisation : la sommation des agrégats l'exige (contrat §2)."""
    if not bottom_series:
        return
    ref = bottom_series[0]["dates"]
    bad = [str(s["group"]) for s in bottom_series[1:] if s["dates"] != ref]
    if bad:
        raise c.Invalid(400, [c.error("hierarchy", "Séries du bas non alignées après régularisation (dates "
                                      "différentes de celles de '%s') : %s." % (bottom_series[0]["group"], ", ".join(bad)))])


def _build_hierarchy_series(hier: hie.Hierarchy, bottom_series: list, events, frequency) -> list:
    """Séries agrégées (total + niveaux intermédiaires), même structure que les séries du bas, pour
    qu'elles traversent ensuite le pipeline de backtest/prévision existant sans traitement spécial."""
    y_bottom = np.array([s["y"] for s in bottom_series])
    y_agg = hie.aggregate(hier.S[:hier.bottom_start], y_bottom)

    agg_full_x, event_names = None, [e.name for e in events]
    if events:
        bottom_full_x = [_fixed_event_matrix(events, s["group"], s["dates"] + s["future"], frequency) for s in bottom_series]
        agg_full_x = hie.propagate_mean(hier, bottom_full_x)

    dates, future = bottom_series[0]["dates"], bottom_series[0]["future"]
    n = len(dates)
    out = []
    for i in range(hier.bottom_start):
        node = hier.nodes[i]
        warn = []
        where = " de l'agrégat '%s' (niveau %s)" % (node.label, node.level)
        if events:
            x_names, x, event_info = _events_for_series(event_names, agg_full_x[i], n, where, warn)
        else:
            x_names, x, event_info = (), np.zeros((n + len(future), 0)), []
        demand_info = dmd.classify(y_agg[i])
        out.append({"group": node.label, "level": node.level, "dates": dates, "y": y_agg[i], "warnings": warn,
                    "future": future, "x_names": x_names, "x": x, "event_info": event_info,
                    "demand": demand_info, "sparse": dmd.is_sparse(demand_info["type"])})
    return out


def _final_W(series: list, cv: list) -> np.ndarray | None:
    """Résidus empilés (T, n_nœuds) de TOUTES les fenêtres de backtest, modèle retenu de chaque nœud
    (une colonne par nœud) ; None si une série a échoué ou si son modèle manque une fenêtre."""
    by_key = {t.key: t for t in cv}
    k = len(series[0]["origins"]) if series else 0
    rows = []
    for w in range(k):
        h = series[0]["h_cv"]
        for step in range(h):
            row = []
            for i, s in enumerate(series):
                t = by_key.get((i, w))
                if not s["winner"] or t is None or s["winner"] not in t.out:
                    return None
                row.append(s["y"][s["origins"][w] + step] - t.out[s["winner"]][0][step])
            rows.append(row)
    return np.array(rows) if rows else None


def _final_points(series: list, by_series: dict) -> np.ndarray | None:
    rows = []
    for i, s in enumerate(series):
        t = by_series.get(i)
        if t is None or not s["winner"] or s["winner"] not in t.out:
            return None
        rows.append(t.out[s["winner"]][0])
    return np.array(rows)


def _reconcile_matrix(hier: hie.Hierarchy, Y: np.ndarray, W: np.ndarray | None, requested: str) -> tuple[np.ndarray, str, str | None]:
    """Réconcilie la matrice (n_nœuds, horizon) des prévisions brutes. Renvoie (Y_rec, méthode
    effective, avertissement de repli éventuel)."""
    if requested == "bottom_up":
        return hie.bottom_up(Y[hier.bottom_start:], hier.S), "bottom_up", None
    if W is None:
        return hie.bottom_up(Y[hier.bottom_start:], hier.S), "bottom_up", (
            "Repli sur bottom_up : covariance des erreurs de backtest non estimable (trop peu de points).")
    if hie.is_singular(W):
        return hie.bottom_up(Y[hier.bottom_start:], hier.S), "bottom_up", (
            "Repli sur bottom_up : la matrice de covariance des erreurs de backtest est singulière.")
    return hie.reconcile(Y, hier.S, W), "mint_shrink", None


def _bounds_shift(bounds_raw: dict, delta: np.ndarray, clip_nonneg: bool) -> dict:
    """Bandes décalées du même delta que le point (approximation documentée, contrat §2)."""
    out = {}
    for lv, (lo, hi) in bounds_raw.items():
        lo2, hi2 = lo + delta, hi + delta
        if clip_nonneg:
            lo2, hi2 = np.maximum(lo2, 0.0), np.maximum(hi2, 0.0)
        out[lv] = (lo2, hi2)
    return out


def _bounds_sum(bounds_list: list, levels: list) -> dict:
    return {lv: (sum(b[lv][0] for b in bounds_list), sum(b[lv][1] for b in bounds_list)) for lv in levels}


def _hierarchy_backtest(hier: hie.Hierarchy, series: list, cv: list, method: str) -> dict | None:
    """MASE moyen avant/après réconciliation, en excluant à chaque fenêtre les résidus de CETTE
    fenêtre de l'estimation de W (leave-one-window-out), comme l'exige le contrat §2 ('preuve')."""
    by_key = {t.key: t for t in cv}
    k = len(series[0]["origins"]) if series else 0
    h = series[0]["h_cv"] if series else 0
    if k < 2 or h < 1:
        return None
    n = len(series)

    # résidus de chaque fenêtre, par nœud : (k, h) par nœud, pour reconstituer W sans une fenêtre.
    resid = np.full((k, h, n), np.nan)
    actual = np.full((k, h, n), np.nan)
    pred = np.full((k, h, n), np.nan)
    scale = np.full((k, n), np.nan)
    for i, s in enumerate(series):
        if not s["winner"]:
            return None
        for w in range(k):
            t = by_key.get((i, w))
            if t is None or s["winner"] not in t.out:
                return None
            o = s["origins"][w]
            a = s["y"][o:o + h]
            p = t.out[s["winner"]][0]
            actual[w, :, i], pred[w, :, i] = a, p
            resid[w, :, i] = a - p
            train = s["y"][:o]
            sc = np.mean(np.abs(np.diff(train))) if len(train) > 1 else 0.0
            scale[w, i] = sc if sc > 0 else np.nan

    err_base, err_rec = [], []
    for w_star in range(k):
        others = [w for w in range(k) if w != w_star]
        E = resid[others].reshape(-1, n)
        Y = pred[w_star].T  # (n, h)
        if method == "bottom_up":
            rec = hie.bottom_up(Y[hier.bottom_start:], hier.S)
        else:
            W, _ = hie.shrink_covariance(E)
            rec = (hie.bottom_up(Y[hier.bottom_start:], hier.S) if W is None or hie.is_singular(W)
                  else hie.reconcile(Y, hier.S, W))
        for i in range(n):
            sc = scale[w_star, i]
            if not np.isfinite(sc):
                continue
            err_base.append(np.abs(actual[w_star, :, i] - Y[i]) / sc)
            err_rec.append(np.abs(actual[w_star, :, i] - rec[i]) / sc)

    if not err_base:
        return None
    base, rec = np.concatenate(err_base), np.concatenate(err_rec)
    return {"mase_base": round(float(np.mean(base)), 4), "mase_reconciled": round(float(np.mean(rec)), 4),
           "points": int(base.size)}


def run_forecast(body, limits: dict, engine: Engine) -> tuple[int, dict]:
    t0 = time.perf_counter()
    try:
        req = c.validate(body, limits)
    except c.Invalid as e:
        return e.status, c.error_body(e.errors)

    warnings = list(req.context_warnings)
    frequency = req.frequency
    if frequency is None:
        frequency = c.detect_frequency(req.df["date"])
        warnings.append("Fréquence détectée automatiquement : %s." % frequency)
    season = c.SEASONAL_PERIOD[frequency]
    baseline = "snaive" if season > 1 else "naive"
    cands = candidates(req.models)
    has_events = bool(req.events)
    # Composants par type de demande : lensemble auto dune série intermittente/lumpy diffère.
    needed = {sp: set().union(*(components(m, has_events, sparse=sp) for m in cands)) | {baseline}
              for sp in (False, True)}
    levels = req.confidence_levels

    series = []
    for g in dict.fromkeys(req.df["group"]):
        sub = req.df[c._mask(req.df, g)].sort_values("date")
        dates, values, n_padded = c.regularize(list(sub["date"]), list(sub["value"]), frequency)
        s_warn = []
        if n_padded > 0:
            s_warn.append("%d point(s) manquant(s) au pas '%s' comblé(s) par interpolation linéaire." % (n_padded, frequency))
        fut = [c.step(dates[-1], frequency, k) for k in range(1, req.horizon + 1)]
        names, x = ctx.matrix(req.events, g, dates + fut, frequency)
        n = len(dates)
        keep = []
        where = "" if g is None else " de la série '%s'" % g
        for j, name in enumerate(names):
            hist = x[:n, j]
            if np.ptp(hist) > LEARNABLE_SPREAD * np.max(hist):
                keep.append(j)
            elif np.max(hist) == 0:
                s_warn.append("L'événement '%s' n'a aucun précédent dans l'historique%s : son effet ne peut pas être appris ; "
                              "pour le simuler, utilisez un ajustement explicite dans un scénario." % (name, where))
            else:
                s_warn.append("L'événement '%s' touche presque également chaque période de l'historique%s : son effet se "
                              "confond avec le niveau de la série et n'est pas appris ; déclarez-le à une fréquence plus fine "
                              "ou utilisez un ajustement explicite." % (name, where))
        demand_info = dmd.classify(values)
        series.append({"group": g, "level": "bottom", "dates": dates, "y": values, "warnings": s_warn, "future": fut,
                       "x_names": tuple(names[j] for j in keep), "x": x[:, keep],
                       "demand": demand_info, "sparse": dmd.is_sparse(demand_info["type"]),
                       "event_info": [{"name": name, "history_share": round(float(np.mean(x[:n, j] > 0)), 4),
                                       "used": j in keep} for j, name in enumerate(names)]})

    hier = req.hierarchy
    if hier:
        try:
            _check_alignment(series)
        except c.Invalid as e:
            return e.status, c.error_body(e.errors)
        series = _build_hierarchy_series(hier, series, req.events, frequency) + series

    # 1) Backtest en origines glissantes, toutes séries et fenêtres dans un même lot.
    cv = []
    for i, s in enumerate(series):
        h_cv, origins = windows(len(s["y"]), req.horizon)
        s["h_cv"], s["origins"] = h_cv, origins
        for w, o in enumerate(origins):
            cv.append(Task(key=(i, w), y=s["y"][:o], dates=s["dates"][:o], h=h_cv,
                           season=_season(o, season), models=needed[s["sparse"]], events=has_events, sparse=s["sparse"],
                           x_names=s["x_names"], x_hist=s["x"][:o], x_fut=s["x"][o:o + h_cv]))
    _run_isolated(engine, cv, levels, frequency)

    for i, s in enumerate(series):
        runs = [t for t in cv if t.key[0] == i]
        tests = [(s["y"][len(t.y):len(t.y) + t.h], t) for t in runs]

        def pooled(model):
            if not all(model in t.out for t in runs):
                return None
            return metrics([(y, t.out[model][0], t.y) for y, t in tests])

        scores = {m: pooled(m) for m in cands}
        ok = [m for m in cands if scores[m] is not None]
        s["winner"] = min(ok, key=lambda m: scores[m]["rmse"]) if ok else None
        s["metrics"] = scores.get(s["winner"])
        s["baseline_metrics"] = pooled(baseline)
        s["holdout_points"] = sum(len(y) for y, _ in tests)
        s["scores"] = ({lv: cal.window_scores([(y, *t.out[s["winner"]]) for y, t in tests], lv) for lv in levels}
                       if s["winner"] else None)

    # Calibration : scores de la série, complétés par ceux des autres séries s'ils manquent.
    for i, s in enumerate(series):
        if s["scores"] is None:
            s["calibration"] = None
            continue
        pool = {lv: np.concatenate([x for j, o in enumerate(series) if j != i and o["scores"] for x in o["scores"][lv]]
                                   or [np.array([])]) for lv in levels}
        s["calibration"] = cal.calibrate(s["scores"], levels, pool)

    # 2) Prévision finale : modèle retenu ré-entraîné sur toute la série.
    # Scénarios : mêmes modèles, événements futurs remplacés (les ajustements viennent après).
    final = []
    for i, s in enumerate(series):
        if not s["winner"]:
            continue
        n = len(s["y"])

        def task(key, x_fut):
            return Task(key=key, y=s["y"], dates=s["dates"], h=req.horizon, season=_season(n, season),
                        models=components(s["winner"], has_events, sparse=s["sparse"]), events=has_events,
                        sparse=s["sparse"], x_names=s["x_names"], x_hist=s["x"][:n], x_fut=x_fut)

        final.append(task((i, None), s["x"][n:]))
        for j, scn in enumerate(req.scenarios):
            if scn.events is not None and s["x_names"]:
                names, x = ctx.matrix(scn.events, s["group"], s["future"], frequency)
                cols = [x[:, names.index(nm)] if nm in names else np.zeros(req.horizon) for nm in s["x_names"]]
                final.append(task((i, j), np.column_stack(cols)))
        if s["x_names"] and s["winner"] in {"ets", "theta", "naive", "snaive"}:
            s["warnings"].append("Le modèle retenu (%s) n'exploite pas les événements déclarés." % s["winner"])
    _run_isolated(engine, final, levels, frequency)
    by_series = {t.key[0]: t for t in final if t.key[1] is None}
    by_scenario = {t.key: t for t in final if t.key[1] is not None}

    # 3) Réconciliation de hiérarchie (point + bornes + scénarios), si demandée.
    reco = None
    Y_rec, W = None, None
    if hier and req.reconciliation != "none":
        Y_raw = _final_points(series, by_series)
        if Y_raw is None:
            warnings.append("Réconciliation de hiérarchie impossible : au moins une série n'a pas pu être prévue.")
            reco = {"method": "none", "coherent": False}
        else:
            W = _final_W(series, cv)
            Y_rec, method, fb_warn = _reconcile_matrix(hier, Y_raw, W, req.reconciliation)
            if fb_warn:
                warnings.append(fb_warn)
            reco = {"method": method, "coherent": True}
            bt = _hierarchy_backtest(hier, series, cv, method)
            if bt:
                reco["backtest"] = bt
    elif hier:
        reco = {"method": "none", "coherent": False}

    bounds_raw_all = [by_series[k].out[series[k]["winner"]][1] for k in range(len(series))] if Y_rec is not None else None

    def scenario_final(j: int, scn) -> list:
        """Point + bornes du scénario `j`, réconciliés puis ajustés, pour TOUS les nœuds (mémoïsé).

        Ordre du contrat §2 : réconciliation avec la même matrice que le point de base, calibration,
        ajustements sur les séries du BAS uniquement, puis agrégats recalculés par somme des bas
        ajustés (pas re-réconciliés) pour rester cohérents après ajustement.
        """
        Y_scn_raw = np.array([by_scenario[(k, j)].out[series[k]["winner"]][0]
                              if (k, j) in by_scenario and series[k]["winner"] in by_scenario[(k, j)].out
                              else Y_raw[k] for k in range(len(series))])
        Y_scn_rec, _, _ = _reconcile_matrix(hier, Y_scn_raw, W, req.reconciliation)

        bottom_final = []
        for k in range(hier.bottom_start, len(series)):
            stt = by_scenario.get((k, j))
            bk_raw = (stt.out[series[k]["winner"]][1] if stt is not None and series[k]["winner"] in stt.out
                     else bounds_raw_all[k])
            dk = Y_scn_rec[k] - Y_scn_raw[k]
            pk = Y_scn_rec[k]
            bk = _bounds_shift(bk_raw, dk, bool(np.all(series[k]["y"] >= 0)))
            bk = cal.apply(pk, bk, series[k]["calibration"])
            pk, bk = ctx.adjust(pk, bk, scn.adjustments, series[k]["future"], frequency)
            bottom_final.append((pk, bk))

        final = [None] * hier.bottom_start + bottom_final
        for i in range(hier.bottom_start - 1, -1, -1):
            kids = [final[hier.bottom_start + c] for c in range(len(bottom_final)) if hier.S[i][c] > 0]
            final[i] = (sum(pk for pk, _ in kids), _bounds_sum([bk for _, bk in kids], levels))
        return final

    scenario_cache: dict = {}
    out = []
    for i, s in enumerate(series):
        group = s["group"]
        t = by_series.get(i)
        if t is None or s["winner"] not in t.out:
            entry = _failed_series(group, s["warnings"] + ["Aucun modèle n'a pu être entraîné sur cette série."],
                                   s["demand"])
            if hier:
                entry["level"] = s["level"]
            out.append(entry)
            continue

        # Ventes rares : jamais arrondi à l'entier (2 décimales), même sur un historique entier.
        is_int = not s["sparse"] and bool(np.all(s["y"] == np.round(s["y"])))

        def fmt(x):
            if s["sparse"]:
                return round(float(x), 2)
            return int(round(float(x))) if is_int else round(float(x), 6)

        def floor0(bounds):
            # Demande intermittente/lumpy : jamais de borne basse négative, même après calibration.
            if not s["sparse"]:
                return bounds
            return {lv: (np.maximum(lo, 0.0), hi) for lv, (lo, hi) in bounds.items()}

        def rows(point, bounds):
            out_rows = []
            for k in range(req.horizon):
                row = {"date": s["future"][k].isoformat(), "value": fmt(point[k])}
                for lv in levels:
                    tag = _tag(lv)
                    row["lower_" + tag], row["upper_" + tag] = fmt(bounds[lv][0][k]), fmt(bounds[lv][1][k])
                out_rows.append(row)
            return out_rows

        point_raw, bounds_raw = t.out[s["winner"]]
        clip_nonneg = bool(np.all(s["y"] >= 0))
        if Y_rec is not None:
            delta = Y_rec[i] - point_raw
            point, bounds_shifted = Y_rec[i], _bounds_shift(bounds_raw, delta, clip_nonneg)
        else:
            point, bounds_shifted = point_raw, bounds_raw
        bounds = floor0(_nested(point, cal.apply(point, bounds_shifted, s["calibration"]), levels))
        forecast = rows(point, bounds)

        scenarios = []
        for j, scn in enumerate(req.scenarios):
            if hier and Y_rec is not None:
                if j not in scenario_cache:
                    scenario_cache[j] = scenario_final(j, scn)
                p_s, b_s = scenario_cache[j][i]
            else:
                st = by_scenario.get((i, j))
                if st is not None and s["winner"] in st.out:
                    p_s, b_s = st.out[s["winner"]]
                    b_s = cal.apply(p_s, b_s, s["calibration"])
                else:
                    p_s, b_s = point, bounds
                p_s, b_s = ctx.adjust(p_s, b_s, scn.adjustments, s["future"], frequency)
            b_s = floor0(_nested(p_s, b_s, levels))
            total, base_total = float(np.sum(p_s)), float(np.sum(point))
            scenarios.append({"name": scn.name, "forecast": rows(p_s, b_s),
                              "difference": {"total": fmt(total - base_total),
                                             "percent": round((total / base_total - 1) * 100, 2) if base_total else None}})

        if s["sparse"]:
            s["warnings"].append("Ventes rares (%s) : la prévision donne la demande moyenne attendue par période."
                                 % s["demand"]["type"])
            if s["winner"] not in {"ensemble", "croston", "tsb", "imapa"}:
                s["warnings"].append("Série à ventes rares : le mode auto ou tsb/imapa sont plus adaptés.")

        m, b = s["metrics"], s["baseline_metrics"] or dict(NA_METRICS)
        model_mase, base_mase = m["mase"], b["mase"]
        entry = {
            "model": s["winner"],
            "history": [{"date": d.isoformat(), "value": fmt(v)} for d, v in zip(s["dates"], s["y"])],
            "forecast": forecast,
            "metrics": {**m, "holdout_points": s["holdout_points"]},
            "baseline": {"model": baseline, "metrics": b},
            "beats_baseline": model_mase is not None and base_mase is not None and model_mase < base_mase,
            "reliability": reliability(model_mase, base_mase, s["holdout_points"]),
            "calibration": _calibration_body(s["calibration"], s["holdout_points"]),
            "demand": s["demand"],
            "warnings": s["warnings"],
        }
        if req.events:
            entry["events"] = s["event_info"]
        if req.scenarios:
            entry["scenarios"] = scenarios
        if hier:
            entry["level"] = s["level"]
        if req.has_group:
            entry = {"group": group, **entry}
        out.append(entry)

    body_out = {
        "status": "success",
        "api_version": "1",
        "frequency": frequency,
        "duration_ms": round((time.perf_counter() - t0) * 1000),
        "warnings": warnings,
        "series": out,
    }
    if hier:
        body_out["reconciliation"] = reco
    return 200, body_out
