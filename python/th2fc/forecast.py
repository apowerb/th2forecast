"""Point d'entrée de /v1/forecast : fonction pure (corps JSON -> code HTTP, corps de réponse)."""
from __future__ import annotations

import logging
import time

import numpy as np

from . import calibration as cal
from . import context as ctx
from . import contract as c
from .engine import Engine, Task, components, metrics, windows

log = logging.getLogger("th2fc")
NA_METRICS = {"mape": None, "smape": None, "mase": None, "rmse": None}


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


def _failed_series(group, warnings: list[str]) -> dict:
    return {
        "group": group, "model": None, "history": [], "forecast": [],
        "metrics": {**NA_METRICS, "holdout_points": 0},
        "baseline": {"model": None, "metrics": dict(NA_METRICS)},
        "beats_baseline": False, "reliability": "unknown", "calibration": None, "warnings": warnings,
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
    needed = set().union(*(components(m, has_events) for m in cands)) | {baseline}
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
        for j, name in enumerate(names):
            if np.ptp(x[:n, j]) > 0:
                keep.append(j)
            else:
                s_warn.append("L'événement '%s' n'a aucun précédent dans l'historique%s : son effet ne peut pas être appris ; "
                              "pour le simuler, utilisez un ajustement explicite dans un scénario."
                              % (name, "" if g is None else " de la série '%s'" % g))
        series.append({"group": g, "dates": dates, "y": values, "warnings": s_warn, "future": fut,
                       "x_names": tuple(names[j] for j in keep), "x": x[:, keep],
                       "event_info": [{"name": name, "history_share": round(float(np.mean(x[:n, j] > 0)), 4),
                                       "used": j in keep} for j, name in enumerate(names)]})

    # 1) Backtest en origines glissantes, toutes séries et fenêtres dans un même lot.
    cv = []
    for i, s in enumerate(series):
        h_cv, origins = windows(len(s["y"]), req.horizon)
        s["h_cv"] = h_cv
        for w, o in enumerate(origins):
            cv.append(Task(key=(i, w), y=s["y"][:o], dates=s["dates"][:o], h=h_cv,
                           season=_season(o, season), models=needed, events=has_events,
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
                        models=components(s["winner"], has_events), events=has_events,
                        x_names=s["x_names"], x_hist=s["x"][:n], x_fut=x_fut)

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

    out = []
    for i, s in enumerate(series):
        group = s["group"]
        t = by_series.get(i)
        if t is None or s["winner"] not in t.out:
            out.append(_failed_series(group, s["warnings"] + ["Aucun modèle n'a pu être entraîné sur cette série."]))
            continue

        is_int = bool(np.all(s["y"] == np.round(s["y"])))

        def fmt(x):
            return int(round(float(x))) if is_int else round(float(x), 6)

        def rows(point, bounds):
            out_rows = []
            for k in range(req.horizon):
                row = {"date": s["future"][k].isoformat(), "value": fmt(point[k])}
                for lv in levels:
                    tag = _tag(lv)
                    row["lower_" + tag], row["upper_" + tag] = fmt(bounds[lv][0][k]), fmt(bounds[lv][1][k])
                out_rows.append(row)
            return out_rows

        point, bounds = t.out[s["winner"]]
        bounds = _nested(point, cal.apply(point, bounds, s["calibration"]), levels)
        forecast = rows(point, bounds)

        scenarios = []
        for j, scn in enumerate(req.scenarios):
            st = by_scenario.get((i, j))
            if st is not None and s["winner"] in st.out:
                p_s, b_s = st.out[s["winner"]]
                b_s = cal.apply(p_s, b_s, s["calibration"])
            else:
                p_s, b_s = point, bounds
            p_s, b_s = ctx.adjust(p_s, b_s, scn.adjustments, s["future"], frequency)
            b_s = _nested(p_s, b_s, levels)
            total, base_total = float(np.sum(p_s)), float(np.sum(point))
            scenarios.append({"name": scn.name, "forecast": rows(p_s, b_s),
                              "difference": {"total": fmt(total - base_total),
                                             "percent": round((total / base_total - 1) * 100, 2) if base_total else None}})

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
            "warnings": s["warnings"],
        }
        if req.events:
            entry["events"] = s["event_info"]
        if req.scenarios:
            entry["scenarios"] = scenarios
        if req.has_group:
            entry = {"group": group, **entry}
        out.append(entry)

    return 200, {
        "status": "success",
        "api_version": "1",
        "frequency": frequency,
        "duration_ms": round((time.perf_counter() - t0) * 1000),
        "warnings": warnings,
        "series": out,
    }
