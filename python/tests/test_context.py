from datetime import date, timedelta

import numpy as np
import pytest

from th2fc.context import coverage
from th2fc.forecast import run_forecast

from conftest import LIMITS, monthly
from test_forecast import body

UPLIFT = 40.0


def promo_days(n_hist=200, h=14):
    """Série journalière : 100 + saisonnalité hebdo + 40 les jours de promo (tous les 9 jours)."""
    rng = np.random.default_rng(3)
    start = date(2025, 1, 1)
    days = [start + timedelta(days=k) for k in range(n_hist + h)]
    promo = [k % 9 == 4 for k in range(n_hist + h)]
    y = [100 + 10 * np.sin(2 * np.pi * k / 7) + UPLIFT * p + rng.normal(0, 2) for k, p in enumerate(promo)]
    rows = [{"date": d.isoformat(), "sales": float(v)} for d, v in zip(days[:n_hist], y[:n_hist])]
    event = {"name": "promo", "dates": [d.isoformat() for d, p in zip(days, promo) if p]}
    return rows, event, np.array(y[n_hist:]), np.array(promo[n_hist:])


def test_couverture_au_prorata_des_jours_de_la_periode():
    months = [date(2025, 11, 1), date(2025, 12, 1), date(2026, 1, 1)]
    got = coverage(months, "month", [(date(2025, 12, 1), date(2025, 12, 15))])
    assert got.tolist() == [0.0, 15 / 31, 0.0]
    days = [date(2025, 12, 14), date(2025, 12, 15), date(2025, 12, 16)]
    assert coverage(days, "day", [(date(2025, 12, 1), date(2025, 12, 15))]).tolist() == [1.0, 1.0, 0.0]


@pytest.mark.parametrize("extra, field, fragment", [
    ({"events": [{"name": "promo", "dates": ["2025-13-01"]}]}, "events", "date '2025-13-01' invalide"),
    ({"events": [{"name": "promo", "ranges": [{"start": "2025-12-10", "end": "2025-12-01"}]}]}, "events", "à l'envers"),
    ({"events": [{"dates": ["2025-12-01"]}]}, "events", "un nom ('name') est requis"),
    ({"events": [{"name": "promo", "dates": ["2025-12-01"]}, {"name": "promo", "dates": ["2025-12-02"]}]},
     "events", "déclaré deux fois"),
    ({"scenarios": [{"name": "x", "adjustments": [{"start": "2026-01-01", "percent": 10, "add": 5}]}]},
     "scenarios", "soit 'percent'"),
    ({"scenarios": [{"name": "x", "adjustments": [{"start": "2026-01-01", "percent": -100}]}]},
     "scenarios", "supérieur à -100"),
])
def test_contexte_invalide_refuse_avec_message_actionnable(engine, extra, field, fragment):
    status, out = run_forecast(body(monthly(24, lambda i: 50 + i), horizon=3, models=["naive"], **extra), LIMITS, engine)
    assert status == 400
    assert out["errors"][0]["field"] == field
    assert fragment in out["errors"][0]["message"]


@pytest.mark.parametrize("model", ["arima", "auto"])
def test_effet_appris_sur_les_jours_evenement(engine, model):
    rows, event, truth, promo = promo_days()
    _, sans = run_forecast(body(rows, horizon=14, models=[model]), LIMITS, engine)
    _, avec = run_forecast(body(rows, horizon=14, models=[model], events=[event]), LIMITS, engine)
    err = {k: np.abs(np.array([f["value"] for f in o["series"][0]["forecast"]]) - truth)[promo].mean()
           for k, o in (("sans", sans), ("avec", avec))}
    assert err["avec"] < 8 < err["sans"]
    assert avec["series"][0]["events"] == [{"name": "promo", "history_share": pytest.approx(22 / 200, abs=0.01), "used": True}]


def test_scenario_sans_promo_et_ajustement_explicite(engine):
    rows, event, _, promo = promo_days()
    future = [f for f in event["dates"] if f >= "2025-07-20"]
    scenarios = [
        {"name": "Sans promo", "events": []},
        {"name": "+10 % la 2e semaine", "adjustments": [{"start": "2025-07-27", "end": "2025-08-02", "percent": 10}]},
    ]
    _, out = run_forecast(body(rows, horizon=14, models=["arima"], events=[event], scenarios=scenarios), LIMITS, engine)
    s = out["series"][0]
    base = np.array([f["value"] for f in s["forecast"]])
    sans = np.array([f["value"] for f in s["scenarios"][0]["forecast"]])
    assert future and promo.any()
    assert (base - sans)[promo].mean() > 25  # l'effet appris disparaît les jours de promo
    assert s["scenarios"][0]["difference"]["total"] < 0
    plus = np.array([f["value"] for f in s["scenarios"][1]["forecast"]])
    week2 = np.array([d["date"] >= "2025-07-27" for d in s["forecast"]])
    assert np.allclose(plus[week2], base[week2] * 1.1, rtol=1e-4)
    assert np.allclose(plus[~week2], base[~week2])
    for f in s["scenarios"][1]["forecast"]:
        assert f["lower_95"] <= f["lower_80"] <= f["value"] <= f["upper_80"] <= f["upper_95"]


def test_evenement_sans_precedent_signale_et_ignore(engine):
    rows = monthly(24, lambda i: 50 + i)
    event = {"name": "ouverture", "ranges": [{"start": "2024-02-01", "end": "2024-02-29"}]}  # futur seulement
    _, out = run_forecast(body(rows, horizon=3, models=["arima"], events=[event],
                               scenarios=[{"name": "Inconnu", "events": [{"name": "grève", "dates": ["2024-02-10"]}]}]),
                          LIMITS, engine)
    s = out["series"][0]
    assert s["events"] == [{"name": "ouverture", "history_share": 0.0, "used": False}]
    assert any("'ouverture' n'a aucun précédent" in w for w in s["warnings"])
    assert any("'grève' n'existe pas" in w for w in out["warnings"])
    assert s["scenarios"][0]["difference"]["total"] == 0


def test_sans_contexte_la_reponse_ne_change_pas(engine):
    _, out = run_forecast(body(monthly(24, lambda i: 50 + i), horizon=3, models=["naive"]), LIMITS, engine)
    assert "events" not in out["series"][0] and "scenarios" not in out["series"][0]
