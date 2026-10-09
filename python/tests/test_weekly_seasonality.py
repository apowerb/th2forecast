import numpy as np
import pandas as pd
import pytest

from th2fc.forecast import run_forecast
from conftest import LIMITS

H = 26


def weekly_series(n_hist, seed=7, noise=0.06, seasonal=True):
    rng = np.random.default_rng(seed)
    t = np.arange(n_hist + H)
    trend = 500 + 1.0 * t
    season = 175 * np.sin(2 * np.pi * t / 52.18 - 1.2) if seasonal else 0 * t
    y = trend + season + rng.normal(0, noise * trend)
    return t, trend, season, y


def forecast(n_hist, model, **kw):
    _, trend, season, y = weekly_series(n_hist, **kw)
    dates = pd.date_range("2023-01-02", periods=n_hist, freq="W-MON")
    rows = [{"date": d.date().isoformat(), "sales": float(v)} for d, v in zip(dates, y[:n_hist])]
    body = {"data": rows, "date_var": "date", "target_var": "sales", "horizon": H, "models": [model], "frequency": "week"}
    status, out = run_forecast(body, LIMITS, ENGINE[0])
    assert status == 200
    return np.array([f["value"] for f in out["series"][0]["forecast"]], float), trend[n_hist:], season[n_hist:]


ENGINE = []


@pytest.fixture(autouse=True)
def _engine(engine):
    ENGINE[:] = [engine]


def skill_over_trend(values, trend, season):
    """1 - erreur / erreur d'une prévision qui connaîtrait la tendance exacte mais ignorerait la saisonnalité."""
    return 1 - np.abs(values - (trend + season)).mean() / np.abs(trend - (trend + season)).mean()


def test_ets_hebdomadaire_suit_la_saisonnalite_annuelle_sur_3_ans():
    values, trend, season = forecast(156, "ets")
    assert skill_over_trend(values, trend, season) > 0.5


def test_prophet_hebdomadaire_suit_la_saisonnalite_des_2_ans_moins_une_semaine():
    values, trend, season = forecast(104, "prophet")
    assert skill_over_trend(values, trend, season) > 0.3


@pytest.mark.parametrize("seed", [1, 2, 3, 4, 5])
def test_ets_hebdomadaire_reste_plat_sur_du_bruit_blanc(seed):
    rng = np.random.default_rng(seed)
    y = 1000 + rng.normal(0, 100, 156 + H)
    dates = pd.date_range("2023-01-02", periods=156, freq="W-MON")
    rows = [{"date": d.date().isoformat(), "sales": float(v)} for d, v in zip(dates, y[:156])]
    body = {"data": rows, "date_var": "date", "target_var": "sales", "horizon": H, "models": ["ets"], "frequency": "week"}
    _, out = run_forecast(body, LIMITS, ENGINE[0])
    values = np.array([f["value"] for f in out["series"][0]["forecast"]], float)
    assert values.std() < 0.25 * 100
