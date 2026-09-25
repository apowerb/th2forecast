import numpy as np
import pytest

from th2fc.forecast import reliability, run_forecast
from conftest import LIMITS, monthly, truth


def body(rows, **kw):
    return {"data": rows, "date_var": "date", "target_var": "sales", **kw}


def test_format_de_reponse_modele_naif(engine):
    status, out = run_forecast(body(monthly(24, lambda i: 100 + i), horizon=3, models=["naive"],
                                    confidence_levels=[0.8]), LIMITS, engine)
    assert status == 200
    assert (out["status"], out["api_version"], out["frequency"]) == ("success", "1", "month")
    assert out["warnings"] == ["Fréquence détectée automatiquement : month."]
    s = out["series"][0]
    assert "group" not in s
    assert s["model"] == "naive"
    assert [f["date"] for f in s["forecast"]] == ["2024-01-01", "2024-02-01", "2024-03-01"]
    assert set(s["forecast"][0]) == {"date", "value", "lower_80", "upper_80"}
    assert set(s["metrics"]) == {"mape", "smape", "mase", "rmse", "holdout_points"}
    assert s["baseline"]["model"] == "snaive"
    assert s["reliability"] in {"good", "fair", "poor", "unknown"}
    assert all(isinstance(f["value"], int) for f in s["forecast"])


def test_une_entree_par_groupe(engine):
    rows = monthly(14, lambda i: 50 + i, store="A") + monthly(14, lambda i: 150 + i, store="B")
    status, out = run_forecast(body(rows, group_var="store", horizon=2, models=["snaive"]), LIMITS, engine)
    assert status == 200
    assert [s["group"] for s in out["series"]] == ["A", "B"]


@pytest.mark.parametrize("model", ["arima", "ets", "auto"])
def test_prevoit_les_12_mois_qui_suivent_la_serie(engine, noisy_truth_rows, model):
    """Régression du défaut R (PR #5) : la prévision doit partir de la fin de la série."""
    status, out = run_forecast(body(noisy_truth_rows, horizon=12, models=[model]), LIMITS, engine)
    s = out["series"][0]
    assert s["forecast"][0]["date"] == "2026-01-01"
    values = np.array([f["value"] for f in s["forecast"]])
    # Un pas de décalage donne une MAE de 10 (12 pas : 24) ; la précision se mesure au banc M3.
    assert np.mean(np.abs(values - truth(np.arange(49, 61)))) < 7
    assert s["model"] == ("ensemble" if model == "auto" else model)


def test_bornes_emboitees_autour_de_la_prevision(engine, noisy_truth_rows):
    _, out = run_forecast(body(noisy_truth_rows, horizon=6, models=["auto"]), LIMITS, engine)
    for f in out["series"][0]["forecast"]:
        assert f["lower_95"] <= f["lower_80"] <= f["value"] <= f["upper_80"] <= f["upper_95"]


def test_prophet_reste_disponible(engine):
    _, out = run_forecast(body(monthly(36, lambda i: 100 + 2 * i), horizon=12, models=["prophet"]), LIMITS, engine)
    s = out["series"][0]
    assert s["model"] == "prophet"
    assert len(s["forecast"]) == 12
    assert abs(s["forecast"][0]["value"] - 174) < 10


def test_meilleur_modele_au_backtest(engine, noisy_truth_rows):
    _, out = run_forecast(body(noisy_truth_rows, horizon=12, models=["naive", "ets"]), LIMITS, engine)
    assert out["series"][0]["model"] == "ets"


def test_metriques_en_fractions_et_fenetres_glissantes(engine, noisy_truth_rows):
    _, out = run_forecast(body(noisy_truth_rows, horizon=12, models=["ets"]), LIMITS, engine)
    m = out["series"][0]["metrics"]
    assert 0 < m["mape"] < 0.2 and 0 < m["smape"] < 0.2
    # 48 points : fenêtres de 9 ; au moins 24 points d'entraînement -> deux origines glissantes.
    assert m["holdout_points"] == 18


def test_valeurs_decimales_arrondies_a_6_chiffres(engine):
    _, out = run_forecast(body(monthly(24, lambda i: 10.123456789 + i), horizon=2, models=["naive"]), LIMITS, engine)
    assert out["series"][0]["history"][0]["value"] == 11.123457


def test_trou_comble_et_signale(engine):
    rows = [r for i, r in enumerate(monthly(24, float)) if i != 5]
    _, out = run_forecast(body(rows, horizon=2, models=["naive"], frequency="month"), LIMITS, engine)
    assert out["warnings"] == []
    assert out["series"][0]["warnings"] == ["1 point(s) manquant(s) au pas 'month' comblé(s) par interpolation linéaire."]


@pytest.mark.parametrize("args, expected", [
    ((None, 1.0, 10), "unknown"), ((1.0, 1.0, 10), "poor"), ((0.5, 1.0, 10), "good"),
    ((0.5, 1.0, 4), "fair"), ((0.9, 1.0, 10), "fair"), ((0.5, 1.0, 1), "unknown"),
])
def test_regle_de_fiabilite(args, expected):
    assert reliability(*args) == expected
