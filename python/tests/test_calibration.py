import numpy as np

from th2fc.calibration import apply, calibrate, conformal_factor, scores, window_scores

from conftest import LIMITS, monthly
from test_forecast import body


def test_score_vaut_1_sur_la_borne_et_0_sur_la_prevision():
    p, lo, hi = np.array([10.0, 10.0, 10.0]), np.array([8.0, 8.0, 8.0]), np.array([14.0, 14.0, 14.0])
    assert scores(np.array([8.0, 10.0, 17.0]), p, lo, hi).tolist() == [1.0, 0.0, 1.75]


def test_facteur_conforme_ordre_et_points_insuffisants():
    s = np.arange(1, 10, dtype=float)  # n = 9
    assert conformal_factor(s, 0.8) == 8.0  # ceil(10 * 0.8) = 8e plus petit
    assert conformal_factor(s, 0.95) is None  # ceil(10 * 0.95) = 10 > 9
    assert conformal_factor(np.array([]), 0.8) is None


def test_bandes_trop_etroites_elargies_jusqua_la_couverture_visee():
    rng = np.random.default_rng(0)
    windows = []
    for _ in range(3):
        y = rng.normal(0, 3, 40)  # erreurs d'écart-type 3...
        p = np.zeros(40)
        windows.append((y, p, {0.8: (p - 1.0, p + 1.0)}))  # ...pour une bande de ±1
    cal = calibrate({0.8: window_scores(windows, 0.8)}, [0.8])[0.8]
    assert cal["raw_coverage"] < 0.4
    assert 3.0 < cal["factor"] < 5.5  # 1,28 écart-type ≈ 3,8
    assert 0.7 <= cal["calibrated_coverage"] <= 0.9


def test_facteur_absent_bandes_inchangees():
    p = np.array([5.0])
    b = {0.95: (np.array([4.0]), np.array([7.0]))}
    assert apply(p, b, {0.95: {"factor": None}})[0.95][0].tolist() == [4.0]
    lo, hi = apply(p, b, {0.95: {"factor": 2.0}})[0.95]
    assert (lo.tolist(), hi.tolist()) == ([3.0], [9.0])


def test_reponse_porte_la_calibration(engine, noisy_truth_rows):
    from th2fc.forecast import run_forecast

    _, out = run_forecast(body(noisy_truth_rows, horizon=6, models=["ets"]), LIMITS, engine)
    cal = out["series"][0]["calibration"]
    assert cal["method"] == "split-conformal"
    assert cal["points"] == out["series"][0]["metrics"]["holdout_points"]
    assert set(cal["levels"]) == {"80", "95"}
    lv80 = cal["levels"]["80"]
    assert lv80["calibrated"] is True and lv80["factor"] > 0
    assert 0 <= lv80["raw_coverage"] <= 1


def test_serie_courte_sans_calibration_a_95(engine):
    from th2fc.forecast import run_forecast

    _, out = run_forecast(body(monthly(14, lambda i: 50 + i), horizon=2, models=["naive"]), LIMITS, engine)
    lv95 = out["series"][0]["calibration"]["levels"]["95"]
    assert lv95 == {"calibrated": False, "pooled": False, "factor": None,
                    "raw_coverage": lv95["raw_coverage"], "calibrated_coverage": None}


def test_series_courtes_calibrees_ensemble_a_95(engine):
    from th2fc.forecast import run_forecast

    rows = [r for k in range(8) for r in monthly(14, lambda i, k=k: 50 + k * i + (i % 3), store=str(k))]
    _, out = run_forecast(body(rows, group_var="store", horizon=2, models=["naive"]), LIMITS, engine)
    for s in out["series"]:
        lv95 = s["calibration"]["levels"]["95"]
        assert lv95["calibrated"] is True and lv95["pooled"] is True


def test_scores_des_autres_series_seulement_si_la_serie_nen_a_pas_assez():
    own = {0.8: [np.array([0.5, 0.6, 0.7, 0.8, 0.9])]}  # n = 5 : assez pour 80 %
    pool = {0.8: np.full(100, 10.0)}
    out = calibrate(own, [0.8], pool)[0.8]
    assert out["factor"] == 0.9 and out["pooled"] is False
