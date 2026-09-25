import numpy as np
import pytest

from th2fc import contract as c
from th2fc.demand import classify
from th2fc.engine import Task, components
from th2fc.forecast import run_forecast
from conftest import LIMITS, monthly


def body(rows, **kw):
    return {"data": rows, "date_var": "date", "target_var": "sales", **kw}


def sparse_series(n=30, p=0.35, mean=8, seed=3):
    """Poisson(mean) x Bernoulli(p) : ADI ~ 1/p (>= 1,32), CV2 faible (Poisson peu dispersé) -> intermittent."""
    rng = np.random.default_rng(seed)
    return np.array([float(rng.poisson(mean)) if rng.random() < p else 0.0 for _ in range(n)])


def lumpy_series(n=30, p=0.3, seed=5):
    """Bernoulli(p) x tailles très dispersées (loi log-normale) -> ADI >= 1,32 et CV2 >= 0,49."""
    rng = np.random.default_rng(seed)
    return np.array([float(rng.lognormal(mean=2, sigma=1.5)) if rng.random() < p else 0.0 for _ in range(n)])


def sparse_rows(n=30, **kw):
    y = sparse_series(n=n, **kw)
    return monthly(n, lambda t: y[t - 1])


# -- classify() : les 4 quadrants Syntetos-Boylan -----------------------------------------------

def test_classify_smooth():
    y = np.array([10, 11, 9, 10, 11, 9, 10, 11, 9, 10, 11, 9, 10, 11, 9, 10, 11, 9, 10, 11], dtype=float)
    r = classify(y)
    assert r["type"] == "smooth"
    assert r["adi"] < 1.32 and r["cv2"] < 0.49


def test_classify_erratic():
    # Ventes à chaque période (ADI = 1) mais taille très dispersée (alternance 1 / 200).
    y = np.array([1.0, 200.0] * 12)
    r = classify(y)
    assert r["type"] == "erratic"
    assert r["adi"] < 1.32
    assert r["cv2"] >= 0.49


def test_classify_intermittent():
    y = sparse_series(n=40, p=0.35, mean=8, seed=1)
    r = classify(y)
    assert r["type"] == "intermittent"
    assert r["adi"] >= 1.32 and r["cv2"] < 0.49


def test_classify_lumpy():
    y = lumpy_series(n=40, p=0.3, seed=5)
    r = classify(y)
    assert r["type"] == "lumpy"
    assert r["adi"] >= 1.32 and r["cv2"] >= 0.49


# -- Cas limites documentés dans le contrat -----------------------------------------------------

def test_classify_valeur_negative_reste_smooth():
    y = sparse_series(n=40, p=0.35, mean=8, seed=1).copy()
    assert classify(y)["type"] == "intermittent"  # sans la négative, la série bascule
    y[0] = -5.0
    r = classify(y)
    assert r["type"] == "smooth"


def test_classify_moins_de_deux_non_nulles_reste_smooth():
    y = np.zeros(20)
    y[5] = 3.0
    r = classify(y)
    assert r["type"] == "smooth"
    assert r["adi"] is not None  # une seule valeur non nulle : ADI calculable, CV2 non.
    assert r["cv2"] is None


def test_classify_tout_nul():
    y = np.zeros(15)
    r = classify(y)
    assert r == {"type": "smooth", "adi": None, "cv2": None, "zero_share": 1.0}


# -- Champ demand toujours présent dans la réponse ------------------------------------------------

def test_demand_toujours_present(engine):
    _, out = run_forecast(body(monthly(24, lambda i: 100 + i), horizon=3, models=["naive"]), LIMITS, engine)
    d = out["series"][0]["demand"]
    assert set(d) == {"type", "adi", "cv2", "zero_share"}
    assert d["type"] == "smooth"


def test_demand_sur_serie_intermittente(engine):
    _, out = run_forecast(body(sparse_rows(n=36, seed=1), horizon=6, models=["naive"]), LIMITS, engine)
    d = out["series"][0]["demand"]
    assert d["type"] == "intermittent"
    assert d["adi"] >= 1.32


# -- auto sur série intermittente : ensemble chronos2 + tsb + imapa (appels réels au moteur) -----

def test_auto_intermittent_utilise_chronos2_tsb_imapa():
    from th2fc.engine import Engine

    y = sparse_series(n=30, p=0.35, mean=8, seed=3)
    task = Task(key=(0, None), y=y, dates=list(range(len(y))), h=6, season=1,
                models=components("ensemble", events=False, sparse=True), events=False, sparse=True)
    Engine().run([task], [0.8], "month", c.step)

    assert {"chronos2", "tsb", "imapa", "ensemble"} <= set(task.out)
    assert not {"ets", "arima", "theta"} & set(task.out)
    expected = np.mean([task.out[m][0] for m in ("chronos2", "tsb", "imapa")], axis=0)
    np.testing.assert_allclose(task.out["ensemble"][0], expected)


def test_auto_lisse_utilise_toujours_lancien_ensemble():
    from th2fc.engine import Engine

    y = np.array([100.0 + i + (2 if i % 2 else -2) for i in range(30)])
    task = Task(key=(0, None), y=y, dates=list(range(len(y))), h=6, season=1,
                models=components("ensemble", events=False, sparse=False), events=False, sparse=False)
    Engine().run([task], [0.8], "month", c.step)

    assert {"chronos2", "ets", "arima", "theta", "ensemble"} <= set(task.out)
    assert not {"tsb", "imapa"} & set(task.out)


# -- modèles croston/tsb/imapa acceptés en HTTP ----------------------------------------------------

@pytest.mark.parametrize("model", ["croston", "tsb", "imapa"])
def test_modeles_intermittents_acceptes(engine, model):
    status, out = run_forecast(body(sparse_rows(n=30, seed=2), horizon=4, models=[model]), LIMITS, engine)
    assert status == 200
    s = out["series"][0]
    assert s["model"] == model
    assert len(s["forecast"]) == 4
    assert all(f["value"] >= 0 for f in s["forecast"])


def test_modele_inconnu_toujours_refuse(engine):
    status, out = run_forecast(body(monthly(12, float), horizon=2, models=["magic"]), LIMITS, engine)
    assert status == 400
    assert out["errors"][0]["field"] == "models"


# -- Bandes bornées à 0 (brutes et calibrées) -------------------------------------------------------

def test_bandes_tsb_bornees_a_zero(engine):
    _, out = run_forecast(body(sparse_rows(n=36, seed=1), horizon=6, models=["tsb"],
                               confidence_levels=[0.8, 0.95]), LIMITS, engine)
    s = out["series"][0]
    assert s["calibration"] is not None
    for f in s["forecast"]:
        assert f["lower_80"] >= 0
        assert f["lower_95"] >= 0


# -- Valeurs non arrondies à l'entier sur série intermittente/lumpy -----------------------------

def test_valeurs_non_arrondies_sur_intermittente(engine):
    rows = sparse_rows(n=36, seed=1)  # valeurs entières (poisson) dans l'historique
    _, out = run_forecast(body(rows, horizon=6, models=["tsb"]), LIMITS, engine)
    s = out["series"][0]
    assert s["demand"]["type"] == "intermittent"
    assert any(f["value"] != int(f["value"]) for f in s["forecast"])


# -- Avertissements ------------------------------------------------------------------------------

def test_avertissement_ventes_rares(engine):
    _, out = run_forecast(body(sparse_rows(n=36, seed=1), horizon=6, models=["auto"]), LIMITS, engine)
    s = out["series"][0]
    assert any(w.startswith("Ventes rares (intermittent)") for w in s["warnings"])


def test_avertissement_modele_explicite_sur_intermittente(engine):
    _, out = run_forecast(body(sparse_rows(n=36, seed=1), horizon=6, models=["arima"]), LIMITS, engine)
    s = out["series"][0]
    assert s["model"] == "arima"
    assert any("mode auto ou tsb/imapa" in w for w in s["warnings"])


# -- Non-régression additive : série lisse, réponse inchangée hors champ demand -----------------

def test_non_regression_serie_lisse(engine):
    """Additif : l'ajout de `demand` ne change aucun autre champ ni aucune valeur de la réponse."""
    rows = monthly(30, lambda i: 100 + i)
    _, out = run_forecast(body(rows, horizon=4, models=["naive"], confidence_levels=[0.8]), LIMITS, engine)
    s = out["series"][0]
    known_keys = {"model", "history", "forecast", "metrics", "baseline", "beats_baseline",
                  "reliability", "calibration", "warnings"}
    assert set(s) - {"demand"} == known_keys
    assert s["demand"]["type"] == "smooth"
    assert s["model"] == "naive"
    last = s["history"][-1]["value"]
    assert all(f["value"] == last for f in s["forecast"])  # invariant du modèle naïf, inchangé


def test_backtest_ne_calcule_que_les_composants_du_type_de_demande():
    """Série lisse : ni tsb ni imapa ; série intermittente : ni ets ni theta (coût de calcul)."""
    from th2fc.engine import Engine

    seen = []

    class Spy(Engine):
        def run(self, tasks, levels, frequency, step):
            seen.extend((t.sparse, frozenset(t.models)) for t in tasks)
            for t in tasks:
                t.out.clear()

    smooth = [dict(r, store="A") for r in monthly(30, lambda t: 100.0 + t)]
    sparse = [dict(r, store="B") for r in sparse_rows(n=30)]
    run_forecast(body(smooth + sparse, group_var="store", horizon=3), LIMITS, Spy())
    assert {sp for sp, _ in seen} == {False, True}
    for sp, models in seen:
        if sp:
            assert not models & {"ets", "theta"}
        else:
            assert not models & {"tsb", "imapa", "croston"}


def test_pas_davertissement_modele_quand_tsb_demande(engine):
    status, res = run_forecast(body(sparse_rows(n=30), horizon=3, models=["tsb"]), LIMITS, engine)
    assert status == 200
    assert not any("plus adaptés" in w for w in res["series"][0]["warnings"])
