import numpy as np
import pytest

from th2fc.forecast import run_forecast
from conftest import LIMITS, monthly


def hier_rows(n=30):
    """Trois magasins (A, B sous Nord ; C sous Sud), séries linéaires bruitées mais distinctes."""
    rng = np.random.default_rng(0)
    rows = []
    specs = [("A", "Nord", 50), ("B", "Nord", 80), ("C", "Sud", 30)]
    for store, region, base in specs:
        rows += monthly(n, lambda i, b=base: float(b + i + rng.normal(0, 1)), store=store, region=region)
    return rows


def body(rows, **kw):
    return {"data": rows, "date_var": "date", "target_var": "sales", "group_var": "store", **kw}


def _coherent(out, atol=1e-6):
    by_group = {(s["group"], s["level"]): s for s in out["series"]}
    total = next(s for s in out["series"] if s["level"] == "total")
    nord = next(s for s in out["series"] if s["level"] == "region" and s["group"] == "Nord")
    sud = next(s for s in out["series"] if s["level"] == "region" and s["group"] == "Sud")
    a = next(s for s in out["series"] if s["level"] == "bottom" and s["group"] == "A")
    b = next(s for s in out["series"] if s["level"] == "bottom" and s["group"] == "B")
    cc = next(s for s in out["series"] if s["level"] == "bottom" and s["group"] == "C")
    for k in range(len(total["forecast"])):
        assert nord["forecast"][k]["value"] == pytest.approx(a["forecast"][k]["value"] + b["forecast"][k]["value"], abs=atol)
        assert sud["forecast"][k]["value"] == pytest.approx(cc["forecast"][k]["value"], abs=atol)
        assert total["forecast"][k]["value"] == pytest.approx(nord["forecast"][k]["value"] + sud["forecast"][k]["value"], abs=atol)


@pytest.mark.parametrize("method", ["mint", "bottom_up"])
def test_coherence_point_et_bornes_agregats_somme_des_enfants(engine, method):
    status, out = run_forecast(body(hier_rows(), horizon=4, models=["ets"],
                                    hierarchy=["region"], reconciliation=method), LIMITS, engine)
    assert status == 200
    assert out["reconciliation"]["method"] in ("mint_shrink", "bottom_up")
    assert out["reconciliation"]["coherent"] is True
    _coherent(out)


def test_ordre_et_champ_level():
    pass  # couvert ci-dessous avec un modèle rapide (naive), pas besoin du moteur complet


def test_reponse_ordre_total_intermediaires_bas_et_champ_level(engine):
    _, out = run_forecast(body(hier_rows(), horizon=3, models=["naive"], hierarchy=["region"]), LIMITS, engine)
    levels = [s["level"] for s in out["series"]]
    assert levels == ["total", "region", "region", "bottom", "bottom", "bottom"]
    groups = [s["group"] for s in out["series"]]
    assert groups[0] == "Total"
    assert set(groups[1:3]) == {"Nord", "Sud"}
    assert set(groups[3:]) == {"A", "B", "C"}


def test_reconciliation_none_par_defaut_absente_sans_hierarchy(engine):
    _, out = run_forecast(body(hier_rows(), horizon=3, models=["naive"]), LIMITS, engine)
    assert "reconciliation" not in out
    assert all("level" not in s for s in out["series"])


def test_reconciliation_explicitement_none_avec_hierarchy(engine):
    _, out = run_forecast(body(hier_rows(), horizon=3, models=["naive"], hierarchy=["region"],
                               reconciliation="none"), LIMITS, engine)
    assert out["reconciliation"] == {"method": "none", "coherent": False}
    # sans réconciliation, rien ne garantit la cohérence (les niveaux sont prévus indépendamment) :
    # on ne l'exige pas ici, juste que le format de réponse reste correct.
    assert {s["level"] for s in out["series"]} == {"total", "region", "bottom"}


def test_requete_sans_hierarchy_reponse_inchangee(engine):
    rows = monthly(24, lambda i: 100 + i, store="A") + monthly(24, lambda i: 50 + i, store="B")
    b = {"data": rows, "date_var": "date", "target_var": "sales", "group_var": "store", "horizon": 3, "models": ["naive"]}
    _, out1 = run_forecast(dict(b), LIMITS, engine)
    _, out2 = run_forecast(dict(b, hierarchy=None), LIMITS, engine)
    assert out1["series"] == out2["series"]
    assert "reconciliation" not in out1 and "reconciliation" not in out2


def test_backtest_avant_apres_present_quand_mint(engine):
    _, out = run_forecast(body(hier_rows(48), horizon=3, models=["ets"], hierarchy=["region"]), LIMITS, engine)
    bt = out["reconciliation"].get("backtest")
    assert bt is not None
    assert bt["points"] > 0
    assert bt["mase_base"] >= 0 and bt["mase_reconciled"] >= 0


def test_max_series_compte_les_agregats():
    rows = hier_rows()
    limits = dict(LIMITS, max_series=4)  # 3 bas + Nord + Sud + Total = 6 > 4
    from th2fc import contract as c
    with pytest.raises(c.Invalid) as e:
        c.validate(body(rows, horizon=3, hierarchy=["region"]), limits)
    assert e.value.status == 413
    assert "agrégats" in e.value.errors[0]["message"]
def test_dates_non_alignees_est_refuse_400(engine):
    from datetime import date

    rows = ([{"date": date(2024, m, 1).isoformat(), "sales": 10 + m, "store": "A", "region": "Nord"} for m in range(1, 13)]
           + [{"date": date(2023, m, 1).isoformat(), "sales": 20 + m, "store": "B", "region": "Nord"} for m in range(1, 13)])
    status, out = run_forecast(body(rows, horizon=2, models=["naive"], hierarchy=["region"]), LIMITS, engine)
    assert status == 400
    assert out["errors"][0]["field"] == "hierarchy"
    assert "non alignées" in out["errors"][0]["message"]


def _two_region_hierarchy():
    from th2fc import hierarchy as hie
    df = __import__("pandas").DataFrame({"group": ["A", "B"], "region": ["Nord", "Sud"]})
    return hie.build(df, ["A", "B"], ["region"])  # nœuds : Total, Nord, Sud, A, B


def test_repli_bottom_up_si_w_singuliere():
    from th2fc.forecast import _reconcile_matrix

    hier = _two_region_hierarchy()
    Y = np.array([[10.0, 11.0], [4.0, 5.0], [6.0, 6.0], [4.0, 5.0], [6.0, 6.0]])  # Total, Nord, Sud, A, B
    W_singular = np.ones((5, 5))  # rang 1 : singulière
    Y_rec, method, warn = _reconcile_matrix(hier, Y, W_singular, "mint")
    assert method == "bottom_up"
    assert warn is not None and "singulière" in warn
    np.testing.assert_allclose(Y_rec[0], Y_rec[3] + Y_rec[4])


def test_repli_bottom_up_si_w_absente():
    from th2fc.forecast import _reconcile_matrix

    hier = _two_region_hierarchy()
    Y = np.array([[10.0], [4.0], [6.0], [4.0], [6.0]])
    Y_rec, method, warn = _reconcile_matrix(hier, Y, None, "mint")
    assert method == "bottom_up" and "non estimable" in warn
