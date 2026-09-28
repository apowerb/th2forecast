import pytest

from th2fc import contract as c
from th2fc.calibration import adaptive_level
from th2fc.forecast import run_forecast

from conftest import LIMITS, monthly


def body(rows, **kw):
    return {"data": rows, "date_var": "date", "target_var": "sales", **kw}


# -- Calcul ACI, à la main -------------------------------------------------------------------


def test_adaptive_level_elargit_sur_depassements_repetes():
    # alpha_1 = 0.2 ; chaque err=1 : alpha -= 0.04 -> alpha_final = 0.04 -> level_used = 0.96
    assert adaptive_level([1, 1, 1, 1], 0.8) == pytest.approx(0.96)


def test_adaptive_level_ne_resserre_jamais_sous_le_niveau_demande():
    # alpha_1 = 0.2 ; chaque err=0 : alpha += 0.01 -> alpha_final = 0.24 -> level_used brut = 0.76 < 0.8
    assert adaptive_level([0, 0, 0, 0], 0.8) == 0.8


def test_adaptive_level_plafonne_a_0995():
    assert adaptive_level([1] * 100, 0.8) == 0.995


def test_adaptive_level_sequence_alternee():
    # calculée à la main : alpha1=0.2 ; +1,0,+1,0,+1,0 (err) -> alpha_final=0.11 -> level_used=0.89
    assert adaptive_level([1, 0, 1, 0, 1, 0], 0.8) == pytest.approx(0.89)


# -- Validation --------------------------------------------------------------------------------


def test_feedback_doit_etre_une_liste():
    with pytest.raises(c.Invalid) as e:
        c.validate(body(monthly(12, lambda i: 100 + i), horizon=2, feedback="oups"), LIMITS)
    assert e.value.status == 400
    assert e.value.errors[0]["field"] == "feedback"


def test_feedback_element_sans_points_rejete():
    with pytest.raises(c.Invalid) as e:
        c.validate(body(monthly(12, lambda i: 100 + i), horizon=2,
                        feedback=[{"group": None, "points": []}]), LIMITS)
    assert e.value.status == 400
    assert e.value.errors[0]["field"] == "feedback"


def test_feedback_point_date_invalide_rejete():
    with pytest.raises(c.Invalid) as e:
        c.validate(body(monthly(12, lambda i: 100 + i), horizon=2,
                        feedback=[{"group": None, "points": [{"date": "pas-une-date", "lower_80": 1, "upper_80": 2}]}]),
                  LIMITS)
    assert e.value.status == 400
    assert e.value.errors[0]["field"] == "feedback"


# -- Sans feedback : réponse inchangée ---------------------------------------------------------


def test_sans_feedback_reponse_inchangee(engine):
    rows = monthly(20, lambda i: 100 + i)
    _, out_absent = run_forecast(body(rows, horizon=2, models=["naive"], confidence_levels=[0.8]), LIMITS, engine)
    _, out_vide = run_forecast(body(rows, horizon=2, models=["naive"], confidence_levels=[0.8], feedback=[]),
                               LIMITS, engine)
    out_absent.pop("duration_ms"), out_vide.pop("duration_ms")  # seul champ qui varie légitimement entre 2 appels
    assert out_absent == out_vide
    assert "adaptive" not in out_absent["series"][0]["calibration"]


# -- Intégration : appariement, ACI, absence de resserrement -----------------------------------


def _feedback_points(hist, errs, tag="80"):
    """Points de feedback forçant la séquence d'erreurs `errs` (bande large -> 0, bande absurde -> 1)."""
    pts = []
    for h, e in zip(hist, errs):
        v = h["value"]
        lo, hi = (v - 1000, v - 999) if e else (v - 1, v + 1)
        pts.append({"date": h["date"], "value": v, "lower_" + tag: lo, "upper_" + tag: hi})
    return pts


def test_aci_appliquee_et_calculee_a_la_main(engine):
    rows = monthly(20, lambda i: 100 + i)
    _, out0 = run_forecast(body(rows, horizon=2, models=["naive"], confidence_levels=[0.8]), LIMITS, engine)
    hist = out0["series"][0]["history"]
    errs = [1, 0, 1, 0, 1, 0]
    feedback = [{"group": None, "level": None, "points": _feedback_points(hist[:6], errs)}]

    _, out = run_forecast(body(rows, horizon=2, models=["naive"], confidence_levels=[0.8], feedback=feedback),
                          LIMITS, engine)
    adaptive = out["series"][0]["calibration"]["adaptive"]["80"]
    assert adaptive["target"] == 0.8
    assert adaptive["points"] == 6
    assert adaptive["observed"] == pytest.approx(0.5, abs=1e-9)
    assert adaptive["level_used"] == pytest.approx(0.89, abs=1e-9)
    assert adaptive["level_used"] >= 0.8


def test_moins_de_4_points_ignore(engine):
    rows = monthly(20, lambda i: 100 + i)
    _, out0 = run_forecast(body(rows, horizon=2, models=["naive"], confidence_levels=[0.8]), LIMITS, engine)
    hist = out0["series"][0]["history"]
    feedback = [{"group": None, "level": None, "points": _feedback_points(hist[:3], [1, 1, 1])}]

    _, out = run_forecast(body(rows, horizon=2, models=["naive"], confidence_levels=[0.8], feedback=feedback),
                          LIMITS, engine)
    assert "adaptive" not in out["series"][0]["calibration"]
    assert out["series"][0]["forecast"] == out0["series"][0]["forecast"]


def test_appariement_par_dates_de_lhistorique_seulement(engine):
    rows = monthly(20, lambda i: 100 + i)
    _, out0 = run_forecast(body(rows, horizon=2, models=["naive"], confidence_levels=[0.8]), LIMITS, engine)
    hist = out0["series"][0]["history"]
    pts = _feedback_points(hist[:4], [1, 1, 1, 1])
    pts.append({"date": "1900-01-01", "value": 0, "lower_80": -1, "upper_80": 1})  # hors historique : ignoré
    feedback = [{"group": None, "level": None, "points": pts}]

    _, out = run_forecast(body(rows, horizon=2, models=["naive"], confidence_levels=[0.8], feedback=feedback),
                          LIMITS, engine)
    assert out["series"][0]["calibration"]["adaptive"]["80"]["points"] == 4


def test_serie_inconnue_ignoree_avec_avertissement_global(engine):
    rows = monthly(20, lambda i: 100 + i)
    feedback = [{"group": "fantome", "level": None,
                "points": [{"date": "2022-01-01", "value": 100, "lower_80": 90, "upper_80": 110}]}]
    status, out = run_forecast(body(rows, horizon=2, models=["naive"], feedback=feedback), LIMITS, engine)
    assert status == 200
    assert any("fantome" in w for w in out["warnings"])


# -- Hiérarchie : appliquée nœud par nœud avant réconciliation ---------------------------------


def _hier_rows(n=24):
    rows = []
    rows += monthly(n, lambda i: 50 + i, store="A", region="Nord")
    rows += monthly(n, lambda i: 80 + i, store="B", region="Nord")
    return rows


def test_hierarchie_feedback_bottom_et_agregat_independants(engine):
    rows = _hier_rows()
    hbody = body(rows, group_var="store", hierarchy=["region"], horizon=2, models=["naive"],
                confidence_levels=[0.8])
    _, out0 = run_forecast(hbody, LIMITS, engine)
    hist_a = next(s for s in out0["series"] if s.get("group") == "A" and s["level"] == "bottom")["history"]
    hist_nord = next(s for s in out0["series"] if s.get("group") == "Nord" and s["level"] == "region")["history"]

    feedback = [
        {"group": "A", "level": "bottom", "points": _feedback_points(hist_a[:6], [1, 0, 1, 0, 1, 0])},
        {"group": "Nord", "level": "region", "points": _feedback_points(hist_nord[:6], [1, 1, 1, 1, 0, 0])},
    ]
    _, out = run_forecast({**hbody, "feedback": feedback}, LIMITS, engine)
    a = next(s for s in out["series"] if s.get("group") == "A" and s["level"] == "bottom")
    nord = next(s for s in out["series"] if s.get("group") == "Nord" and s["level"] == "region")
    assert "adaptive" in a["calibration"] and "80" in a["calibration"]["adaptive"]
    assert "adaptive" in nord["calibration"] and "80" in nord["calibration"]["adaptive"]
    assert a["calibration"]["adaptive"]["80"]["points"] == 6
    assert nord["calibration"]["adaptive"]["80"]["points"] == 6
