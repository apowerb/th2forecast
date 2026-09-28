import pytest

from th2fc import contract as c
from conftest import LIMITS, monthly


def invalid(body, limits=LIMITS):
    with pytest.raises(c.Invalid) as e:
        c.validate(body, limits)
    return e.value


def test_colonne_date_absente_liste_les_colonnes():
    e = invalid({"data": [{"dt": "2024-01-01", "sales": 10}], "date_var": "date", "target_var": "sales", "horizon": 3})
    assert e.status == 400
    assert e.errors[0]["field"] == "date_var"
    assert e.errors[0]["message"] == "Colonne 'date' absente ; colonnes disponibles : dt, sales."


def test_cible_non_numerique():
    e = invalid({"data": [{"date": "2024-01-01", "sales": "abc"}, {"date": "2024-02-01", "sales": "def"}],
                 "date_var": "date", "target_var": "sales", "horizon": 1})
    assert e.errors[0] == {"field": "target_var", "message": "La colonne cible 'sales' doit être numérique."}


def test_dates_non_parsables():
    e = invalid({"data": [{"date": "pas-une-date", "sales": 1}], "date_var": "date", "target_var": "sales", "horizon": 1})
    assert e.errors[0]["message"] == ("Dates non parsables dans la colonne 'date' (format attendu YYYY-MM-DD) : pas-une-date.")


def test_modele_inconnu():
    e = invalid({"data": monthly(12, float), "date_var": "date", "target_var": "sales", "horizon": 2, "models": ["magic"]})
    assert e.errors[0]["field"] == "models"
    assert e.errors[0]["message"] == ("Modèle(s) inconnu(s) : magic ; modèles disponibles : prophet, arima, ets, snaive, naive, auto.")


def test_doublons_de_dates_par_serie():
    rows = [{"date": "2024-01-01", "sales": 1, "store": "A"}, {"date": "2024-01-01", "sales": 2, "store": "A"},
            {"date": "2024-02-01", "sales": 3, "store": "A"}]
    e = invalid({"data": rows, "date_var": "date", "target_var": "sales", "group_var": "store", "horizon": 1})
    assert e.errors[0]["message"] == "Doublons de dates détectés pour la série 'A' : 2024-01-01."


def test_historique_trop_court():
    e = invalid({"data": monthly(5, float), "date_var": "date", "target_var": "sales", "horizon": 12})
    assert e.errors[0]["field"] == "horizon"
    assert e.errors[0]["message"] == ("Historique trop court : 5 points pour un horizon de 12. "
                                      "Il faut au moins 13 points, ou réduire l'horizon à 4 au plus.")


@pytest.mark.parametrize("body, field, status", [
    ({"data": monthly(24, float), "horizon": 500}, "horizon", 413),
    ({"data": monthly(24, float), "horizon": 2}, "data", 413),
])
def test_limites_413(body, field, status):
    limits = dict(LIMITS, max_rows=10) if field == "data" else LIMITS
    e = invalid({**body, "date_var": "date", "target_var": "sales"}, limits)
    assert (e.status, e.errors[0]["field"]) == (status, field)


def test_trop_de_series_413():
    rows = [r for g in "ABC" for r in monthly(12, float, store=g)]
    e = invalid({"data": rows, "date_var": "date", "target_var": "sales", "group_var": "store", "horizon": 1},
                dict(LIMITS, max_series=2))
    assert e.status == 413
    assert e.errors[0]["message"] == "Nombre de séries (3) supérieur à la limite autorisée (2)."


def test_erreurs_de_structure_cumulees():
    e = invalid({"data": [], "horizon": 0, "frequency": "hour", "confidence_levels": [1.5]})
    assert [x["field"] for x in e.errors] == ["data", "date_var", "target_var", "horizon", "frequency", "confidence_levels"]


@pytest.mark.parametrize("horizon", ["12", True, 2.5, -1])
def test_horizon_invalide(horizon):
    e = invalid({"data": monthly(24, float), "date_var": "date", "target_var": "sales", "horizon": horizon})
    assert e.errors[0]["message"] == "Le champ 'horizon' doit être un entier strictement positif."


def test_corps_absent():
    assert invalid(None).errors == [{"field": None, "message": "Corps de requête JSON manquant ou invalide."}]


def test_valeurs_par_defaut():
    req = c.validate({"data": monthly(12, float), "date_var": "date", "target_var": "sales", "horizon": 2.0}, LIMITS)
    assert (req.horizon, req.models, req.confidence_levels, req.has_group) == (2, ["auto"], [0.8, 0.95], False)


def test_detection_de_frequence():
    from datetime import date, timedelta

    base = date(2024, 1, 1)
    assert c.detect_frequency([base + timedelta(days=i) for i in range(10)]) == "day"
    assert c.detect_frequency([base + timedelta(weeks=i) for i in range(10)]) == "week"
    assert c.detect_frequency([c.step(base, "month", i) for i in range(10)]) == "month"
    assert c.detect_frequency([c.step(base, "quarter", i) for i in range(10)]) == "quarter"
    assert c.detect_frequency([c.step(base, "year", i) for i in range(10)]) == "year"


def test_regularisation_comble_et_interpole():
    from datetime import date

    dates = [date(2024, 1, 1), date(2024, 2, 1), date(2024, 5, 1)]
    full, v, n = c.regularize(dates, [10.0, 20.0, 50.0], "month")
    assert n == 2
    assert [d.isoformat() for d in full] == ["2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01"]
    assert list(v) == [10.0, 20.0, 30.0, 40.0, 50.0]
