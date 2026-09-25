import numpy as np
import pandas as pd
import pytest

from th2fc import contract as c
from th2fc import hierarchy as h


# ---------------------------------------------------------------------------------
# parse_fields : validation légère des deux champs de requête.
# ---------------------------------------------------------------------------------

def test_hierarchy_sans_group_var_est_refusee():
    errors = []
    h.parse_fields(["region"], None, None, errors)
    assert errors and errors[0]["field"] == "hierarchy"
    assert "group_var" in errors[0]["message"]


def test_hierarchy_absente_donne_reconciliation_none_par_defaut():
    errors = []
    cols, reco = h.parse_fields(None, None, "store", errors)
    assert not errors
    assert cols == [] and reco == "none"


def test_hierarchy_presente_donne_reconciliation_mint_par_defaut():
    errors = []
    cols, reco = h.parse_fields(["region"], None, "store", errors)
    assert not errors
    assert cols == ["region"] and reco == "mint"


def test_reconciliation_inconnue_est_refusee():
    errors = []
    h.parse_fields(["region"], "magic", "store", errors)
    assert errors and errors[0]["field"] == "reconciliation"
    assert "mint, bottom_up, none" in errors[0]["message"]


def test_hierarchy_mauvais_type_est_refusee():
    errors = []
    h.parse_fields("region", None, "store", errors)
    assert errors and errors[0]["field"] == "hierarchy"


# ---------------------------------------------------------------------------------
# build : structure de l'arbre + validations qui dépendent des données.
# ---------------------------------------------------------------------------------

def _df(rows):
    return pd.DataFrame(rows)


def test_build_parent_non_unique_est_refuse_400():
    df = _df([
        {"group": "A", "region": "Nord"}, {"group": "A", "region": "Sud"},
        {"group": "B", "region": "Sud"}, {"group": "B", "region": "Sud"},
    ])
    with pytest.raises(c.Invalid) as e:
        h.build(df, ["A", "B"], ["region"])
    assert e.value.status == 400
    assert e.value.errors[0]["field"] == "hierarchy"
    assert "non unique" in e.value.errors[0]["message"]


def test_build_collision_de_libelle_entre_niveaux_est_refuse_400():
    # Un magasin nommé "Nord" alors que "Nord" est aussi une valeur de la colonne région.
    df = _df([
        {"group": "Nord", "region": "Nord"}, {"group": "B", "region": "Sud"},
    ])
    with pytest.raises(c.Invalid) as e:
        h.build(df, ["Nord", "B"], ["region"])
    assert e.value.status == 400
    assert "Collision de libellé" in e.value.errors[0]["message"]


def test_build_collision_avec_total_est_refusee():
    df = _df([{"group": "A", "region": "Total"}, {"group": "B", "region": "Sud"}])
    with pytest.raises(c.Invalid) as e:
        h.build(df, ["A", "B"], ["region"])
    assert "Collision de libellé" in e.value.errors[0]["message"]


def test_build_arbre_un_niveau_ordre_et_matrice_s():
    df = _df([
        {"group": "A", "region": "Nord"}, {"group": "B", "region": "Nord"}, {"group": "C", "region": "Sud"},
    ])
    hier = h.build(df, ["A", "B", "C"], ["region"])
    labels = [(n.label, n.level) for n in hier.nodes]
    assert labels == [("Total", "total"), ("Nord", "region"), ("Sud", "region"),
                      ("A", "bottom"), ("B", "bottom"), ("C", "bottom")]
    assert hier.bottom_start == 3
    np.testing.assert_array_equal(hier.S, np.array([
        [1, 1, 1],   # Total
        [1, 1, 0],   # Nord = A + B
        [0, 0, 1],   # Sud = C
        [1, 0, 0],   # A
        [0, 1, 0],   # B
        [0, 0, 1],   # C
    ], dtype=float))


def test_build_deux_niveaux():
    df = _df([
        {"group": "A", "region": "Nord", "zone": "N1"},
        {"group": "B", "region": "Nord", "zone": "N2"},
        {"group": "C", "region": "Sud", "zone": "S1"},
    ])
    hier = h.build(df, ["A", "B", "C"], ["region", "zone"])
    levels = [n.level for n in hier.nodes]
    assert levels == ["total", "region", "region", "zone", "zone", "zone", "bottom", "bottom", "bottom"]
    # La zone "N1" doit sommer seulement A, la région "Nord" doit sommer A + B.
    nord = hier.nodes.index(next(n for n in hier.nodes if n.label == "Nord"))
    n1 = next(i for i, n in enumerate(hier.nodes) if n.label == "N1")
    np.testing.assert_array_equal(hier.S[nord], [1, 1, 0])
    np.testing.assert_array_equal(hier.S[n1], [1, 0, 0])


def test_build_sans_colonnes_renvoie_none():
    assert h.build(_df([{"group": "A"}]), ["A"], []) is None


# ---------------------------------------------------------------------------------
# aggregate : les agrégats sont la somme de leurs enfants.
# ---------------------------------------------------------------------------------

def test_aggregate_est_une_somme():
    S = np.array([[1, 1], [1, 0], [0, 1]], dtype=float)
    y = np.array([[1.0, 2.0, 3.0], [10.0, 20.0, 30.0]])
    agg = h.aggregate(S, y)
    np.testing.assert_allclose(agg[0], y[0] + y[1])


# ---------------------------------------------------------------------------------
# shrink_covariance : comparaison à une implémentation de référence (boucles explicites),
# d'après Schäfer & Strimmer (2005) / hierarchicalforecast `mint_shrink`.
# ---------------------------------------------------------------------------------

def _reference_shrink(errors: np.ndarray):
    """Référence non vectorisée, écrite indépendamment de hierarchy.shrink_covariance."""
    T, n = errors.shape
    var = np.array([np.mean([errors[t, i] ** 2 for t in range(T)]) for i in range(n)])
    s = np.sqrt(var)
    x = errors / s

    r = np.zeros((n, n))
    for i in range(n):
        for j in range(n):
            r[i, j] = sum(x[t, i] * x[t, j] for t in range(T)) / T

    var_r = np.zeros((n, n))
    for i in range(n):
        for j in range(n):
            wbar = r[i, j]
            acc = 0.0
            for t in range(T):
                w = x[t, i] * x[t, j]
                acc += (w - wbar) ** 2
            var_r[i, j] = T / (T - 1) ** 3 * acc

    num = sum(var_r[i, j] for i in range(n) for j in range(n) if i != j)
    den = sum(r[i, j] ** 2 for i in range(n) for j in range(n) if i != j)
    lam = min(max(num / den if den else 1.0, 0.0), 1.0)

    r_shrunk = r * (1 - lam)
    for i in range(n):
        r_shrunk[i, i] = 1.0
    W = np.zeros((n, n))
    for i in range(n):
        for j in range(n):
            W[i, j] = r_shrunk[i, j] * s[i] * s[j]
    return W, lam


def test_shrink_covariance_contre_reference_a_la_main():
    rng = np.random.default_rng(42)
    # Erreurs corrélées (une composante commune + bruit propre), 9 points, 4 nœuds : T petit devant
    # n^2/2 paires de corrélations, cas où le rétrécissement est utile (matrice brute peu fiable).
    T, n = 9, 4
    common = rng.normal(0, 1, T)
    errors = np.column_stack([common * b + rng.normal(0, 0.5, T) for b in (1.0, 0.8, -0.6, 0.3)])

    W, lam = h.shrink_covariance(errors)
    W_ref, lam_ref = _reference_shrink(errors)

    assert 0.0 <= lam <= 1.0
    np.testing.assert_allclose(lam, lam_ref, atol=1e-9)
    np.testing.assert_allclose(W, W_ref, atol=1e-9)
    # symétrique, diagonale = variance brute (non rétrécie).
    np.testing.assert_allclose(W, W.T)
    np.testing.assert_allclose(np.diag(W), np.mean(errors ** 2, axis=0))


def test_shrink_covariance_lambda_1_si_correlations_tres_bruitees():
    # Peu de points (3) pour 5 séries indépendantes : le rétrécissement doit dominer (lambda proche de 1),
    # la matrice résultante doit rester proche de la diagonale.
    rng = np.random.default_rng(7)
    errors = rng.normal(0, 1, (3, 5))
    W, lam = h.shrink_covariance(errors)
    assert lam > 0.5
    off = W - np.diag(np.diag(W))
    assert np.max(np.abs(off)) < np.max(np.diag(W))


def test_shrink_covariance_insuffisant_renvoie_none():
    assert h.shrink_covariance(np.zeros((1, 3))) == (None, None)
    assert h.shrink_covariance(np.zeros((5, 0))) == (None, None)


# ---------------------------------------------------------------------------------
# reconcile : cohérence garantie par construction (S (S'W⁻¹S)⁻¹S'W⁻¹S = S) et repli bottom_up.
# ---------------------------------------------------------------------------------

def test_reconcile_rend_les_agregats_coherents_avec_le_bas():
    S = np.array([[1, 1], [1, 0], [0, 1]], dtype=float)  # Total, A, B
    rng = np.random.default_rng(3)
    y_hat = rng.normal(10, 2, (3, 6))  # prévisions incohérentes (bruit indépendant par nœud)
    W = np.eye(3) * np.array([2.0, 1.0, 1.5])
    rec = h.reconcile(y_hat, S, W)
    np.testing.assert_allclose(rec[0], rec[1] + rec[2], atol=1e-9)


def test_reconcile_scenarios_aussi_coherents():
    S = np.array([[1, 1], [1, 0], [0, 1]], dtype=float)
    rng = np.random.default_rng(4)
    W = np.array([[2.0, 0.3, 0.1], [0.3, 1.0, 0.05], [0.1, 0.05, 1.2]])
    for _ in range(3):
        y_hat = rng.normal(5, 3, (3, 4))
        rec = h.reconcile(y_hat, S, W)
        np.testing.assert_allclose(rec[0], rec[1] + rec[2], atol=1e-8)


def test_bottom_up_est_une_simple_somme_et_coherent():
    S = np.array([[1, 1], [1, 0], [0, 1]], dtype=float)
    y_bottom_only = np.array([[np.nan, np.nan], [3.0, 4.0], [5.0, 6.0]])  # ligne Total ignorée par bottom_up
    y_bottom = y_bottom_only[1:]
    rec = h.bottom_up(y_bottom, S[:, :])
    np.testing.assert_allclose(rec[0], rec[1] + rec[2])


def test_is_singular_detecte_une_matrice_non_inversible():
    assert h.is_singular(np.array([[1.0, 1.0], [1.0, 1.0]]))
    assert not h.is_singular(np.eye(3))
