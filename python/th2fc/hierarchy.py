"""Hiérarchie de séries et réconciliation MinT (numpy seul).

Une hiérarchie ajoute, au-dessus de `group_var`, des niveaux d'agrégation (ex. région > magasin).
Ce module construit l'arbre (nœuds + matrice de sommation S), agrège les séries du bas, estime la
matrice de covariance des erreurs par l'estimateur à rétrécissement de Schäfer-Strimmer (comme
`hierarchicalforecast.methods.MinTrace(method="mint_shrink")`) et réconcilie les prévisions.
"""
from __future__ import annotations

from dataclasses import dataclass, field

import numpy as np
import pandas as pd

from . import contract as c

TOTAL_LABEL = "Total"


@dataclass
class Node:
    label: str          # valeur affichée : "Total", la valeur de la colonne de hiérarchie, ou le groupe
    level: str           # "total" | "<colonne>" | "bottom"
    path: tuple          # identité interne (désambiguïse deux nœuds de même libellé, parents différents)


@dataclass
class Hierarchy:
    nodes: list[Node]         # ordre de réponse : total, niveaux intermédiaires (haut -> bas), bas
    bottom_order: list        # valeurs de group_var, dans l'ordre des colonnes de S
    S: np.ndarray              # (n_nodes, n_bottom), 0/1 : S[i, j] = 1 si le bas j contribue au nœud i
    bottom_start: int          # index du premier nœud "bottom" dans `nodes` (= len(nodes) - n_bottom)


def _raw(v) -> str | None:
    return c._as_character(v)


def parse_fields(hierarchy_var, reconciliation_var, group_var, errors: list) -> tuple[list[str], str]:
    """Validation de type des deux champs de requête ; ne connaît pas encore les colonnes disponibles."""
    if hierarchy_var in (None, []):
        hierarchy_cols: list[str] = []
    elif isinstance(hierarchy_var, list) and all(isinstance(x, str) and x for x in hierarchy_var):
        hierarchy_cols = list(dict.fromkeys(hierarchy_var))
    else:
        errors.append(c.error("hierarchy", "Le champ 'hierarchy' doit être une liste de noms de colonnes."))
        hierarchy_cols = []
    if hierarchy_cols and not group_var:
        errors.append(c.error("hierarchy", "'hierarchy' exige 'group_var' (les colonnes de hiérarchie se situent au-dessus du groupe)."))

    allowed = ("mint", "bottom_up", "none")
    if reconciliation_var in (None, ""):
        reconciliation = "mint" if hierarchy_cols else "none"
    elif reconciliation_var in allowed:
        reconciliation = reconciliation_var
    else:
        errors.append(c.error("reconciliation", "Valeur '%s' inconnue ; valeurs acceptées : %s."
                              % (reconciliation_var, ", ".join(allowed))))
        reconciliation = "none"
    return hierarchy_cols, reconciliation


def build(df: pd.DataFrame, bottom_order: list, hierarchy_cols: list[str]) -> Hierarchy | None:
    """Construit l'arbre à partir des colonnes brutes de `df` (une valeur par ligne, par groupe).

    Lève `contract.Invalid` (400) : valeur non unique d'une colonne de hiérarchie au sein d'un
    groupe, ou collision de libellé entre niveaux (total, une colonne de hiérarchie, le bas).
    """
    if not hierarchy_cols:
        return None

    def label_g(g):
        return "" if g is None else " pour la série '%s'" % g

    # 1) une seule valeur par colonne de hiérarchie et par groupe.
    parent = {}  # group -> tuple(valeur par colonne de hiérarchie, du plus haut au plus bas)
    bad = []
    for g in bottom_order:
        mask = df["group"].isna() if g is None else df["group"] == g
        path = []
        for col in hierarchy_cols:
            values = list(dict.fromkeys(_raw(v) for v in df.loc[mask, col]))
            if len(values) != 1 or values[0] is None:
                bad.append("colonne '%s'%s : %s" % (col, label_g(g),
                           "aucune valeur" if not values or values[0] is None and len(values) == 1
                           else "%d valeurs différentes (%s)" % (len(values), ", ".join(v or "" for v in values))))
                path.append(None)
            else:
                path.append(values[0])
        parent[g] = tuple(path)
    if bad:
        raise c.Invalid(400, [c.error("hierarchy", "Colonne de hiérarchie non unique par groupe (une seule valeur "
                                      "attendue par groupe) : %s." % " ; ".join(bad))])

    # 2) collision de libellé entre niveaux (total, chaque colonne, bas).
    level_of_label: dict[str, str] = {TOTAL_LABEL: "total"}
    collisions = []
    for j, col in enumerate(hierarchy_cols):
        for g in bottom_order:
            lbl = parent[g][j]
            prev = level_of_label.get(lbl)
            if prev is not None and prev != col:
                collisions.append("'%s' apparaît à la fois au niveau '%s' et au niveau '%s'" % (lbl, prev, col))
            level_of_label[lbl] = col
    for g in bottom_order:
        lbl = "" if g is None else str(g)
        prev = level_of_label.get(lbl)
        if prev is not None and prev != "bottom":
            collisions.append("'%s' apparaît à la fois au niveau '%s' et au niveau 'bottom' (le groupe)" % (lbl, prev))
        level_of_label[lbl] = "bottom"
    if collisions:
        raise c.Invalid(400, [c.error("hierarchy", "Collision de libellé entre niveaux : %s."
                                      % " ; ".join(dict.fromkeys(collisions)))])

    # 3) nœuds : total, puis chaque colonne (du haut au bas, valeurs dans l'ordre de première
    #    apparition parmi les groupes), puis le bas.
    m = len(bottom_order)
    nodes = [Node(TOTAL_LABEL, "total", ())]
    rows = [np.ones(m, dtype=float)]
    for j, col in enumerate(hierarchy_cols):
        seen = []
        for g in bottom_order:
            prefix = parent[g][:j + 1]
            if prefix not in seen:
                seen.append(prefix)
        for prefix in seen:
            nodes.append(Node(prefix[-1], col, prefix))
            rows.append(np.array([1.0 if parent[g][:j + 1] == prefix else 0.0 for g in bottom_order]))
    bottom_start = len(nodes)
    for k, g in enumerate(bottom_order):
        nodes.append(Node("" if g is None else str(g), "bottom", ("__bottom__", g)))
        row = np.zeros(m)
        row[k] = 1.0
        rows.append(row)

    return Hierarchy(nodes=nodes, bottom_order=bottom_order, S=np.vstack(rows), bottom_start=bottom_start)


def aggregate(S: np.ndarray, y_bottom: np.ndarray) -> np.ndarray:
    """Sommes des séries filles : S (n, m) @ y_bottom (m, T) -> (n, T)."""
    return S @ y_bottom


# -- Estimateur de covariance à rétrécissement (Schäfer & Strimmer, 2005) -------------------------
def shrink_covariance(errors: np.ndarray) -> tuple[np.ndarray | None, float | None]:
    """errors : (T, n) résidus (un par nœud), supposés de moyenne nulle (résidus de prévision).

    Rétrécit la matrice de CORRÉLATION vers l'identité (les variances ne sont pas rétrécies), comme
    `hierarchicalforecast`'s `mint_shrink`. Renvoie (W, lambda) ; (None, None) si non estimable
    (moins de 2 points, ou une variance nulle).
    """
    T, n = errors.shape
    if T < 2 or n == 0:
        return None, None
    var = np.mean(errors ** 2, axis=0)  # variance non centrée (résidus supposés centrés)
    if np.any(var <= 0):
        return None, None
    s = np.sqrt(var)
    x = errors / s  # standardisé : (T, n)

    # w_tij = x_ti * x_tj ; wbar_ij = moyenne_t w_tij = corrélation empirique r_ij.
    # Var(r_ij) = T / (T-1)^3 * somme_t (w_tij - wbar_ij)^2, calculée pour tout (i, j) à la fois.
    r = (x.T @ x) / T  # (n, n), r_ii = 1
    # w[t, i, j] via broadcasting : (T, n, 1) * (T, 1, n)
    w = x[:, :, None] * x[:, None, :]  # (T, n, n)
    dev = w - r[None, :, :]
    var_r = (T / (T - 1) ** 3) * np.sum(dev ** 2, axis=0)  # (n, n)

    off = ~np.eye(n, dtype=bool)
    num = float(np.sum(var_r[off]))
    den = float(np.sum(r[off] ** 2))
    lam = 1.0 if den == 0 else num / den
    lam = min(max(lam, 0.0), 1.0)

    r_shrunk = r * (1 - lam)
    np.fill_diagonal(r_shrunk, 1.0)
    W = r_shrunk * np.outer(s, s)
    return W, lam


def reconcile(y_hat: np.ndarray, S: np.ndarray, W: np.ndarray) -> np.ndarray:
    """ŷ_rec = S (S' W⁻¹ S)⁻¹ S' W⁻¹ ŷ. y_hat : (n, T) ; renvoie (n, T)."""
    Winv = np.linalg.inv(W)
    mid = S.T @ Winv @ S
    P = np.linalg.solve(mid, S.T @ Winv)  # (m, n), évite l'inversion explicite de `mid`
    return S @ (P @ y_hat)


def bottom_up(y_hat_bottom: np.ndarray, S: np.ndarray) -> np.ndarray:
    """Agrégation pure des prévisions du bas (aucune information des niveaux hauts n'est utilisée)."""
    return S @ y_hat_bottom


def is_singular(W: np.ndarray) -> bool:
    try:
        # Un conditionnement extrême équivaut en pratique à une matrice non inversible.
        return not np.isfinite(np.linalg.cond(W)) or np.linalg.cond(W) > 1e12
    except np.linalg.LinAlgError:
        return True
