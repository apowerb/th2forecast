"""Classification Syntetos-Boylan de la demande (smooth / erratic / intermittent / lumpy).

ADI (average inter-demand interval) = nb de périodes / nb de périodes non nulles : mesure la
fréquence des ventes. CV² = (écart-type / moyenne)² des valeurs non nulles : mesure la
variabilité de la taille de la demande quand elle a lieu. Seuils historiques Syntetos & Boylan
(2005) : ADI = 1,32, CV² = 0,49.
"""
from __future__ import annotations

import numpy as np

ADI_THRESHOLD = 1.32
CV2_THRESHOLD = 0.49
SPARSE_TYPES = ("intermittent", "lumpy")


def is_sparse(demand_type: str) -> bool:
    return demand_type in SPARSE_TYPES


def classify(y: np.ndarray) -> dict:
    """Classe une série régularisée à partir de son historique.

    Une valeur négative ou moins de deux valeurs non nulles rend le CV² non fiable (variance
    non définie, ou signe qui n'a pas de sens pour une taille de demande) : la série est alors
    toujours classée `smooth`, sans bascule vers les autres types, même quand ADI est calculable.
    """
    y = np.asarray(y, dtype=float)
    n = len(y)
    nz = y[y != 0]
    zero_share = round(float(np.mean(y == 0)), 4) if n else 0.0
    adi = round(n / len(nz), 4) if len(nz) else None

    cv2 = None
    if len(nz) >= 2:
        mean = float(np.mean(nz))
        if mean != 0:
            cv2 = round((float(np.std(nz)) / mean) ** 2, 4)

    reliable = not (np.any(y < 0) or len(nz) < 2)
    if not reliable or adi is None or cv2 is None:
        demand_type = "smooth"
    elif adi < ADI_THRESHOLD:
        demand_type = "erratic" if cv2 >= CV2_THRESHOLD else "smooth"
    else:
        demand_type = "lumpy" if cv2 >= CV2_THRESHOLD else "intermittent"

    return {"type": demand_type, "adi": adi, "cv2": cv2, "zero_share": zero_share}
