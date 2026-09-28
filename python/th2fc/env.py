"""Lecture des réglages TH2FORECAST_* depuis l'environnement.

Une variable présente mais vide vaut « non réglée » : Compose écrit
`NOM: ${NOM:-}` et pose alors la clé, vide, dans le conteneur. `os.environ.get`
ne retombe sur son défaut que si la clé est absente ; `int('')` faisait donc
planter le démarrage.
"""
import os


def env_str(name: str, default: str) -> str:
    value = os.environ.get(name, "").strip()
    return value or default


def env_int(name: str, default: int) -> int:
    value = os.environ.get(name, "").strip()
    if not value:
        return default
    try:
        return int(value)
    except ValueError:
        raise ValueError(f"{name} doit être un entier, reçu {value!r}") from None
