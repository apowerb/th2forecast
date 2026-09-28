# th2forecast — moteur Python (Chronos-2 + statsforecast)

Service HTTP au **contrat v1 identique** à l'API R (`docs/API.md`) : mêmes routes, mêmes champs,
mêmes messages d'erreur, mêmes codes (400, 401, 404, 413). Le smoke test `tests/e2e/smoke.sh`
passe tel quel contre les deux services.

## Ce qui change par rapport à l'API R

- `auto` = **ensemble fixe** Chronos-2 + AutoETS + AutoARIMA + AutoTheta (moyenne des points et
  des bornes). Le champ `model` de la réponse vaut alors `ensemble`.
- `arima`, `ets`, `naive`, `snaive`, `prophet` restent disponibles individuellement.
- Backtest en **origines glissantes** (jusqu'à 3 fenêtres) au lieu d'un seul découpage ;
  les métriques sont poolées sur les fenêtres, `holdout_points` compte tous les points testés.
- Prévision finale toujours **ré-entraînée sur toute la série** (le défaut corrigé par la PR #5
  ne peut pas se produire ici).
- Toutes les séries d'une requête `group_var` sont calculées en lot (un appel statsforecast et
  un appel Chronos-2 par horizon).
- Écart connu : une date absente (`null`) est signalée comme non parsable.

## Mesures (banc M3 mensuel, 100 séries, h = 12, une requête)

| Moteur | MASE moyen | Couverture 80 % | Durée |
| --- | --- | --- | --- |
| API R corrigée (PR #5) | 0,912 | 0,63 | — |
| Python `arima` | 0,907 | 0,74 | 44 s |
| Python `auto` (ensemble) | **0,881** | **0,77** | 54 s |

Mesuré le 25/09/2026 sur une VM 4 vCPU sans GPU ; mémoire du conteneur ≈ 420 Mo après le banc.

## Lancer

```bash
docker build -t th2forecast-py python
docker run --rm -p 8000:8000 -e TH2FORECAST_API_TOKEN=... th2forecast-py
```

Les poids Chronos-2 (Apache-2.0, révision figée dans le `Dockerfile`) sont inclus dans l'image :
aucun accès réseau au démarrage.

## Variables d'environnement

| Variable | Défaut | Rôle |
| --- | --- | --- |
| `TH2FORECAST_API_TOKEN` | vide (pas d'auth) | Jeton Bearer attendu |
| `TH2FORECAST_MAX_ROWS` / `_MAX_SERIES` / `_MAX_HORIZON` | 100000 / 200 / 366 | Limites (413) |
| `TH2FORECAST_WORKERS` | 2 | Jobs asynchrones simultanés (0 = synchrone) |
| `TH2FORECAST_PRELOAD` | 1 | Charge Chronos-2 au démarrage |
| `TH2FORECAST_TORCH_THREADS` | nb de CPU | Threads torch |
| `TH2FORECAST_SF_JOBS` | 1 | Processus statsforecast |
| `TH2FORECAST_CHRONOS_PATH` | `/opt/models/chronos-2` | Dossier des poids |

## Tests

```bash
docker run --rm th2forecast-py python -m pytest -q -p no:cacheprovider
```
