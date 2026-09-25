# API th2forecast v1

Contrat source : voir `../CONTRAT.md` du dépôt de coordination `th2fc`
(section « Service R »), fixé le 24/09/2026. Ce document en reprend le
contenu et précise les choix d'implémentation pris pour le lot A.

## Authentification

Si la variable d'environnement `TH2FORECAST_API_TOKEN` est définie, toutes
les routes sauf `GET /health` exigent l'en-tête `Authorization: Bearer <token>`.
Sinon, l'authentification est désactivée (usage local/dev uniquement).

Réponse en cas d'échec : `401` avec le format d'erreur standard (voir plus bas).

## Limites (variables d'environnement, valeurs par défaut)

| Variable | Défaut | Effet si dépassé |
|---|---|---|
| `TH2FORECAST_MAX_ROWS` | 100000 | 413 |
| `TH2FORECAST_MAX_SERIES` | 200 | 413 |
| `TH2FORECAST_MAX_HORIZON` | 366 | 413 |

Ces limites portent sur le contenu métier de la requête déjà désérialisée
(nombre de lignes, nombre de séries distinctes, horizon demandé) : plumber2
ne documente pas de mécanisme natif de limite de taille brute en octets, ce
choix est donc documenté comme un écart mineur au libellé du contrat.

## Endpoints

- `GET /health` → `{"status":"UP","version":"<version du package>"}`
- `POST /v1/forecast` (synchrone) → 200, réponse décrite plus bas.
- `POST /v1/jobs` → 202 `{"job_id":"…","status":"queued"}`.
  `GET /v1/jobs/{id}` → `{"job_id","status":"queued|running|succeeded|failed","result":<réponse>|null,"error":<erreur>|null}` ;
  404 si `id` inconnu.
- **Écart au contrat, documenté** : l'ancien `POST /forecast` (base64 R,
  `plumber` v1) a été **retiré** plutôt que conservé. Le fichier
  `R/plumber_th2_forecast.R` était écrit pour l'API plumber v1
  (`function(res, input_data, ...)`), incompatible avec plumber2 installé
  (voir NEWS.md), et ses `tryCatch(..., error = ...)` ne stoppaient pas
  l'exécution de `forecast()` (un renvoi de type erreur dans un callback
  `tryCatch` ne fait pas sortir la fonction englobante), ce qui produisait un
  `500` opaque sur toute entrée invalide. Le remplacer proprement aurait
  dupliqué toute la logique de validation déjà écrite pour `/v1/forecast`
  sans bénéfice pour les lots B/C, qui consomment exclusivement le JSON v1.

### Asynchrone (`/v1/jobs`)

Implémenté avec le package `mirai` (daemons lancés dans `entrypoint.R` via
`mirai::daemons()`), recommandé par la documentation plumber2 pour
l'exécution asynchrone. `POST /v1/jobs` valide la requête de façon
**synchrone** (échec rapide en 400/413 sans créer de job), puis délègue le
calcul à un processus `mirai` séparé si des daemons sont configurés. Repli
documenté : si aucun daemon mirai n'est disponible au démarrage (variable
`TH2FORECAST_WORKERS=0` ou échec de `mirai::daemons()`), le job est exécuté
en synchrone au moment du `POST` et immédiatement renvoyé comme
`"succeeded"`/`"failed"` au premier `GET` — le contrat HTTP (202 puis
`GET /v1/jobs/{id}`) reste respecté, seule la parallélisation réelle est
perdue dans ce cas de repli.

### Requête (JSON) — `POST /v1/forecast` et `POST /v1/jobs`

```json
{
  "data": [{"date": "2024-01-01", "sales": 120, "store": "A"}],
  "date_var": "date",
  "target_var": "sales",
  "group_var": null,
  "horizon": 12,
  "frequency": null,
  "models": ["prophet"],
  "confidence_levels": [0.8, 0.95],
  "holidays_country": null
}
```

- `data` : tableau d'objets (lignes).
- `frequency` : `null` (détection automatique) ou une valeur explicite parmi
  `day`, `week`, `month`, `quarter`, `year`. Une fréquence explicite est
  toujours prioritaire sur la détection.
- `models` : sous-ensemble de `["prophet","arima","ets","snaive","naive","auto"]`.

**Précision d'implémentation (choix documenté, contrat ambigu sur ce point) :**
le schéma de réponse ne renvoie **qu'un seul** `model` par série. Le service
compare donc systématiquement, au backtest (RMSE), tous les modèles
demandés (`"auto"` = `{arima, prophet, ets}` en plus des modèles explicites
listés) et ne renvoie que le meilleur. La baseline (`snaive`/`naive`) est
toujours calculée séparément, indépendamment des modèles demandés, pour le
seul usage de `beats_baseline`/`reliability`.

### Réponse `200`

Identique au contrat (voir `CONTRAT.md`). Précisions :

- `frequency` : fréquence effective utilisée (détectée ou fournie).
- Historique régularisé : les trous du calendrier au pas de fréquence
  détecté/fourni sont comblés par `timetk::pad_by_time()`, puis les valeurs
  manquantes introduites sont interpolées linéairement
  (`stats::approx(..., rule = 2)`). Un avertissement `warnings[]` (au niveau
  série) précise le nombre de points comblés.
- Intervalles de confiance : **conformal split** via
  `modeltime::modeltime_forecast(..., conf_method = "conformal_split")`,
  calculés sur les résidus du jeu de test (holdout), un appel par niveau de
  `confidence_levels` demandé, fusionnés en colonnes `lower_XX`/`upper_XX`.
- Prévision finale : le modèle retenu est **ré-entraîné sur toute la série**
  (`modeltime::modeltime_refit()`) avant `modeltime_forecast(h = horizon)`.
  Sans ce ré-entraînement, `arima` et `ets` prévoient à partir de la fin de
  leurs données d'entraînement (ils ignorent les dates demandées) : la
  prévision renvoyée était celle du holdout, datée comme le futur. Les
  intervalles restent calibrés sur les résidus du holdout (conformal split),
  donc sur un modèle entraîné avec `holdout` points de moins que le modèle
  final. Test de régression : `tests/testthat/test-api_v1_forecast_horizon.R`.

### Découpage backtest / holdout

`holdout = max(2, min(horizon, floor(0.2 * n)))`, borné à `n - 3` points
d'entraînement minimum. `min_points = max(10, horizon + 1)` est exigé en
validation (400 sinon) pour garantir un découpage exploitable.

### Métriques et baseline

`modeltime::modeltime_accuracy()` (jeu de métriques par défaut) sur le jeu
de test : `mape`, `smape`, `mase`, `rmse`. **Unités** : `mape` et `smape`
sont des **fractions** (`0.08` = 8 %), pas des pourcentages —
`yardstick::mape()`/`yardstick::smape()` renvoient des points de
pourcentage (`8.0` pour 8 %), divisés par 100 avant de sortir dans la
réponse (`mase`, `rmse` restent des mesures d'échelle, non concernées).
`holdout_points` = nombre de points du jeu de test. Baseline : `snaive` si
la fréquence a une saisonnalité (`day` → 7, `week` → 52, `month` → 12,
`quarter` → 4), `naive` sinon (`year`, pas de cycle saisonnier annuel
exploitable).

### Règle `reliability`

Fonction `api_v1_reliability(model_mase, baseline_mase, holdout_points)` :

- `"unknown"` : métriques indisponibles ou `holdout_points < 2`.
- `"poor"` : le modèle ne bat pas la baseline (`model_mase >= baseline_mase`).
- `"good"` : le modèle bat la baseline **et** `holdout_points >= 6` **et**
  `model_mase / baseline_mase <= 0.8`.
- `"fair"` : le modèle bat la baseline mais ne remplit pas les deux
  conditions de `"good"`.

### Champ optionnel `calibration` (moteur Python uniquement)

Le moteur Python (`python/`) ajoute à chaque série un champ `calibration` ; l'API R ne le
renvoie pas, un client doit donc tolérer son absence (ou `null` pour une série en échec).

```json
"calibration": {
  "method": "split-conformal",
  "points": 14,
  "levels": {
    "80": {"calibrated": true, "pooled": false, "factor": 1.1237,
           "raw_coverage": 0.7143, "calibrated_coverage": 0.8571}
  }
}
```

- Score de chaque point du backtest : facteur d'élargissement de la bande du modèle qu'il aurait
  fallu pour contenir la valeur réelle. `factor` = quantile conforme d'ordre
  `ceil((n + 1) × niveau)` de ces scores ; les bandes renvoyées sont celles du modèle multipliées
  par ce facteur autour de la prévision (élargies si `factor > 1`, resserrées sinon).
- `pooled: true` : la série seule n'avait pas assez de points pour ce niveau (il en faut 4 pour
  80 %, 19 pour 95 %) ; ses scores ont été complétés par ceux des autres séries de la requête.
- `calibrated: false` : points insuffisants même ainsi ; bandes du modèle inchangées.
- `raw_coverage` : part des valeurs réelles du backtest dans les bandes brutes du modèle.
- `calibrated_coverage` : même mesure pour les bandes calibrées, estimée hors échantillon
  (chaque fenêtre recalibrée sans ses propres points) ; `null` s'il n'y a qu'une fenêtre.

### Champs optionnels `events` et `scenarios` (moteur Python uniquement)

Requête :

```json
"events": [
  {"name": "promo", "ranges": [{"start": "2025-12-01", "end": "2025-12-15"}], "dates": ["2024-12-05"],
   "groups": ["A"]}
],
"scenarios": [
  {"name": "Sans promo de décembre", "events": []},
  {"name": "Hausse de prix", "adjustments": [{"start": "2026-03-01", "end": "2026-06-30", "percent": -8}]}
]
```

- **Événement** : dates ou plages sur l'historique **et** l'horizon ; `groups` (facultatif) limite
  l'événement à certaines séries. Chaque période reçoit la part de ses jours couverte (0 à 1), utilisée
  comme covariable connue par Chronos-2, ARIMA (ARIMAX) et Prophet (régresseur). Avec des événements,
  `auto` = ensemble Chronos-2 + ARIMA (ETS et Theta ne les exploitent pas).
- Un événement **sans précédent dans l'historique** d'une série est ignoré pour elle (avertissement) :
  son effet ne peut pas être appris ; le simuler avec un ajustement.
- **Scénario** : `events` (facultatif) remplace les événements **futurs** (liste vide = aucun) ; les
  noms inconnus de `events` sont ignorés avec un avertissement. `adjustments` impose ensuite un
  effet explicite, `percent` (> -100) ou `add`, au prorata des jours couverts de chaque période.
  Les bandes du scénario reçoivent la même calibration que la prévision de base.
- Limites : 20 événements, 400 plages par événement, 5 scénarios, 20 ajustements par scénario.

Réponse, par série (seulement si la requête en contient) :

```json
"events": [{"name": "promo", "history_share": 0.11, "used": true}],
"scenarios": [{"name": "Sans promo de décembre", "forecast": [...],
               "difference": {"total": -1840, "percent": -6.2}}]
```

`history_share` : part des périodes de l'historique touchées par l'événement. `difference` :
total du scénario moins total de la prévision de base sur l'horizon.

### Erreurs

Même format que le contrat : `400/401/404/413`
`{"status":"error","errors":[{"field":..., "message":"..."}]}` — `field` vaut
`null` (JSON) quand l'erreur ne porte pas sur un champ précis (pas `{}` :
tous les endpoints sérialisent en `application/json;charset=utf-8` via
`reqres::format_json(auto_unbox = TRUE, null = "null")`, qui rend les `NULL`
R comme `null` JSON plutôt que le défaut de plumber2 — `{}` — voir
`plumber.R`). Messages en français, accentués (UTF-8), actionnables
(colonnes disponibles listées, modèles disponibles listés, etc.). Aucune
entrée invalide ne produit de `500` : toute erreur prévisible est
interceptée en amont de l'ajustement des modèles
(`api_v1_validate_request()`), et un échec d'ajustement isolé sur une série
(dans un cas multi-groupes) est reporté comme avertissement au niveau de
cette série plutôt que de faire échouer toute la requête.

## Logs

Une ligne JSON par requête sur `stdout` (`api_v1_log_request()`) :
`ts, id, route, status, duration_ms, n_rows, n_series, models`. Aucune
donnée utilisateur (pas de valeurs de `data`, pas d'IP, pas de token).

## Limite connue (contournement documenté)

`modeltime` enregistre ses implémentations de modèles personnalisés
(`naive_reg`, `arima_reg`, `exp_smoothing`, `prophet_reg`, ...) auprès de
`parsnip` d'une manière qui exige, en pratique, que le namespace `modeltime`
soit **attaché** (`library(modeltime)`) et pas seulement chargé via
`modeltime::...` : sans cela, `parsnip::fit()` échoue avec
`could not find function '..._fit_impl'`. Vérifié par reproduction directe
(appel identique avec et sans `library(modeltime)` préalable). `entrypoint.R`
et `tests/testthat/setup.R` attachent donc explicitement `modeltime` et
`parsnip` (y compris dans les daemons `mirai` via `mirai::everywhere()`).
Ce comportement affecte toutes les fonctions `th2_*_engine()` du package,
pas seulement le code de l'API v1.
