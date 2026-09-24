#!/usr/bin/env bash
# Smoke test end-to-end pour th2forecast API v1.
# Pre-requis : un conteneur th2forecast demarre et accessible.
#
# Variables d'environnement :
#   TH2FORECAST_BASE_URL   (defaut: http://127.0.0.1:18000)
#   TH2FORECAST_API_TOKEN  (defaut: test-token) - doit correspondre au jeton
#                           avec lequel le conteneur a ete demarre.
set -euo pipefail

BASE_URL="${TH2FORECAST_BASE_URL:-http://127.0.0.1:18000}"
TOKEN="${TH2FORECAST_API_TOKEN:-test-token}"

pass=0
fail=0

check() {
  local desc="$1"
  local ok="$2"
  if [ "$ok" = "1" ]; then
    echo "OK   - $desc"
    pass=$((pass + 1))
  else
    echo "FAIL - $desc"
    fail=$((fail + 1))
  fi
}

need() {
  command -v "$1" >/dev/null 2>&1 || { echo "outil requis manquant : $1"; exit 2; }
}
need curl
need python3

echo "== smoke test th2forecast ($BASE_URL) =="

# 1) /health
health_body=$(curl -s -o /tmp/th2fc_health.json -w '%{http_code}' "$BASE_URL/health")
health_status=$(python3 -c "import json;print(json.load(open('/tmp/th2fc_health.json')).get('status'))" 2>/dev/null || echo "")
check "GET /health -> 200" "$([ "$health_body" = "200" ] && echo 1 || echo 0)"
check "GET /health -> status UP" "$([ "$health_status" = "UP" ] && echo 1 || echo 0)"

# 2) POST /v1/forecast : serie mensuelle de 36 points, prophet, horizon 12
python3 - "$BASE_URL" "$TOKEN" <<'PYEOF' > /tmp/th2fc_forecast_out.json
import json, sys, urllib.request, datetime

base_url, token = sys.argv[1], sys.argv[2]

rows = []
d = datetime.date(2021, 1, 1)
for i in range(36):
    month = ((d.month - 1 + i) % 12) + 1
    year = d.year + (d.month - 1 + i) // 12
    rows.append({"date": f"{year:04d}-{month:02d}-01", "sales": 100 + i * 2})

payload = {
    "data": rows,
    "date_var": "date",
    "target_var": "sales",
    "horizon": 12,
    "models": ["prophet"],
    "confidence_levels": [0.8, 0.95],
}

req = urllib.request.Request(
    base_url + "/v1/forecast",
    data=json.dumps(payload).encode("utf-8"),
    headers={"Content-Type": "application/json", "Authorization": f"Bearer {token}"},
    method="POST",
)
try:
    with urllib.request.urlopen(req, timeout=120) as resp:
        print(json.dumps({"status_code": resp.status, "body": json.loads(resp.read())}))
except urllib.error.HTTPError as e:
    print(json.dumps({"status_code": e.code, "body": json.loads(e.read())}))
PYEOF

fc_status=$(python3 -c "import json;print(json.load(open('/tmp/th2fc_forecast_out.json'))['status_code'])")
fc_npoints=$(python3 -c "
import json
d = json.load(open('/tmp/th2fc_forecast_out.json'))['body']
s = d['series'][0]
print(len(s['forecast']))
")
fc_has_bounds=$(python3 -c "
import json
d = json.load(open('/tmp/th2fc_forecast_out.json'))['body']
p = d['series'][0]['forecast'][0]
print(1 if all(k in p for k in ('lower_80','upper_80','lower_95','upper_95')) else 0)
")

check "POST /v1/forecast (prophet, h=12) -> 200" "$([ "$fc_status" = "200" ] && echo 1 || echo 0)"
check "POST /v1/forecast -> exactement 12 points de prevision" "$([ "$fc_npoints" = "12" ] && echo 1 || echo 0)"
check "POST /v1/forecast -> bornes lower_XX/upper_XX presentes" "$fc_has_bounds"

# 3) Cas invalide -> 400 avec errors[0].field
inv_status=$(curl -s -o /tmp/th2fc_invalid.json -w '%{http_code}' -X POST "$BASE_URL/v1/forecast" \
  -H "Authorization: Bearer $TOKEN" -H "Content-Type: application/json" \
  -d '{"data":[{"dt":"2024-01-01","sales":1}],"date_var":"date","target_var":"sales","horizon":3}')
inv_field=$(python3 -c "import json;print(json.load(open('/tmp/th2fc_invalid.json'))['errors'][0]['field'])" 2>/dev/null || echo "")

check "POST /v1/forecast (colonne absente) -> 400" "$([ "$inv_status" = "400" ] && echo 1 || echo 0)"
check "POST /v1/forecast (colonne absente) -> errors[0].field = date_var" "$([ "$inv_field" = "date_var" ] && echo 1 || echo 0)"

# 4) Auth : 401 sans jeton quand un jeton est configure
auth_status=$(curl -s -o /dev/null -w '%{http_code}' -X POST "$BASE_URL/v1/forecast" \
  -H "Content-Type: application/json" \
  -d '{"data":[{"date":"2024-01-01","sales":1}],"date_var":"date","target_var":"sales","horizon":1}')
check "POST /v1/forecast sans jeton -> 401" "$([ "$auth_status" = "401" ] && echo 1 || echo 0)"

# 5) /v1/jobs : creation + polling jusqu'a succeeded/failed
job_status=$(curl -s -o /tmp/th2fc_job_create.json -w '%{http_code}' -X POST "$BASE_URL/v1/jobs" \
  -H "Authorization: Bearer $TOKEN" -H "Content-Type: application/json" \
  -d '{"data":[{"date":"2024-01-01","sales":10},{"date":"2024-02-01","sales":12},{"date":"2024-03-01","sales":13},{"date":"2024-04-01","sales":15},{"date":"2024-05-01","sales":16},{"date":"2024-06-01","sales":18},{"date":"2024-07-01","sales":19},{"date":"2024-08-01","sales":21},{"date":"2024-09-01","sales":22},{"date":"2024-10-01","sales":24},{"date":"2024-11-01","sales":25},{"date":"2024-12-01","sales":27}],"date_var":"date","target_var":"sales","horizon":2,"models":["naive"]}')
job_id=$(python3 -c "import json;print(json.load(open('/tmp/th2fc_job_create.json'))['job_id'])" 2>/dev/null || echo "")

check "POST /v1/jobs -> 202" "$([ "$job_status" = "202" ] && echo 1 || echo 0)"

job_final="unknown"
for _ in $(seq 1 30); do
  curl -s -o /tmp/th2fc_job_get.json "$BASE_URL/v1/jobs/$job_id" -H "Authorization: Bearer $TOKEN"
  job_final=$(python3 -c "import json;print(json.load(open('/tmp/th2fc_job_get.json'))['status'])" 2>/dev/null || echo "unknown")
  [ "$job_final" = "succeeded" ] || [ "$job_final" = "failed" ] && break
  sleep 1
done
check "GET /v1/jobs/{id} -> se termine (succeeded|failed)" "$([ "$job_final" = "succeeded" ] || [ "$job_final" = "failed" ] && echo 1 || echo 0)"

echo "== resultat : $pass reussi(s), $fail echoue(s) =="
[ "$fail" -eq 0 ]
