import json
import time

import pytest
from fastapi.testclient import TestClient

from conftest import monthly


@pytest.fixture
def client(monkeypatch):
    monkeypatch.setenv("TH2FORECAST_API_TOKEN", "t0k")
    from th2fc.app import app

    with TestClient(app) as cl:
        yield cl


AUTH = {"Authorization": "Bearer t0k"}


def test_health_sans_jeton(client):
    r = client.get("/health")
    assert r.status_code == 200 and r.json()["status"] == "UP"


def test_401_sans_jeton(client):
    r = client.post("/v1/forecast", json={})
    assert r.status_code == 401
    assert r.json()["errors"][0] == {"field": None, "message": (
        "Authentification requise : en-tête 'Authorization: Bearer <token>' manquant ou invalide.")}


def test_json_invalide_donne_400(client):
    r = client.post("/v1/forecast", content=b"{pas du json", headers={**AUTH, "Content-Type": "application/json"})
    assert r.status_code == 400
    assert r.json() == {"status": "error", "errors": [{"field": None, "message": "Corps de requête JSON manquant ou invalide."}]}


def test_field_null_et_pas_objet_vide(client):
    r = client.post("/v1/forecast", headers=AUTH, content=b"[]")
    assert '"field":null' in r.text.replace(" ", "")


def test_job_de_bout_en_bout(client):
    body = {"data": monthly(12, lambda i: 10 + i), "date_var": "date", "target_var": "sales", "horizon": 2, "models": ["naive"]}
    r = client.post("/v1/jobs", headers=AUTH, json=body)
    assert r.status_code == 202 and r.json()["status"] == "queued"
    job_id = r.json()["job_id"]
    for _ in range(60):
        j = client.get(f"/v1/jobs/{job_id}", headers=AUTH).json()
        if j["status"] in ("succeeded", "failed"):
            break
        time.sleep(0.5)
    assert j["status"] == "succeeded" and j["error"] is None
    assert len(j["result"]["series"][0]["forecast"]) == 2


def test_job_inconnu_404(client):
    r = client.get("/v1/jobs/nope", headers=AUTH)
    assert r.status_code == 404 and r.json()["errors"][0]["field"] == "job_id"


def test_job_invalide_refuse_sans_creer_de_job(client):
    r = client.post("/v1/jobs", headers=AUTH, json={"data": [], "horizon": 1})
    assert r.status_code == 400


def test_une_ligne_de_log_sans_donnees(client, capsys):
    client.post("/v1/forecast", headers=AUTH, json={"data": monthly(12, float), "date_var": "date",
                                                    "target_var": "sales", "horizon": 1, "models": ["naive"]})
    lines = [json.loads(x) for x in capsys.readouterr().out.splitlines() if x.startswith("{")]
    entry = lines[-1]
    assert set(entry) == {"ts", "id", "route", "status", "duration_ms", "n_rows", "n_series", "models"}
    assert (entry["route"], entry["status"], entry["n_rows"], entry["models"]) == ("POST /v1/forecast", 200, 12, ["naive"])
