import pytest

from th2fc.env import env_int, env_str


def test_absent_ou_vide_retombe_sur_le_defaut(monkeypatch):
    # Compose pose `NOM: ${NOM:-}` comme une variable VIDE : elle doit valoir
    # « non réglée », pas planter le démarrage sur int('').
    monkeypatch.delenv("TH2FC_TEST_N", raising=False)
    assert env_int("TH2FC_TEST_N", 7) == 7
    for vide in ("", "   "):
        monkeypatch.setenv("TH2FC_TEST_N", vide)
        assert env_int("TH2FC_TEST_N", 7) == 7


def test_valeur_posee_est_lue(monkeypatch):
    monkeypatch.setenv("TH2FC_TEST_N", " 3 ")
    assert env_int("TH2FC_TEST_N", 7) == 3


def test_valeur_invalide_nomme_la_variable(monkeypatch):
    monkeypatch.setenv("TH2FC_TEST_N", "deux")
    with pytest.raises(ValueError, match="TH2FC_TEST_N"):
        env_int("TH2FC_TEST_N", 7)


def test_chaine_vide_retombe_sur_le_defaut(monkeypatch):
    monkeypatch.setenv("TH2FC_TEST_S", "")
    assert env_str("TH2FC_TEST_S", "1") == "1"
    monkeypatch.setenv("TH2FC_TEST_S", "0")
    assert env_str("TH2FC_TEST_S", "1") == "0"
