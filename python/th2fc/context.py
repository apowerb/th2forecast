"""Contexte métier d'une prévision : événements datés et scénarios « et si ».

Un événement (promotion, fermeture, jour férié…) devient une covariable : pour chaque période de
la série, la part de ses jours couverte par l'événement (0 à 1). Le modèle apprend son effet sur
l'historique ; il faut donc que l'événement y ait des précédents. Un scénario remplace les
événements futurs et/ou impose des ajustements explicites (en % ou en valeur) sur des dates.
"""
from __future__ import annotations

from dataclasses import dataclass, field
from datetime import date, timedelta

import numpy as np

from . import contract as c

MAX_EVENTS = 20
MAX_RANGES = 400
MAX_SCENARIOS = 5
MAX_ADJUSTMENTS = 20


@dataclass
class Event:
    name: str
    ranges: list[tuple[date, date]]
    groups: set[str] | None = None  # None : toutes les séries


@dataclass
class Adjustment:
    start: date
    end: date
    percent: float | None = None
    add: float | None = None


@dataclass
class Scenario:
    name: str
    events: list[Event] | None  # None : événements futurs de la prévision de base
    adjustments: list[Adjustment] = field(default_factory=list)


def _date(v, where: str, errors: list) -> date | None:
    d = c._parse_date(v if isinstance(v, str) else None)
    if d is None:
        errors.append("%s : date '%s' invalide (format attendu YYYY-MM-DD)." % (where, v))
    return d


def _ranges(item: dict, where: str, errors: list) -> list[tuple[date, date]]:
    out, before = [], len(errors)
    for d in item.get("dates") or []:
        x = _date(d, where, errors)
        if x:
            out.append((x, x))
    for r in item.get("ranges") or []:
        if not isinstance(r, dict):
            errors.append("%s : chaque plage doit être un objet {start, end}." % where)
            continue
        a, b = _date(r.get("start"), where, errors), _date(r.get("end", r.get("start")), where, errors)
        if a and b:
            if b < a:
                errors.append("%s : plage %s → %s à l'envers." % (where, a, b))
            else:
                out.append((a, b))
    if not out and len(errors) == before:
        errors.append("%s : au moins une date ou une plage est requise." % where)
    if len(out) > MAX_RANGES:
        errors.append("%s : %d plages, au plus %d." % (where, len(out), MAX_RANGES))
    return out


def parse_events(raw, errors: list, where: str = "events") -> list[Event]:
    if raw is None:
        return []
    if not isinstance(raw, list):
        errors.append("'%s' doit être une liste d'événements." % where)
        return []
    if len(raw) > MAX_EVENTS:
        errors.append("'%s' : %d événements, au plus %d." % (where, len(raw), MAX_EVENTS))
        return []
    events, seen = [], set()
    for i, item in enumerate(raw):
        label = "%s[%d]" % (where, i)
        if not isinstance(item, dict) or not isinstance(item.get("name"), str) or not item["name"].strip():
            errors.append("%s : un nom ('name') est requis." % label)
            continue
        name = item["name"].strip()
        if name in seen:
            errors.append("%s : l'événement '%s' est déclaré deux fois ; regroupez ses dates." % (label, name))
            continue
        seen.add(name)
        groups = item.get("groups")
        events.append(Event(name, _ranges(item, "%s ('%s')" % (label, name), errors),
                            set(map(str, groups)) if isinstance(groups, list) and groups else None))
    return events


def parse_scenarios(raw, base_names: set[str], errors: list, warnings: list) -> list[Scenario]:
    if raw is None:
        return []
    if not isinstance(raw, list) or len(raw) > MAX_SCENARIOS:
        errors.append("'scenarios' doit être une liste d'au plus %d scénarios." % MAX_SCENARIOS)
        return []
    out = []
    for i, item in enumerate(raw):
        label = "scenarios[%d]" % i
        if not isinstance(item, dict) or not isinstance(item.get("name"), str) or not item["name"].strip():
            errors.append("%s : un nom ('name') est requis." % label)
            continue
        events = parse_events(item["events"], errors, label + ".events") if "events" in item else None
        for e in events or []:
            if e.name not in base_names:
                warnings.append("Scénario '%s' : l'événement '%s' n'existe pas dans l'historique déclaré ('events') ; "
                                "son effet ne peut pas être appris, utilisez un ajustement." % (item["name"], e.name))
        events = [e for e in events if e.name in base_names] if events is not None else None
        adjustments = []
        raw_adj = item.get("adjustments") or []
        if not isinstance(raw_adj, list) or len(raw_adj) > MAX_ADJUSTMENTS:
            errors.append("%s : 'adjustments' doit être une liste d'au plus %d ajustements." % (label, MAX_ADJUSTMENTS))
            raw_adj = []
        for j, a in enumerate(raw_adj):
            where = "%s.adjustments[%d]" % (label, j)
            if not isinstance(a, dict):
                errors.append("%s : objet {start, end, percent | add} attendu." % where)
                continue
            start, end = _date(a.get("start"), where, errors), _date(a.get("end", a.get("start")), where, errors)
            pct, add = a.get("percent"), a.get("add")
            numeric = [v for v in (pct, add) if isinstance(v, (int, float)) and not isinstance(v, bool)]
            if len(numeric) != 1 or (pct is not None and add is not None):
                errors.append("%s : indiquez soit 'percent' (ex. -20), soit 'add' (ex. 150)." % where)
                continue
            if pct is not None and pct <= -100:
                errors.append("%s : 'percent' doit être supérieur à -100." % where)
                continue
            if start and end:
                adjustments.append(Adjustment(start, end, float(pct) if pct is not None else None,
                                              float(add) if add is not None else None))
        out.append(Scenario(item["name"].strip(), events, adjustments))
    return out


def _period_end(d: date, frequency: str) -> date:
    return c.step(d, frequency, 1) - timedelta(days=1)


def coverage(periods: list[date], frequency: str, ranges: list[tuple[date, date]]) -> np.ndarray:
    """Part des jours de chaque période couverte par au moins une plage."""
    out = np.zeros(len(periods))
    for k, p in enumerate(periods):
        end = _period_end(p, frequency)
        days = (end - p).days + 1
        covered = set()
        for a, b in ranges:
            lo, hi = max(a, p), min(b, end)
            if lo <= hi:
                covered.update(range((lo - p).days, (hi - p).days + 1))
        out[k] = len(covered) / days
    return out


def matrix(events: list[Event], group, periods: list[date], frequency: str) -> tuple[list[str], np.ndarray]:
    """Covariables (périodes × événements) des événements qui s'appliquent à la série `group`."""
    names, cols = [], []
    for e in events:
        if e.groups is None or (group is not None and str(group) in e.groups):
            names.append(e.name)
            cols.append(coverage(periods, frequency, e.ranges))
    return names, (np.column_stack(cols) if cols else np.zeros((len(periods), 0)))


def adjust(point, bounds: dict, adjustments: list[Adjustment], periods: list[date], frequency: str):
    """Applique les ajustements explicites, au prorata des jours couverts de chaque période."""
    point = np.array(point, dtype=float)
    bounds = {lv: (np.array(lo, dtype=float), np.array(hi, dtype=float)) for lv, (lo, hi) in bounds.items()}
    for a in adjustments:
        f = coverage(periods, frequency, [(a.start, a.end)])
        if a.percent is not None:
            m = 1 + f * a.percent / 100
            point = point * m
            bounds = {lv: (lo * m, hi * m) for lv, (lo, hi) in bounds.items()}
        else:
            s = f * a.add
            point = point + s
            bounds = {lv: (lo + s, hi + s) for lv, (lo, hi) in bounds.items()}
    return point, bounds
