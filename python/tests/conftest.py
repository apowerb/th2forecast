import os
from datetime import date

import numpy as np
import pytest

os.environ.setdefault("TH2FORECAST_PRELOAD", "0")

LIMITS = {"max_rows": 1000, "max_series": 10, "max_horizon": 366}


def monthly(n, f, start=(2022, 1), **extra):
    y0, m0 = start
    rows = []
    for i in range(n):
        m = m0 - 1 + i
        rows.append({"date": date(y0 + m // 12, m % 12 + 1, 1).isoformat(), "sales": f(i + 1), **extra})
    return rows


def truth(t):
    return 100 + 2 * t + 30 * np.sin(2 * np.pi * t / 12)


@pytest.fixture(scope="session")
def engine():
    from th2fc.engine import Engine

    return Engine()


@pytest.fixture
def noisy_truth_rows():
    rng = np.random.default_rng(1)
    return monthly(48, lambda t: float(truth(t) + rng.normal(0, 2)))  # float JSON, pas np.float64
