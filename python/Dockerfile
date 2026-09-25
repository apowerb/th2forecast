# Service Python th2forecast : contrat v1 identique à l'API R (voir README.md).
FROM python:3.12-slim

ENV PYTHONDONTWRITEBYTECODE=1 PYTHONUNBUFFERED=1 PIP_NO_CACHE_DIR=1 \
    HF_HUB_OFFLINE=1 TRANSFORMERS_OFFLINE=1 \
    TH2FORECAST_CHRONOS_PATH=/opt/models/chronos-2

WORKDIR /app

# torch CPU seul (l'index par défaut tirerait CUDA, plusieurs Go inutiles).
RUN pip install --root-user-action=ignore --index-url https://download.pytorch.org/whl/cpu torch==2.8.0
COPY requirements.txt .
RUN pip install --root-user-action=ignore -r requirements.txt

# Poids Chronos-2 figés dans l'image : aucun accès réseau au démarrage.
ARG CHRONOS_REVISION=29ec3766d36d6f73f0696f85560a422f50e8498c
RUN HF_HUB_OFFLINE=0 python -c "from huggingface_hub import snapshot_download; \
snapshot_download('amazon/chronos-2', revision='${CHRONOS_REVISION}', local_dir='/opt/models/chronos-2')"

COPY th2fc ./th2fc
COPY pytest.ini .
COPY tests ./tests

RUN useradd --uid 10001 --create-home app
USER app

EXPOSE 8000
HEALTHCHECK --interval=30s --timeout=5s --start-period=60s --retries=3 \
  CMD python -c "import urllib.request; urllib.request.urlopen('http://127.0.0.1:8000/health', timeout=4)"
CMD ["uvicorn", "th2fc.app:app", "--host", "0.0.0.0", "--port", "8000", "--no-access-log"]
