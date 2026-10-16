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

# Même purge que les images apowerb, th2etl et th2pulse, sur la même base
# Debian trixie. perl-base (Essential) et util-linux/mount/login/bsdutils
# portent l'essentiel des avis HIGH de cette image (perl CVE-2026-9538 ;
# util-linux CVE-2026-76642, -78408, -78409, -78410, rattachés à chacun des
# paquets binaires de la source), sans correctif Debian publié. Le service est
# du Python pur (uvicorn) : il n'exécute ni perl, ni mount, login, su ou
# nsenter. La purge vient après useradd, qui appartient au paquet passwd et
# reste disponible.
#
# `apt-get upgrade` prend les correctifs de sécurité Debian publiés après la
# dernière image python:3.12-slim, au lieu d'attendre sa reconstruction.
RUN apt-get update \
    && apt-get upgrade -y \
    && apt-get purge -y --allow-remove-essential perl-base util-linux mount login bsdutils \
    && apt-get autoremove -y \
    && rm -rf /var/lib/apt/lists/*

# libuuid1 et ncurses (libncursesw6, ncurses-bin, ncurses-base) portent sept des
# onze avis HIGH restants -- util-linux CVE-2026-76642, -78408, -78409, -78410
# et ncurses CVE-2025-69720 --, sans correctif Debian trixie publié (Trivy
# 0.68.1, 16/10/2026). Le service n'en a pas besoin : seuls les modules `_uuid`
# et `_curses` de Python lient ces bibliothèques, et ils partent avec. `import
# uuid` prend alors son chemin pur Python ; `import curses` échoue en
# ImportError au lieu d'un .so orphelin.
#
# dpkg --purge plutôt qu'`apt-get purge --allow-remove-essential` : dpkg refuse
# de retirer un paquet dont un autre dépend, donc un consommateur ajouté plus
# tard fait échouer la construction ici au lieu d'une cascade silencieuse. Seuls
# ncurses-bin et ncurses-base sont Essential et demandent le forçage.
#
# Restent : libtinfo6 (bash, readline), libacl1 (coreutils, tar, passwd),
# libsystemd0 et libudev1 (apt, PAM) -- les quatre HIGH qui demeurent. Même
# purge que les images apowerb, th2etl et th2pulse.
RUN dpkg --purge libuuid1 libncursesw6 \
    && dpkg --purge --force-remove-essential ncurses-bin ncurses-base \
    && rm -f /usr/local/lib/python3.12/lib-dynload/_uuid.* \
             /usr/local/lib/python3.12/lib-dynload/_curses*

USER app

EXPOSE 8000
HEALTHCHECK --interval=30s --timeout=5s --start-period=60s --retries=3 \
  CMD python -c "import urllib.request; urllib.request.urlopen('http://127.0.0.1:8000/health', timeout=4)"
CMD ["uvicorn", "th2fc.app:app", "--host", "0.0.0.0", "--port", "8000", "--no-access-log"]
