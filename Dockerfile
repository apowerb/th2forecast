########################################################################
# Etage "builder" : compile th2forecast et ses dependances R depuis les
# sources (toolchain complete, en-tetes -dev, compilateurs). Cet etage
# n'est PAS expedie : seule sa bibliotheque R compilee est copiee dans
# l'etage "runtime" ci-dessous.
########################################################################
FROM rocker/r-ver@sha256:c3f39b365d1077fe24f8e9ab2742e352b6d3950897f51af1624a5bb5550c21c0 AS builder

RUN apt-get update && apt-get upgrade -y && apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev libfribidi-dev libharfbuzz-dev libicu-dev libpng-dev \
      libssl-dev libtiff-dev libv8-dev libxml2-dev make pandoc zlib1g-dev curl \
      cmake libfontconfig1-dev libfreetype6-dev libfreetype-dev libjpeg-dev \
      libsodium-dev libx11-dev xz-utils libuv1-dev libnode-dev ca-certificates \
    && rm -rf /var/lib/apt/lists/*

RUN curl -fsSL https://raw.githubusercontent.com/nbafrank/uvr/v0.4.6/install.sh \
      | UVR_VERSION=v0.4.6 UVR_INSTALL_DIR=/usr/local/bin sh

RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/ \
    && echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" \
      | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site

WORKDIR /build_zone

# Couche de dependances (cachee tant que le manifeste ne change pas).
COPY uvr.toml uvr.lock ./
RUN uvr sync --frozen

# Code source (invalide le cache a partir d'ici uniquement).
COPY . .

# th2forecast est installe depuis les sources LOCALES de ce depot, pas depuis
# GitHub : voir NEWS.md ("bug corrige : dependance auto-referentielle uvr.toml").
RUN Rscript -e "install.packages('.', repos = NULL, type = 'source', INSTALL_opts = c('--no-multiarch', '--no-docs', '--no-build-vignettes'))"

########################################################################
# Etage "runtime" : image expediee. Aucun compilateur, aucun en-tete -dev,
# aucun outil de build — seulement R, les bibliotheques partagees dont les
# paquets compiles ont besoin au chargement, et la bibliotheque R deja
# compilee (copiee depuis "builder"). Cela supprime la quasi-totalite des
# CVE du scan de reference (dominees par linux-libc-dev, apporte par la
# chaine de compilation et absent ici).
########################################################################
FROM rocker/r-ver@sha256:c3f39b365d1077fe24f8e9ab2742e352b6d3950897f51af1624a5bb5550c21c0 AS runtime

# L'image de base rocker/r-ver embarque de facto un toolchain de compilation
# complet (gcc/g++/cpp/binutils/libc6-dev/linux-libc-dev) pour permettre
# `install.packages(type = "source")`. C'est la source de la quasi-totalite
# des CVE CRITICAL du scan de reference (linux-libc-dev). Le runtime n'en a
# pas besoin (les paquets R sont deja compiles et copies depuis "builder") :
# on le purge explicitement, puis on ne reinstalle que les bibliotheques
# partagees (non -dev) necessaires au chargement des paquets compiles.
RUN apt-get update && apt-get upgrade -y \
    && apt-get purge -y --autoremove \
      binutils binutils-common binutils-x86-64-linux-gnu libbinutils \
      cpp cpp-13 cpp-x86-64-linux-gnu cpp-13-x86-64-linux-gnu \
      gcc gcc-13 gcc-x86-64-linux-gnu gcc-13-x86-64-linux-gnu \
      g++ g++-13 g++-x86-64-linux-gnu g++-13-x86-64-linux-gnu \
      libc6-dev linux-libc-dev libgcc-13-dev \
    && apt-get install -y --no-install-recommends \
      libcurl4t64 libfribidi0 libharfbuzz0b libicu74 libpng16-16t64 \
      libssl3t64 libtiff6 libxml2 libfontconfig1 libfreetype6 \
      libjpeg-turbo8 libsodium23 libx11-6 libuv1t64 ca-certificates curl \
      libgomp1 libquadmath0 libwebpmux3 \
    && rm -rf /var/lib/apt/lists/* \
    && useradd --create-home --home-dir /home/appuser --shell /usr/sbin/nologin --uid 1000 appuser

WORKDIR /build_zone

# Bibliotheque R compilee par l'etage "builder" (paquets uvr + th2forecast).
COPY --from=builder --chown=appuser:appuser /build_zone/.uvr/library /build_zone/.uvr/library
COPY --from=builder --chown=appuser:appuser /build_zone/.Rprofile /build_zone/.Rprofile

# Code applicatif necessaire a l'execution : point d'entree, API plumber2,
# et suite testthat (le smoke CI lance les tests DANS l'image, voir
# .github/workflows/docker-build.yml).
COPY --chown=appuser:appuser entrypoint.R plumber.R ./
COPY --chown=appuser:appuser tests/ ./tests/

EXPOSE 8000
ENV PORT=8000
ENV TH2FORECAST_WORKERS=2

USER appuser

HEALTHCHECK --interval=30s --timeout=5s --start-period=20s --retries=3 \
  CMD curl -fsS "http://127.0.0.1:${PORT}/health" || exit 1

CMD ["Rscript", "entrypoint.R"]
