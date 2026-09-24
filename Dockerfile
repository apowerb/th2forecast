FROM rocker/r-ver:4.5

RUN apt-get update && apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev libfribidi-dev libharfbuzz-dev libicu-dev libpng-dev \
      libssl-dev libtiff-dev libv8-dev libxml2-dev make pandoc zlib1g-dev curl \
      cmake libfontconfig1-dev libfreetype6-dev libfreetype-dev libjpeg-dev \
      libsodium-dev libx11-dev xz-utils libuv1-dev libnode-dev ca-certificates \
    && rm -rf /var/lib/apt/lists/*

RUN curl -fsSL https://raw.githubusercontent.com/nbafrank/uvr/v0.4.6/install.sh \
      | UVR_INSTALL_DIR=/usr/local/bin sh

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

EXPOSE 8000
ENV PORT=8000
ENV TH2FORECAST_WORKERS=2

CMD ["Rscript", "entrypoint.R"]
