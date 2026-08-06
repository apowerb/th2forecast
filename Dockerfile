FROM rocker/r-ver:latest
RUN apt-get update && apt-get install -y libcurl4-openssl-dev libfribidi-dev libharfbuzz-dev libicu-dev libpng-dev libssl-dev libtiff-dev libv8-dev libxml2-dev make pandoc zlib1g-dev curl && rm -rf /var/lib/apt/lists/*
RUN curl -fsSL https://raw.githubusercontent.com/nbafrank/uvr/main/install.sh | UVR_INSTALL_DIR=/usr/local/bin sh
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN uvr sync
# RUN uvr run R -e 'devtools::test(stop_on_failure = TRUE)'
EXPOSE 8000
CMD ["uvr", "run", "serve_api.R "]
RUN rm -rf /build_zone
