FROM rocker/r-ver:4.5
RUN apt-get update && apt-get install -y libcurl4-openssl-dev libfribidi-dev libharfbuzz-dev libicu-dev libpng-dev libssl-dev libtiff-dev libv8-dev libxml2-dev make pandoc zlib1g-dev curl cmake libfontconfig1-dev libfreetype6-dev libfreetype-dev libjpeg-dev libsodium-dev libx11-dev xz-utils libuv1-dev libnode-dev && rm -rf /var/lib/apt/lists/*
RUN curl -fsSL https://raw.githubusercontent.com/nbafrank/uvr/main/install.sh | UVR_INSTALL_DIR=/usr/local/bin sh
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
# ENV UVR_INSTALL_SYSREQS=1
RUN uvr sync
EXPOSE 8000
CMD ["Rscript", "-e", "plumber2::api('plumber.R') |> plumber2::api_run(port=8000, host='0.0.0.0')"]