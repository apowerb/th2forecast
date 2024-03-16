FROM rocker/shiny-verse:4.3.2
RUN apt-get update && apt-get install -y  libcurl4-openssl-dev libfribidi-dev libharfbuzz-dev libicu-dev libpng-dev libssl-dev libtiff-dev libv8-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.4")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.25")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.45")'
RUN Rscript -e 'remotes::install_version("tidymodels",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("tidyverse",upgrade="never", version = "2.0.0")'
RUN Rscript -e 'remotes::install_version("prophet",upgrade="never", version = "1.0")'
RUN Rscript -e 'remotes::install_version("janitor",upgrade="never", version = "2.2.0")'
RUN Rscript -e 'remotes::install_version("timetk",upgrade="never", version = "2.9.0")'
RUN Rscript -e 'remotes::install_version("naniar",upgrade="never", version = NA)'
RUN Rscript -e 'remotes::install_version("modeltime",upgrade="never", version = NA)'
RUN Rscript -e 'remotes::install_version("caret",upgrade="never", version = NA)'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN R -e 'devtools::test(stop_on_failure  = TRUE)'
RUN rm -rf /build_zone
