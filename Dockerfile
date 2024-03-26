FROM rocker/r-base:4.2.1

ARG QUARTO_VERSION="1.3.450"

FROM ghcr.io/quarto-dev/quarto:${QUARTO_VERSION} AS builder

Rscript -e 'pak::pkg_install("renv")'

# Install packages from renv
## Copy files created with renv
# RUN mkdir -p renv
# COPY .Rprofile .Rprofile
# COPY renv/activate.R renv/activate.R
# COPY renv/settings.json renv/settings.json

COPY . /app
WORKDIR /app
RUN Rscript -e "renv::restore()"

# RUN quarto render .

## Install packages
# RUN R -e "renv::restore()"