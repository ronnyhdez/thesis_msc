FROM rocker/r-ver:4.2.1

RUN apt-get update \
   && apt-get install -y \
   libxml2 \
   wget

# Download the latest version of Quarto
RUN wget https://github.com/quarto-dev/quarto-cli/releases/download/v1.3.450/quarto-1.3.450-linux-amd64.deb -O ~/quarto.deb

# Install the latest version of Quarto
RUN dpkg -i ~/quarto.deb

# Remove the installer
RUN rm ~/quarto.deb

RUN quarto install tinytex

RUN Rscript -e 'install.packages("renv")'

COPY . /app
WORKDIR /app
RUN Rscript -e "renv::restore()"

# RUN quarto render .

