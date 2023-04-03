FROM rocker/r-base:latest

WORKDIR /docker

COPY install_packages.R install_packages.R
COPY WebScraping.R WebScraping.R
COPY getRainfall.R getRainfall.R
COPY getGrid.R getGrid.R
COPY plotWeatherStations.R plotWeatherStations.R
COPY predictions.R predictions.R

RUN Rscript install_packages.R
CMD Rscript WebScraping.R
CMD Rscript getRainfall.R
