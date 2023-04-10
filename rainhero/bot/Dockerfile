FROM rocker/rstudio

WORKDIR /app

COPY . .

RUN Rscript install_required_packages.R

CMD ["Rscript", "bot.R"]

