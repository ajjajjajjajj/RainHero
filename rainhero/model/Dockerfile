FROM rstudio/plumber

WORKDIR /app

COPY . .

RUN Rscript install_packages.R

ENTRYPOINT ["Rscript", "run_api.R"]
