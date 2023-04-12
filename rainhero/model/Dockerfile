FROM rstudio/plumber

WORKDIR /app

COPY . .

RUN Rscript install_packages.R
EXPOSE 8000

ENTRYPOINT ["Rscript", "run_api.R"]
