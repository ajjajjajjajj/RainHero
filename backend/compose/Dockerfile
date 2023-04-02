# syntax=docker/dockerfile:1

FROM rocker/r-base:latest

RUN apt-get update && apt-get install -y \
    r-base \
    r-base-dev \
    libssl-dev \
    libcurl4-gnutls-dev

RUN R -e "install.packages('zoo', repos='https://cloud.r-project.org')"
RUN R -e "install.packages('sp', repos='https://cloud.r-project.org')"
RUN R -e "install.packages('sf', repos='https://cloud.r-project.org')"
RUN R -e "install.packages('gstat', repos='https://cloud.r-project.org')"
RUN R -e "install.packages('forecast', repos='https://cloud.r-project.org')"
RUN R -e "install.packages('xts', repos='https://cloud.r-project.org')"
RUN R -e "install.packages('tidyverse', repos='https://cloud.r-project.org')"
RUN R -e "install.packages('forecast', repos='https://cloud.r-project.org')"
RUN R -e "install.packages('classInt', repos='https://cloud.r-project.org')"
RUN R -e "install.packages('RColorBrewer', repos='https://cloud.r-project.org')"

#COPY requirements.txt requirements.txt
#RUN pip install -r requirements.txt

EXPOSE 5000
COPY . .
CMD ["R"]

