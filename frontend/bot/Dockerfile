FROM rocker/tidyverse

RUN install2.r -s telegram.bot

WORKDIR /app

COPY . .

CMD ["Rscript", "bot.R"]

