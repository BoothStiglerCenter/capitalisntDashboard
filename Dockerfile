FROM rocker/shiny
RUN apt-get update
# Need to install various core utilities to download prerequiste R libraries
RUN apt-get install -y \
    bash \
    libsodium-dev \
    libssl-dev \ 
    libxml2-dev
RUN install2.r rsconnect rdrop2 cyphr sodium renv --error --skipinstalled


WORKDIR /app
COPY capitalisntDashboardApp /app
COPY deployApp.R /app
COPY renv.lock /app

RUN R -e "renv::consent(provided=TRUE)"
RUN R -e "renv::restore()"

CMD Rscript deployApp.R


