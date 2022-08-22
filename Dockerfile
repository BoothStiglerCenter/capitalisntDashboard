FROM rocker/shiny
RUN install2.r rsconnect rdrop2 cyphr sodiumf
WORKDIR /app

COPY capitalisntDashboardApp /app
COPY deployApp.R /app
COPY HelloWorld.R /app

# CMD Rscript HelloWorld.R
CMD Rscript deployApp.R


