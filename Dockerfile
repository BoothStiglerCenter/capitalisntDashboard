# IMAGE NAME: capitalisnt-dashboard-deploy
# FROM rocker/shiny
FROM renv-restore


RUN ls






COPY deployApp.R /app
CMD Rscript deployApp.R


