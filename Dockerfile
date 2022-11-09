# IMAGE NAME: capitalisnt-dashboard-deploy
# FROM rocker/shiny
FROM renv-restore


RUN ls


COPY capitalisntDashboardApp /app
COPY deployApp.R /app
CMD Rscript deployApp.R


