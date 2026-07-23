FROM directcare-base:latest

COPY directCareData /tmp/directCareData
RUN R -e "install.packages('/tmp/directCareData', repos = NULL, type = 'source')" \
    && rm -rf /tmp/directCareData

COPY directCarePlanR /tmp/directCarePlanR
RUN R -e "install.packages('/tmp/directCarePlanR', repos = NULL, type = 'source')" \
    && rm -rf /tmp/directCarePlanR

COPY directCarePlanner /app
WORKDIR /app
RUN R -e "install.packages('.', repos = NULL, type = 'source')"

EXPOSE 3838
CMD ["R", "-e", "options('shiny.port'=3838,'shiny.host'='0.0.0.0'); directCarePlanner::run_app()"]
