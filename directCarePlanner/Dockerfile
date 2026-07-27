FROM directcare-base:latest

COPY directCareAuth /tmp/directCareAuth
RUN R -e "install.packages('/tmp/directCareAuth', repos = NULL, type = 'source')" \
    && rm -rf /tmp/directCareAuth

COPY directCareData /tmp/directCareData
RUN R -e "install.packages('/tmp/directCareData', repos = NULL, type = 'source')" \
    && rm -rf /tmp/directCareData

COPY directCarePlanR /tmp/directCarePlanR
RUN R -e "install.packages('/tmp/directCarePlanR', repos = NULL, type = 'source')" \
    && rm -rf /tmp/directCarePlanR

COPY directCareScenarios /tmp/directCareScenarios
RUN R -e "install.packages('/tmp/directCareScenarios', repos = NULL, type = 'source')" \
    && rm -rf /tmp/directCareScenarios

COPY directCarePlanner /app
WORKDIR /app
RUN R -e "install.packages('.', repos = NULL, type = 'source')"

USER appuser

EXPOSE 3838
CMD ["R", "-e", "options('shiny.port'=3838,'shiny.host'='0.0.0.0'); directCarePlanner::run_app()"]
