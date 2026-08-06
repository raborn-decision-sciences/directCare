FROM directcare-base:latest

COPY directCareAuth /tmp/directCareAuth
RUN R -e "install.packages('/tmp/directCareAuth', repos = NULL, type = 'source')" \
    && rm -rf /tmp/directCareAuth

COPY directCareForecastR /tmp/directCareForecastR
RUN R -e "install.packages('/tmp/directCareForecastR', repos = NULL, type = 'source')" \
    && rm -rf /tmp/directCareForecastR

COPY directCareScenarios /tmp/directCareScenarios
RUN R -e "install.packages('/tmp/directCareScenarios', repos = NULL, type = 'source')" \
    && rm -rf /tmp/directCareScenarios

COPY directCareBilling /tmp/directCareBilling
RUN R -e "install.packages('/tmp/directCareBilling', repos = NULL, type = 'source')" \
    && rm -rf /tmp/directCareBilling

COPY directCareAnalytics /app
WORKDIR /app
RUN R -e "install.packages('.', repos = NULL, type = 'source')"

USER appuser

EXPOSE 3838
CMD ["R", "-e", "options('shiny.port'=3838,'shiny.host'='0.0.0.0'); directCareAnalytics::run_app()"]
