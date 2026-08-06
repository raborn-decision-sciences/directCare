# Deliberately its own base, not `FROM directcare-base:latest` -- that
# image bundles shiny/bslib/DT/ggplot2/cicerone/mirai/typst and everything
# else both Shiny apps need, none of which this single-route webhook
# receiver touches. See STRIPE_BILLING.md Part 4.
FROM rocker/r-ver:4.4.0

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libpq-dev \
    ca-certificates \
    # zlib1g-dev -- httpuv (plumber's HTTP server, via Rook). libsodium-dev
    # -- sodium, a plumber dependency (its request-body digest helpers).
    # Both come "for free" inside directcare-base:latest's rocker/r-ver
    # image via other packages' transitive apt deps already installed
    # there; this Dockerfile starts from bare rocker/r-ver, so needs them
    # explicit -- confirmed live: install.packages() failed on both with
    # "fatal error: zlib.h/sodium.h: No such file or directory" without them.
    zlib1g-dev \
    libsodium-dev \
    && rm -rf /var/lib/apt/lists/*

# Same pinned-snapshot rationale as docker/base.Dockerfile -- a from-source
# CRAN compile months from now shouldn't silently pick up a broken/
# vulnerable release of any of these. Bump deliberately, not automatically.
ARG PPM_SNAPSHOT_DATE=2026-07-19
RUN R -e "install.packages(c( \
    'plumber', 'DBI', 'RPostgres', 'httr2', 'openssl', 'jsonlite', 'bcrypt' \
  ), repos = 'https://packagemanager.posit.co/cran/__linux__/bookworm/${PPM_SNAPSHOT_DATE}')"

# Same non-root convention as docker/base.Dockerfile (same UID, so the
# same host-side `chown 1000:1000` on Docker secret files covers this
# service too -- see secrets/README.md).
RUN useradd --uid 1000 --create-home --shell /bin/bash appuser

COPY directCareAuth /tmp/directCareAuth
RUN R -e "install.packages('/tmp/directCareAuth', repos = NULL, type = 'source')" \
    && rm -rf /tmp/directCareAuth

COPY directCareBilling /tmp/directCareBilling
RUN R -e "install.packages('/tmp/directCareBilling', repos = NULL, type = 'source')" \
    && rm -rf /tmp/directCareBilling

COPY billing-service/plumber.R /app/plumber.R
WORKDIR /app

USER appuser

EXPOSE 8000
CMD ["R", "-e", "pr <- plumber::plumb('plumber.R'); pr$run(host = '0.0.0.0', port = 8000)"]
