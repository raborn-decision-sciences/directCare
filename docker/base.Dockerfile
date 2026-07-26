FROM rocker/r-ver:4.4.0

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libuv1-dev \
    libpq-dev \
    curl \
    ca-certificates \
    xz-utils \
    && rm -rf /var/lib/apt/lists/*

# typst CLI -- both DCA and Planner shell out to `typst` (via Sys.which()) to
# render PDF reports. Debian bookworm has no typst apt package, so install
# the official prebuilt release binary. Bump TYPST_VERSION as needed.
ARG TYPST_VERSION=0.12.0
RUN curl -fsSL "https://github.com/typst/typst/releases/download/v${TYPST_VERSION}/typst-x86_64-unknown-linux-musl.tar.xz" \
    | tar -xJ -C /usr/local/bin --strip-components=1 "typst-x86_64-unknown-linux-musl/typst" \
    && typst --version

# Shared R packages used by both apps (and their internal core packages).
# Posit Package Manager serves prebuilt binaries for Debian bookworm (the
# rocker/r-ver:4.4.0 base OS), avoiding a from-source CRAN compile that
# would otherwise take 10+ minutes per rebuild. No renv.lock in this repo,
# so versions float to whatever the snapshot serves at build time.
RUN R -e "install.packages(c( \
    'shiny', 'golem', 'bslib', 'bsicons', 'config', 'DT', 'ggplot2', \
    'scales', 'thematic', 'dplyr', 'tibble', 'tidyr', 'rlang', \
    'jsonlite', 'htmltools', 'pkgload', 'forecast', 'lubridate', 'readr', \
    'shinymanager', 'DBI', 'RPostgres', 'bcrypt', 'remotes', \
    'cicerone', 'later', 'httr2', 'openssl' \
  ), repos = 'https://packagemanager.posit.co/cran/__linux__/bookworm/latest')"

# Non-root user for running the app (created here, switched to at the end of
# each app's own Dockerfile, after all root-owned package installs). Package
# installs and app source stay root-owned/read-only at runtime -- both apps
# only ever write to R's tempdir() (world-writable /tmp), never to /app --
# so a non-root process needs no extra chown to run correctly.
RUN useradd --create-home --shell /bin/bash appuser
