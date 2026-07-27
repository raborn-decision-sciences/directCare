#' Open a connection to the shared practices database
#'
#' Connection params come from env vars set in docker-compose.yml. The
#' password is read from a Docker secret file (DB_PASSWORD_FILE) when
#' present, falling back to a plain DB_PASSWORD env var for local dev.
#'
#' @return An open `DBI` connection. Caller is responsible for
#'   `DBI::dbDisconnect()`.
#' @export
db_connect <- function() {
  password_file <- Sys.getenv("DB_PASSWORD_FILE", unset = NA)
  if (!is.na(password_file) && file.exists(password_file)) {
    password <- trimws(readLines(password_file, warn = FALSE))
  } else {
    password <- Sys.getenv("DB_PASSWORD", unset = "")
  }

  # Explicit rather than left to libpq's implicit default ("prefer").
  # "disable" matches current reality: the `db` container has no TLS cert
  # configured, so an opportunistic "prefer"/"require" would either
  # silently fall back to plaintext anyway or refuse to connect at all --
  # neither is better than being honest about it. This connection never
  # leaves the private Compose network (see docker-compose.yml's removed
  # host port publishing). Upgrading to real encryption-in-transit would
  # need Postgres server-side TLS (cert + `ssl = on` in postgresql.conf),
  # not just a client-side flag -- a larger change than this one.
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST", "db"),
    port = as.integer(Sys.getenv("DB_PORT", "5432")),
    dbname = Sys.getenv("DB_NAME", "directcare"),
    user = Sys.getenv("DB_USER", "directcare"),
    password = password,
    sslmode = "disable"
  )
}
