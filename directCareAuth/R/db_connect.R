#' Resolve the DB password from a Docker secret file (DB_PASSWORD_FILE)
#' when present, falling back to a plain DB_PASSWORD env var for local dev.
#'
#' Shared by [db_connect()] and [db_pool()] so the two connection paths
#' can't drift on how the password is resolved.
#' @keywords internal
.db_password <- function() {
  password_file <- Sys.getenv("DB_PASSWORD_FILE", unset = NA)
  if (!is.na(password_file) && file.exists(password_file)) {
    trimws(readLines(password_file, warn = FALSE))
  } else {
    Sys.getenv("DB_PASSWORD", unset = "")
  }
}

#' Open a connection to the shared practices database
#'
#' Connection params come from env vars set in docker-compose.yml. See
#' [db_pool()]/[db_checkout()] for a pooled alternative -- most callers in
#' this codebase should prefer that over `db_connect()`.
#'
#' @return An open `DBI` connection. Caller is responsible for
#'   `DBI::dbDisconnect()`.
#' @export
db_connect <- function() {
  password <- .db_password()

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
