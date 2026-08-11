# Package-level singleton pool, lazily created on first use and shared for
# the life of the R process. One pool per process is correct here: each
# Shiny app / plumber service in this codebase runs as a single long-lived
# R process per Docker container (see docker-compose.yml), so a
# package-level singleton is equivalent to "one pool per app instance"
# without needing to thread a pool object through every function call.
.db_pool_env <- new.env(parent = emptyenv())

#' Get (creating if needed) the shared connection pool
#'
#' Uses the same connection params as [db_connect()]. Created lazily on
#' first call and memoized for the life of the R process -- call this
#' explicitly once at app startup (see both apps' `run_app.R`) to pay the
#' pool's initial-connection cost before the first real request, rather
#' than on whichever request happens to arrive first.
#'
#' `minSize`/`maxSize` are deliberately modest Phase-3 starting values, not
#' yet tuned against real traffic -- mirrors `mirai::daemons(n = 2)`'s own
#' "no CPU/memory limit set in docker-compose.yml" reasoning in both
#' apps' `run_app.R`.
#'
#' @return A `pool::Pool` object.
#' @export
db_pool <- function() {
  if (is.null(.db_pool_env$pool)) {
    .db_pool_env$pool <- pool::dbPool(
      RPostgres::Postgres(),
      host = Sys.getenv("DB_HOST", "db"),
      port = as.integer(Sys.getenv("DB_PORT", "5432")),
      dbname = Sys.getenv("DB_NAME", "directcare"),
      user = Sys.getenv("DB_USER", "directcare"),
      password = .db_password(),
      sslmode = "disable",
      minSize = 1,
      maxSize = 5
    )
  }
  .db_pool_env$pool
}

#' Check out a pooled connection
#'
#' Pair with [db_release()] -- never `DBI::dbDisconnect()` directly, which
#' destroys the underlying connection instead of returning it to the pool
#' (silently degrading back to a fresh handshake per request, one
#' connection at a time). Typical use:
#' ```r
#' con <- directCareAuth::db_checkout()
#' on.exit(directCareAuth::db_release(con), add = TRUE)
#' ```
#' @return An open `DBI` connection borrowed from the shared pool.
#' @export
db_checkout <- function() {
  pool::poolCheckout(db_pool())
}

#' Return a connection checked out via [db_checkout()] to the pool
#' @param con A connection returned by [db_checkout()].
#' @export
db_release <- function(con) {
  pool::poolReturn(con)
}

#' Close the shared connection pool
#'
#' Call from an app's `onStop()` for a clean shutdown. Safe to call even
#' if the pool was never created (e.g. a process that never handled a
#' request) -- a no-op in that case.
#' @export
db_pool_close <- function() {
  if (!is.null(.db_pool_env$pool)) {
    pool::poolClose(.db_pool_env$pool)
    .db_pool_env$pool <- NULL
  }
  invisible(NULL)
}
