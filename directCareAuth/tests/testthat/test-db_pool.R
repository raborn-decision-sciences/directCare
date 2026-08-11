# Unit tests for db_pool()/db_checkout()/db_release()/db_pool_close(). All
# pool/DBI calls are mocked -- these never touch a real Postgres instance.
# .db_pool_env is this package's own internal singleton (db_pool.R); every
# test resets it via withr::defer() so pool state never leaks between
# tests regardless of run order.

local_reset_pool_env <- function() {
  withr::defer(.db_pool_env$pool <- NULL, envir = parent.frame())
  .db_pool_env$pool <- NULL
}

test_that("db_pool creates the pool once and memoizes it across calls", {
  local_reset_pool_env()

  call_count <- 0L
  local_mocked_bindings(
    dbPool = function(drv, ...) {
      call_count <<- call_count + 1L
      structure(list(), class = "mock_pool")
    },
    .package = "pool"
  )
  local_mocked_bindings(Postgres = function() "postgres-driver", .package = "RPostgres")

  p1 <- db_pool()
  p2 <- db_pool()

  expect_equal(call_count, 1L)
  expect_identical(p1, p2)
  expect_s3_class(p1, "mock_pool")
})

test_that("db_pool passes the same connection args db_connect() resolves", {
  local_reset_pool_env()
  withr::local_envvar(
    DB_PASSWORD_FILE = NA,
    DB_PASSWORD = "env-secret-pw",
    DB_HOST = "test-host",
    DB_PORT = "5433",
    DB_NAME = "test-db",
    DB_USER = "test-user"
  )

  captured <- NULL
  local_mocked_bindings(
    dbPool = function(drv, ...) {
      captured <<- list(...)
      structure(list(), class = "mock_pool")
    },
    .package = "pool"
  )
  local_mocked_bindings(Postgres = function() "postgres-driver", .package = "RPostgres")

  db_pool()

  expect_equal(captured$password, "env-secret-pw")
  expect_equal(captured$host, "test-host")
  expect_equal(captured$port, 5433L)
  expect_equal(captured$dbname, "test-db")
  expect_equal(captured$user, "test-user")
  expect_equal(captured$sslmode, "disable")
  expect_equal(captured$minSize, 1)
  expect_equal(captured$maxSize, 5)
})

test_that("db_checkout checks out a connection from the shared pool", {
  local_reset_pool_env()

  local_mocked_bindings(dbPool = function(drv, ...) structure(list(), class = "mock_pool"), .package = "pool")
  local_mocked_bindings(Postgres = function() "postgres-driver", .package = "RPostgres")

  captured_pool <- NULL
  local_mocked_bindings(
    poolCheckout = function(pool) {
      captured_pool <<- pool
      structure(list(), class = "mock_con")
    },
    .package = "pool"
  )

  con <- db_checkout()

  expect_s3_class(con, "mock_con")
  expect_s3_class(captured_pool, "mock_pool")
})

test_that("db_release returns a connection to the pool", {
  released <- NULL
  local_mocked_bindings(
    poolReturn = function(object) released <<- object,
    .package = "pool"
  )

  con <- structure(list(), class = "mock_con")
  db_release(con)

  expect_identical(released, con)
})

test_that("db_pool_close is a no-op when no pool was ever created", {
  local_reset_pool_env()

  closed <- FALSE
  local_mocked_bindings(poolClose = function(pool) closed <<- TRUE, .package = "pool")

  expect_no_error(db_pool_close())
  expect_false(closed)
})

test_that("db_pool_close closes and clears the singleton, so a later db_pool() call creates a fresh one", {
  local_reset_pool_env()

  call_count <- 0L
  local_mocked_bindings(
    dbPool = function(drv, ...) {
      call_count <<- call_count + 1L
      structure(list(instance = call_count), class = "mock_pool")
    },
    .package = "pool"
  )
  local_mocked_bindings(Postgres = function() "postgres-driver", .package = "RPostgres")

  closed_pool <- NULL
  local_mocked_bindings(poolClose = function(pool) closed_pool <<- pool, .package = "pool")

  p1 <- db_pool()
  db_pool_close()
  p2 <- db_pool()

  expect_identical(closed_pool, p1)
  expect_equal(call_count, 2L)
  expect_false(identical(p1, p2))
})
