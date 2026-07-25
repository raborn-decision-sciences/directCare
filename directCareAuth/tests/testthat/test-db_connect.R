# Unit tests for db_connect(). All DB calls are mocked -- these never touch
# a real Postgres instance.

test_that("db_connect prefers DB_PASSWORD_FILE over DB_PASSWORD when set", {
  pw_file <- withr::local_tempfile(lines = "file-secret-pw")
  withr::local_envvar(
    DB_PASSWORD_FILE = pw_file,
    DB_PASSWORD = "should-be-ignored",
    DB_HOST = "test-host",
    DB_PORT = "5433",
    DB_NAME = "test-db",
    DB_USER = "test-user"
  )

  captured <- NULL
  local_mocked_bindings(
    dbConnect = function(drv, ...) {
      captured <<- list(...)
      structure(list(), class = "mock_con")
    },
    .package = "DBI"
  )
  local_mocked_bindings(Postgres = function() "postgres-driver", .package = "RPostgres")

  db_connect()

  expect_equal(captured$password, "file-secret-pw")
  expect_equal(captured$host, "test-host")
  expect_equal(captured$port, 5433L)
  expect_equal(captured$dbname, "test-db")
  expect_equal(captured$user, "test-user")
})

test_that("db_connect falls back to DB_PASSWORD and defaults when unset", {
  withr::local_envvar(
    DB_PASSWORD_FILE = NA,
    DB_PASSWORD = "env-secret-pw",
    DB_HOST = NA,
    DB_PORT = NA,
    DB_NAME = NA,
    DB_USER = NA
  )

  captured <- NULL
  local_mocked_bindings(
    dbConnect = function(drv, ...) {
      captured <<- list(...)
      structure(list(), class = "mock_con")
    },
    .package = "DBI"
  )
  local_mocked_bindings(Postgres = function() "postgres-driver", .package = "RPostgres")

  db_connect()

  expect_equal(captured$password, "env-secret-pw")
  expect_equal(captured$host, "db")
  expect_equal(captured$port, 5432L)
  expect_equal(captured$dbname, "directcare")
  expect_equal(captured$user, "directcare")
})
