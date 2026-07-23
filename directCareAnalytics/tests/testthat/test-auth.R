# Unit tests for the shinymanager auth wiring in R/auth.R. All DB calls are
# mocked -- these never touch a real Postgres instance.

test_that("check_credentials_db grants access on a valid email/password pair", {
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con")
  )
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      data.frame(
        id = 1L,
        practice_name = "River DPC",
        email = "doc@example.com",
        password_hash = "hashed-value",
        stringsAsFactors = FALSE
      )
    },
    dbDisconnect = function(conn) invisible(NULL),
    .package = "DBI"
  )
  local_mocked_bindings(checkpw = function(password, hash) TRUE, .package = "bcrypt")

  result <- check_credentials_db("doc@example.com", "correct-password")

  expect_true(result$result)
  expect_equal(result$user_info$practice_id, 1L)
  expect_equal(result$user_info$practice_name, "River DPC")
  expect_equal(result$user_info$email, "doc@example.com")
})

test_that("check_credentials_db denies access on a wrong password", {
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con")
  )
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      data.frame(
        id = 1L,
        practice_name = "River DPC",
        email = "doc@example.com",
        password_hash = "hashed-value",
        stringsAsFactors = FALSE
      )
    },
    dbDisconnect = function(conn) invisible(NULL),
    .package = "DBI"
  )
  local_mocked_bindings(checkpw = function(password, hash) FALSE, .package = "bcrypt")

  result <- check_credentials_db("doc@example.com", "wrong-password")

  expect_false(result$result)
  expect_null(result$user_info)
})

test_that("check_credentials_db denies access for an unknown email without hashing", {
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con")
  )
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      data.frame(
        id = integer(0),
        practice_name = character(0),
        email = character(0),
        password_hash = character(0),
        stringsAsFactors = FALSE
      )
    },
    dbDisconnect = function(conn) invisible(NULL),
    .package = "DBI"
  )
  # nrow != 1 should short-circuit before checkpw() is ever reached.
  local_mocked_bindings(
    checkpw = function(...) stop("checkpw() should not be called"),
    .package = "bcrypt"
  )

  result <- check_credentials_db("nobody@example.com", "whatever")

  expect_false(result$result)
})

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
