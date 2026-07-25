# Unit tests for R/auth_events.R (audit logging + login lockout). All DB
# calls are mocked -- these never touch a real Postgres instance.

test_that("auth_event_log inserts the given fields", {
  captured <- NULL
  local_mocked_bindings(
    dbExecute = function(conn, statement, params) {
      captured <<- list(statement = statement, params = params)
      1L
    },
    .package = "DBI"
  )

  auth_event_log(
    "mock_con", event_type = "login_success",
    email = "doc@example.com", practice_id = 1L
  )

  expect_match(captured$statement, "INSERT INTO auth_events")
  expect_equal(captured$params, list(1L, "doc@example.com", "login_success", NA_character_))
})

test_that("auth_is_locked_out is FALSE below the failure threshold", {
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) data.frame(n = 4L),
    .package = "DBI"
  )

  expect_false(auth_is_locked_out("mock_con", "doc@example.com"))
})

test_that("auth_is_locked_out is TRUE at or above the failure threshold", {
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) data.frame(n = 5L),
    .package = "DBI"
  )

  expect_true(auth_is_locked_out("mock_con", "doc@example.com"))
})

test_that("auth_is_locked_out passes the configured window in minutes", {
  captured <- NULL
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      captured <<- params
      data.frame(n = 0L)
    },
    .package = "DBI"
  )

  auth_is_locked_out("mock_con", "doc@example.com", max_attempts = 3, window_minutes = 30)

  expect_equal(captured, list("doc@example.com", 30L))
})
