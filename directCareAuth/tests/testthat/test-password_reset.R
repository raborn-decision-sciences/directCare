# Unit tests for R/password_reset.R. All DB calls are mocked -- these never
# touch a real Postgres instance.

test_that("password_reset_is_rate_limited is FALSE below the request threshold", {
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) data.frame(n = 2L),
    .package = "DBI"
  )

  expect_false(password_reset_is_rate_limited("mock_con", "doc@example.com"))
})

test_that("password_reset_is_rate_limited is TRUE at or above the request threshold", {
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) data.frame(n = 3L),
    .package = "DBI"
  )

  expect_true(password_reset_is_rate_limited("mock_con", "doc@example.com"))
})

test_that("password_reset_request returns ok=FALSE for an unknown email, without inserting", {
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      data.frame(
        id = integer(0), practice_name = character(0), email = character(0),
        password_hash = character(0), address = character(0),
        stringsAsFactors = FALSE
      )
    },
    dbExecute = function(...) stop("dbExecute() should not be called"),
    .package = "DBI"
  )

  result <- password_reset_request("mock_con", "nobody@example.com")

  expect_false(result$ok)
})

test_that("password_reset_request generates and stores a hashed token for a known email", {
  captured <- NULL
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      data.frame(
        id = 7L, practice_name = "River DPC", email = "doc@example.com",
        password_hash = "hash", address = NA_character_,
        stringsAsFactors = FALSE
      )
    },
    dbExecute = function(conn, statement, params) {
      captured <<- list(statement = statement, params = params)
      1L
    },
    .package = "DBI"
  )
  local_mocked_bindings(
    rand_bytes = function(n) as.raw(rep(1L, n)),
    sha256 = function(x) structure("hashed-token-value", class = c("hash", "sha256")),
    .package = "openssl"
  )

  result <- password_reset_request("mock_con", "doc@example.com")

  expect_true(result$ok)
  expect_equal(result$practice_id, 7L)
  expect_equal(result$practice_name, "River DPC")
  expect_match(result$token, "^([0-9a-f]{2})+$")
  expect_match(captured$statement, "INSERT INTO password_resets")
  expect_equal(captured$params[[1]], 7L)
  expect_equal(captured$params[[2]], "hashed-token-value")
  expect_equal(captured$params[[3]], 60L)
})

test_that("password_reset_consume rejects a NULL/blank token without querying the DB", {
  local_mocked_bindings(
    dbGetQuery = function(...) stop("dbGetQuery() should not be called"),
    .package = "DBI"
  )

  result <- password_reset_consume("mock_con", NULL, "a-strong-password")

  expect_false(result$ok)
  expect_equal(result$reason, "invalid_or_expired_token")
})

test_that("password_reset_consume rejects a token with no matching, unused, unexpired row", {
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) data.frame(id = integer(0), practice_id = integer(0)),
    .package = "DBI"
  )
  local_mocked_bindings(
    sha256 = function(x) structure("some-hash", class = c("hash", "sha256")),
    .package = "openssl"
  )

  result <- password_reset_consume("mock_con", "garbage-token", "a-strong-password")

  expect_false(result$ok)
  expect_equal(result$reason, "invalid_or_expired_token")
})

test_that("password_reset_consume rejects a weak new password without updating anything", {
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) data.frame(id = 1L, practice_id = 7L),
    dbExecute = function(...) stop("dbExecute() should not be called"),
    .package = "DBI"
  )
  local_mocked_bindings(
    sha256 = function(x) structure("valid-hash", class = c("hash", "sha256")),
    .package = "openssl"
  )

  result <- password_reset_consume("mock_con", "valid-token", "short1")

  expect_false(result$ok)
  expect_equal(result$reason, "weak_password")
})

test_that("password_reset_consume succeeds: updates the password and marks the token used", {
  captured <- list()
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) data.frame(id = 1L, practice_id = 7L),
    dbExecute = function(conn, statement, params) {
      captured[[length(captured) + 1]] <<- list(statement = statement, params = params)
      1L
    },
    .package = "DBI"
  )
  local_mocked_bindings(
    sha256 = function(x) structure("valid-hash", class = c("hash", "sha256")),
    .package = "openssl"
  )
  local_mocked_bindings(
    hashpw = function(password, salt) "new-hashed-password",
    gensalt = function() "salt",
    .package = "bcrypt"
  )

  result <- password_reset_consume("mock_con", "valid-token", "a-new-strong-pw")

  expect_true(result$ok)
  expect_equal(result$practice_id, 7L)
  expect_match(captured[[1]]$statement, "UPDATE practices SET password_hash")
  expect_equal(captured[[1]]$params, list("new-hashed-password", 7L))
  expect_match(captured[[2]]$statement, "UPDATE password_resets SET used_at")
  expect_equal(captured[[2]]$params, list(1L))
})
