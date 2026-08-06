# Unit tests for R/practices.R. All DB calls are mocked -- these never
# touch a real Postgres instance.

test_that("signup_is_rate_limited is FALSE below the attempt threshold", {
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) data.frame(n = 4L),
    .package = "DBI"
  )

  expect_false(signup_is_rate_limited("mock_con", "doc@example.com"))
})

test_that("signup_is_rate_limited is TRUE at or above the attempt threshold", {
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) data.frame(n = 5L),
    .package = "DBI"
  )

  expect_true(signup_is_rate_limited("mock_con", "doc@example.com"))
})

test_that("signup_is_rate_limited queries by email and the configured window", {
  captured <- NULL
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      captured <<- list(statement = statement, params = params)
      data.frame(n = 0L)
    },
    .package = "DBI"
  )

  signup_is_rate_limited("mock_con", "doc@example.com", max_requests = 3, window_minutes = 30)

  expect_match(captured$statement, "event_type = 'signup_attempt'")
  expect_equal(captured$params, list("doc@example.com", 30L))
})

test_that("signup_is_rate_limited_by_ip is FALSE when the IP is NA, without querying", {
  local_mocked_bindings(
    dbGetQuery = function(...) stop("dbGetQuery() should not be called for an NA IP"),
    .package = "DBI"
  )

  expect_false(signup_is_rate_limited_by_ip("mock_con", NA_character_))
})

test_that("signup_is_rate_limited_by_ip is FALSE below the attempt threshold", {
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) data.frame(n = 19L),
    .package = "DBI"
  )

  expect_false(signup_is_rate_limited_by_ip("mock_con", "203.0.113.7"))
})

test_that("signup_is_rate_limited_by_ip is TRUE at or above the default 20-attempt threshold", {
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) data.frame(n = 20L),
    .package = "DBI"
  )

  expect_true(signup_is_rate_limited_by_ip("mock_con", "203.0.113.7"))
})

test_that("signup_is_rate_limited_by_ip queries by ip and the configured window", {
  captured <- NULL
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      captured <<- list(statement = statement, params = params)
      data.frame(n = 0L)
    },
    .package = "DBI"
  )

  signup_is_rate_limited_by_ip("mock_con", "203.0.113.7", max_requests = 10, window_minutes = 30)

  expect_match(captured$statement, "event_type = 'signup_attempt'")
  expect_match(captured$statement, "ip_address = \\$1")
  expect_equal(captured$params, list("203.0.113.7", 30L))
})

test_that("practice_find_by_email issues the expected query", {
  captured <- NULL
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      captured <<- list(statement = statement, params = params)
      data.frame(
        id = 1L, practice_name = "River DPC", email = "doc@example.com",
        password_hash = "hash", address = "123 Main St",
        stringsAsFactors = FALSE
      )
    },
    .package = "DBI"
  )

  result <- practice_find_by_email("mock_con", "doc@example.com")

  expect_equal(captured$params, list("doc@example.com"))
  expect_match(captured$statement, "FROM practices WHERE email = \\$1")
  expect_equal(result$practice_name, "River DPC")
  expect_equal(result$address, "123 Main St")
})

test_that("practice_create rejects a weak password without touching the DB", {
  local_mocked_bindings(
    dbGetQuery = function(...) stop("dbGetQuery() should not be called"),
    .package = "DBI"
  )

  result <- practice_create("mock_con", "River DPC", "doc@example.com", "short1")

  expect_false(result$ok)
  expect_equal(result$reason, "weak_password")
})

test_that("practice_create hashes the password and inserts a new row", {
  captured <- NULL
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      captured <<- list(statement = statement, params = params)
      data.frame(id = 42L)
    },
    .package = "DBI"
  )
  local_mocked_bindings(
    hashpw = function(password, salt) "hashed-password",
    gensalt = function() "salt",
    .package = "bcrypt"
  )

  result <- practice_create("mock_con", "River DPC", "doc@example.com", "a-strong-password")

  expect_true(result$ok)
  expect_equal(result$id, 42L)
  expect_equal(
    captured$params,
    list("River DPC", "doc@example.com", "hashed-password", "", "", "", "", "", "", "")
  )
  expect_match(captured$statement, "ON CONFLICT \\(email\\) DO NOTHING")
})

test_that("practice_create passes through the optional signup profile fields", {
  captured <- NULL
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      captured <<- list(statement = statement, params = params)
      data.frame(id = 42L)
    },
    .package = "DBI"
  )
  local_mocked_bindings(
    hashpw = function(password, salt) "hashed-password",
    gensalt = function() "salt",
    .package = "bcrypt"
  )

  result <- practice_create(
    "mock_con", "River DPC", "doc@example.com", "a-strong-password",
    first_name = "Jane", last_name = "Smith", practice_type = "Physician",
    practice_type_other = "", practice_status = "Just exploring",
    practice_specialty = "Primary Care", referral_source = "Word of mouth"
  )

  expect_true(result$ok)
  expect_equal(
    captured$params,
    list(
      "River DPC", "doc@example.com", "hashed-password",
      "Jane", "Smith", "Physician", "", "Just exploring",
      "Primary Care", "Word of mouth"
    )
  )
})

test_that("practice_create reports email_taken when the insert returns no row", {
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) data.frame(id = integer(0)),
    .package = "DBI"
  )
  local_mocked_bindings(
    hashpw = function(password, salt) "hashed-password",
    gensalt = function() "salt",
    .package = "bcrypt"
  )

  result <- practice_create("mock_con", "River DPC", "taken@example.com", "a-strong-password")

  expect_false(result$ok)
  expect_equal(result$reason, "email_taken")
})

test_that("practice_update_password rejects a wrong current password without hashing", {
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) data.frame(password_hash = "hash"),
    .package = "DBI"
  )
  local_mocked_bindings(checkpw = function(password, hash) FALSE, .package = "bcrypt")
  local_mocked_bindings(
    hashpw = function(...) stop("hashpw() should not be called"),
    .package = "bcrypt"
  )

  result <- practice_update_password("mock_con", 1L, "wrong-current", "a-new-strong-pw")

  expect_false(result$ok)
  expect_equal(result$reason, "wrong_current_password")
})

test_that("practice_update_password rejects a weak new password", {
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) data.frame(password_hash = "hash"),
    .package = "DBI"
  )
  local_mocked_bindings(checkpw = function(password, hash) TRUE, .package = "bcrypt")

  result <- practice_update_password("mock_con", 1L, "correct-current-pw", "short1")

  expect_false(result$ok)
  expect_equal(result$reason, "weak_password")
})

test_that("practice_update_password succeeds and writes the new hash", {
  captured <- NULL
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) data.frame(password_hash = "old-hash"),
    dbExecute = function(conn, statement, params) {
      captured <<- list(statement = statement, params = params)
      1L
    },
    .package = "DBI"
  )
  local_mocked_bindings(
    checkpw = function(password, hash) TRUE,
    hashpw = function(password, salt) "new-hash",
    gensalt = function() "salt",
    .package = "bcrypt"
  )

  result <- practice_update_password("mock_con", 1L, "correct-current-pw", "a-new-strong-pw")

  expect_true(result$ok)
  expect_equal(captured$params, list("new-hash", 1L))
})

test_that("practice_update_profile issues the expected update", {
  captured <- NULL
  local_mocked_bindings(
    dbExecute = function(conn, statement, params) {
      captured <<- list(statement = statement, params = params)
      1L
    },
    .package = "DBI"
  )

  result <- practice_update_profile("mock_con", 7L, "New Name", "456 Oak Ave")

  expect_true(result$ok)
  expect_equal(
    captured$params,
    list("New Name", "456 Oak Ave", "", "", "", "", "", "", "", 7L)
  )
})

test_that("practice_update_profile passes through the optional profile fields", {
  captured <- NULL
  local_mocked_bindings(
    dbExecute = function(conn, statement, params) {
      captured <<- list(statement = statement, params = params)
      1L
    },
    .package = "DBI"
  )

  result <- practice_update_profile(
    "mock_con", 7L, "New Name", "456 Oak Ave",
    first_name = "Jane", last_name = "Smith", practice_type = "Physician",
    practice_type_other = "", practice_status = "Just exploring",
    practice_specialty = "Primary Care", referral_source = "Word of mouth"
  )

  expect_true(result$ok)
  expect_equal(
    captured$params,
    list(
      "New Name", "456 Oak Ave", "Jane", "Smith", "Physician", "",
      "Just exploring", "Primary Care", "Word of mouth", 7L
    )
  )
})
