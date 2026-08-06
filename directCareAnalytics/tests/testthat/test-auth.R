# Unit tests for the shinymanager auth wiring in R/auth.R. check_credentials_db()
# is now a thin wrapper over directCareAuth -- these tests mock that
# package's exported functions, plus bcrypt/DBI, and never touch a real
# Postgres instance. directCareAuth's own DB-level logic (lockout counting,
# query shape, etc.) is tested in that package directly.

test_that("check_credentials_db grants access on a valid email/password pair", {
  local_mocked_bindings(db_connect = function() structure(list(), class = "mock_con"),
    .package = "directCareAuth")
  local_mocked_bindings(
    auth_is_locked_out = function(con, email) FALSE,
    auth_is_locked_out_by_ip = function(con, ip) FALSE,
    practice_find_by_email = function(con, email) {
      data.frame(
        id = 1L,
        practice_name = "River DPC",
        email = "doc@example.com",
        password_hash = "hashed-value",
        address = "123 Main St",
        plan_tier = "pro",
        subscription_status = "active",
        stringsAsFactors = FALSE
      )
    },
    auth_event_log = function(...) invisible(NULL),
    .package = "directCareAuth"
  )
  local_mocked_bindings(dbDisconnect = function(conn) invisible(NULL), .package = "DBI")
  local_mocked_bindings(checkpw = function(password, hash) TRUE, .package = "bcrypt")

  result <- check_credentials_db("doc@example.com", "correct-password", ip = "203.0.113.7")

  expect_true(result$result)
  expect_equal(result$user_info$practice_id, 1L)
  expect_equal(result$user_info$practice_name, "River DPC")
  expect_equal(result$user_info$email, "doc@example.com")
  expect_equal(result$user_info$address, "123 Main St")
  expect_equal(result$user_info$plan_tier, "pro")
  expect_equal(result$user_info$subscription_status, "active")
})

test_that("check_credentials_db denies access on a wrong password and logs a failure with the ip", {
  local_mocked_bindings(db_connect = function() structure(list(), class = "mock_con"),
    .package = "directCareAuth")
  logged <- NULL
  local_mocked_bindings(
    auth_is_locked_out = function(con, email) FALSE,
    auth_is_locked_out_by_ip = function(con, ip) FALSE,
    practice_find_by_email = function(con, email) {
      data.frame(
        id = 1L, practice_name = "River DPC", email = "doc@example.com",
        password_hash = "hashed-value", address = NA_character_,
        stringsAsFactors = FALSE
      )
    },
    auth_event_log = function(con, event_type, email = NA, practice_id = NA, detail = NA,
                               ip_address = NA) {
      logged <<- list(event_type = event_type, ip_address = ip_address)
    },
    .package = "directCareAuth"
  )
  local_mocked_bindings(dbDisconnect = function(conn) invisible(NULL), .package = "DBI")
  local_mocked_bindings(checkpw = function(password, hash) FALSE, .package = "bcrypt")

  result <- check_credentials_db("doc@example.com", "wrong-password", ip = "203.0.113.7")

  expect_false(result$result)
  expect_null(result$user_info)
  expect_equal(logged$event_type, "login_failure")
  expect_equal(logged$ip_address, "203.0.113.7")
})

test_that("check_credentials_db denies access for an unknown email without hashing", {
  local_mocked_bindings(db_connect = function() structure(list(), class = "mock_con"),
    .package = "directCareAuth")
  local_mocked_bindings(
    auth_is_locked_out = function(con, email) FALSE,
    auth_is_locked_out_by_ip = function(con, ip) FALSE,
    practice_find_by_email = function(con, email) {
      data.frame(
        id = integer(0), practice_name = character(0), email = character(0),
        password_hash = character(0), address = character(0),
        stringsAsFactors = FALSE
      )
    },
    auth_event_log = function(...) invisible(NULL),
    .package = "directCareAuth"
  )
  local_mocked_bindings(dbDisconnect = function(conn) invisible(NULL), .package = "DBI")
  # nrow != 1 should short-circuit before checkpw() is ever reached.
  local_mocked_bindings(
    checkpw = function(...) stop("checkpw() should not be called"),
    .package = "bcrypt"
  )

  result <- check_credentials_db("nobody@example.com", "whatever")

  expect_false(result$result)
})

test_that("check_credentials_db denies access when locked out by email, without querying the practice", {
  local_mocked_bindings(db_connect = function() structure(list(), class = "mock_con"),
    .package = "directCareAuth")
  logged <- NULL
  local_mocked_bindings(
    auth_is_locked_out = function(con, email) TRUE,
    auth_is_locked_out_by_ip = function(con, ip) FALSE,
    practice_find_by_email = function(...) stop("practice_find_by_email() should not be called"),
    auth_event_log = function(con, event_type, email = NA, practice_id = NA, detail = NA,
                               ip_address = NA) {
      logged <<- event_type
    },
    .package = "directCareAuth"
  )
  local_mocked_bindings(dbDisconnect = function(conn) invisible(NULL), .package = "DBI")

  result <- check_credentials_db("doc@example.com", "correct-password")

  expect_false(result$result)
  expect_equal(logged, "login_locked_out")
})

test_that("check_credentials_db denies access when locked out by ip, without querying the practice", {
  # The other half of the OR condition -- a spray attack across many
  # emails from one IP, none of which individually trips the email-based
  # lockout, should still be blocked.
  local_mocked_bindings(db_connect = function() structure(list(), class = "mock_con"),
    .package = "directCareAuth")
  logged <- NULL
  local_mocked_bindings(
    auth_is_locked_out = function(con, email) FALSE,
    auth_is_locked_out_by_ip = function(con, ip) TRUE,
    practice_find_by_email = function(...) stop("practice_find_by_email() should not be called"),
    auth_event_log = function(con, event_type, email = NA, practice_id = NA, detail = NA,
                               ip_address = NA) {
      logged <<- list(event_type = event_type, ip_address = ip_address)
    },
    .package = "directCareAuth"
  )
  local_mocked_bindings(dbDisconnect = function(conn) invisible(NULL), .package = "DBI")

  result <- check_credentials_db("someone@example.com", "correct-password", ip = "203.0.113.7")

  expect_false(result$result)
  expect_equal(logged$event_type, "login_locked_out")
  expect_equal(logged$ip_address, "203.0.113.7")
})
