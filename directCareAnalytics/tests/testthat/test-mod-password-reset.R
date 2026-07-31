# Unit tests for mod_password_reset.R. All directCareAuth calls are
# mocked -- these never touch a real Postgres instance or send a real
# email.
#
# Note: shiny's MockShinySession hardcodes clientData$url_search to a
# fixed test value with no way to override it in testServer(), so
# token_r()'s reactive always resolves NULL here -- see
# .extract_reset_token()'s own tests below for the token-parsing logic
# itself, and mock directCareAuth::password_reset_consume() directly
# (rather than relying on a real token) to test the reset_submit
# observer's branching.

test_that(".extract_reset_token pulls the token out of a URL search string", {
  expect_equal(.extract_reset_token("?reset=1&token=abc123"), "abc123")
})

test_that(".extract_reset_token returns NULL when no token is present", {
  expect_null(.extract_reset_token("?reset=1"))
  expect_null(.extract_reset_token(""))
  expect_null(.extract_reset_token(NULL))
})

test_that(".extract_reset_token returns NULL for a blank token value", {
  expect_null(.extract_reset_token("?reset=1&token="))
})

# ---------------------------------------------------------------------------
# Request-a-link branch (input$request_submit)
# ---------------------------------------------------------------------------

test_that("request_submit rejects a blank email without touching the DB", {
  local_mocked_bindings(
    db_connect = function() stop("db_connect() should not be called"),
    .package = "directCareAuth"
  )

  testServer(mod_password_reset_server, args = list(id = "reset"), {
    session$setInputs(email = "   ", request_submit = 1)
    expect_match(as.character(reset_msg()), "Enter your email")
  })
})

test_that("request_submit shows the generic message and sends an email for a known account", {
  logged <- list()
  sent <- NULL
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con"),
    extract_client_ip = function(session) NA_character_,
    auth_event_log = function(con, event_type, email = NA, practice_id = NA,
                               detail = NA, ip_address = NA) {
      logged[[length(logged) + 1]] <<- event_type
    },
    password_reset_is_rate_limited = function(con, email) FALSE,
    password_reset_is_rate_limited_by_ip = function(con, ip) FALSE,
    password_reset_request = function(con, email) {
      list(ok = TRUE, token = "tok-123", practice_id = 1L, practice_name = "River DPC")
    },
    send_password_reset_email = function(to_email, practice_name, reset_url) {
      sent <<- list(to_email = to_email, practice_name = practice_name, reset_url = reset_url)
    },
    .package = "directCareAuth"
  )
  local_mocked_bindings(dbDisconnect = function(conn) invisible(NULL), .package = "DBI")
  withr::local_envvar(APP_BASE_URL = "https://app.example.com")

  testServer(mod_password_reset_server, args = list(id = "reset"), {
    session$setInputs(email = "doc@example.com", request_submit = 1)
    expect_equal(logged, list("password_reset_requested"))
  })

  expect_equal(sent$to_email, "doc@example.com")
  expect_equal(sent$practice_name, "River DPC")
  expect_match(sent$reset_url, "^https://app\\.example\\.com/\\?reset=1&token=tok-123$")
})

test_that("request_submit shows the same generic message for an unknown email, without sending", {
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con"),
    extract_client_ip = function(session) NA_character_,
    auth_event_log = function(...) invisible(NULL),
    password_reset_is_rate_limited = function(con, email) FALSE,
    password_reset_is_rate_limited_by_ip = function(con, ip) FALSE,
    password_reset_request = function(con, email) list(ok = FALSE),
    send_password_reset_email = function(...) stop("send_password_reset_email() should not be called"),
    .package = "directCareAuth"
  )
  local_mocked_bindings(dbDisconnect = function(conn) invisible(NULL), .package = "DBI")

  testServer(mod_password_reset_server, args = list(id = "reset"), {
    session$setInputs(email = "nobody@example.com", request_submit = 1)
    expect_match(as.character(reset_msg()), "reset link")
  })
})

test_that("request_submit skips the lookup entirely when rate-limited by email", {
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con"),
    extract_client_ip = function(session) NA_character_,
    auth_event_log = function(...) invisible(NULL),
    password_reset_is_rate_limited = function(con, email) TRUE,
    password_reset_is_rate_limited_by_ip = function(con, ip) FALSE,
    password_reset_request = function(...) stop("password_reset_request() should not be called"),
    .package = "directCareAuth"
  )
  local_mocked_bindings(dbDisconnect = function(conn) invisible(NULL), .package = "DBI")

  testServer(mod_password_reset_server, args = list(id = "reset"), {
    session$setInputs(email = "doc@example.com", request_submit = 1)
    expect_match(as.character(reset_msg()), "reset link")
  })
})

test_that("request_submit skips the lookup entirely when rate-limited by IP", {
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con"),
    extract_client_ip = function(session) "203.0.113.7",
    auth_event_log = function(...) invisible(NULL),
    password_reset_is_rate_limited = function(con, email) FALSE,
    password_reset_is_rate_limited_by_ip = function(con, ip) TRUE,
    password_reset_request = function(...) stop("password_reset_request() should not be called"),
    .package = "directCareAuth"
  )
  local_mocked_bindings(dbDisconnect = function(conn) invisible(NULL), .package = "DBI")

  testServer(mod_password_reset_server, args = list(id = "reset"), {
    session$setInputs(email = "someone@example.com", request_submit = 1)
    expect_match(as.character(reset_msg()), "reset link")
  })
})

# ---------------------------------------------------------------------------
# Set-new-password branch (input$reset_submit)
# ---------------------------------------------------------------------------

test_that("reset_submit rejects mismatched passwords without calling password_reset_consume", {
  local_mocked_bindings(
    db_connect = function() stop("db_connect() should not be called"),
    .package = "directCareAuth"
  )

  testServer(mod_password_reset_server, args = list(id = "reset"), {
    session$setInputs(
      new_password = "a-strong-password", confirm_password = "different-password",
      reset_submit = 1
    )
    expect_match(as.character(reset_msg()), "do not match")
  })
})

test_that("reset_submit succeeds: marks reset_done and logs completion", {
  logged <- NULL
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con"),
    password_reset_consume = function(con, token, new_password) list(ok = TRUE, practice_id = 1L),
    auth_event_log = function(con, event_type, email = NA, practice_id = NA, detail = NA) {
      logged <<- list(event_type = event_type, practice_id = practice_id)
    },
    .package = "directCareAuth"
  )
  local_mocked_bindings(dbDisconnect = function(conn) invisible(NULL), .package = "DBI")

  testServer(mod_password_reset_server, args = list(id = "reset"), {
    session$setInputs(
      new_password = "a-new-strong-pw", confirm_password = "a-new-strong-pw",
      reset_submit = 1
    )
    expect_true(reset_done())
    expect_equal(logged$event_type, "password_reset_completed")
    expect_equal(logged$practice_id, 1L)
  })
})

test_that("reset_submit surfaces an invalid/expired token error", {
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con"),
    password_reset_consume = function(...) list(ok = FALSE, reason = "invalid_or_expired_token"),
    .package = "directCareAuth"
  )
  local_mocked_bindings(dbDisconnect = function(conn) invisible(NULL), .package = "DBI")

  testServer(mod_password_reset_server, args = list(id = "reset"), {
    session$setInputs(
      new_password = "a-new-strong-pw", confirm_password = "a-new-strong-pw",
      reset_submit = 1
    )
    expect_false(isTRUE(reset_done()))
    expect_match(as.character(reset_msg()), "invalid or has expired")
  })
})

test_that("reset_submit surfaces a weak-password error", {
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con"),
    password_reset_consume = function(...) list(ok = FALSE, reason = "weak_password"),
    .package = "directCareAuth"
  )
  local_mocked_bindings(dbDisconnect = function(conn) invisible(NULL), .package = "DBI")

  testServer(mod_password_reset_server, args = list(id = "reset"), {
    session$setInputs(
      new_password = "short1", confirm_password = "short1",
      reset_submit = 1
    )
    expect_false(isTRUE(reset_done()))
    expect_match(as.character(reset_msg()), "at least 10 characters")
  })
})
