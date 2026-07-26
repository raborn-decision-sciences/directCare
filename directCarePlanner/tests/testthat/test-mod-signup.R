# Unit tests for mod_signup_server (via testServer). All directCareAuth
# calls are mocked -- these never touch a real Postgres instance.

test_that("submit is a no-op (no DB call) when required fields are blank", {
  local_mocked_bindings(
    practice_create = function(...) stop("practice_create() should not be called"),
    .package = "directCareAuth"
  )

  testServer(mod_signup_server, {
    session$setInputs(
      practice_name = "", email = "", password = "a-strong-password",
      confirm_password = "a-strong-password", submit = 1
    )
    expect_false(isTRUE(signup_done()))
  })
})

test_that("submit is a no-op (no DB call) when passwords do not match", {
  local_mocked_bindings(
    practice_create = function(...) stop("practice_create() should not be called"),
    .package = "directCareAuth"
  )

  testServer(mod_signup_server, {
    session$setInputs(
      practice_name = "River DPC", email = "doc@example.com",
      password = "a-strong-password", confirm_password = "different-password",
      submit = 1
    )
    expect_false(isTRUE(signup_done()))
  })
})

test_that("a successful signup marks signup_done and logs a signup event", {
  logged <- NULL
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con"),
    signup_is_rate_limited = function(...) FALSE,
    practice_create = function(con, practice_name, email, password) {
      list(ok = TRUE, id = 7L)
    },
    auth_event_log = function(con, event_type, email = NA, practice_id = NA, detail = NA) {
      logged <<- list(event_type = event_type, email = email, practice_id = practice_id)
    },
    .package = "directCareAuth"
  )
  local_mocked_bindings(dbDisconnect = function(conn) invisible(NULL), .package = "DBI")

  testServer(mod_signup_server, {
    session$setInputs(
      practice_name = "River DPC", email = "doc@example.com",
      password = "a-strong-password", confirm_password = "a-strong-password",
      submit = 1
    )
    expect_true(signup_done())
    expect_equal(logged$event_type, "signup")
    expect_equal(logged$practice_id, 7L)
    expect_equal(logged$email, "doc@example.com")
  })
})

test_that("email_taken keeps the form up, without marking signup_done", {
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con"),
    signup_is_rate_limited = function(...) FALSE,
    auth_event_log = function(...) invisible(NULL),
    practice_create = function(...) list(ok = FALSE, reason = "email_taken"),
    .package = "directCareAuth"
  )
  local_mocked_bindings(dbDisconnect = function(conn) invisible(NULL), .package = "DBI")

  testServer(mod_signup_server, {
    session$setInputs(
      practice_name = "River DPC", email = "taken@example.com",
      password = "a-strong-password", confirm_password = "a-strong-password",
      submit = 1
    )
    expect_false(isTRUE(signup_done()))
  })
})

test_that("weak_password keeps the form up, without marking signup_done", {
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con"),
    signup_is_rate_limited = function(...) FALSE,
    auth_event_log = function(...) invisible(NULL),
    practice_create = function(...) list(ok = FALSE, reason = "weak_password"),
    .package = "directCareAuth"
  )
  local_mocked_bindings(dbDisconnect = function(conn) invisible(NULL), .package = "DBI")

  testServer(mod_signup_server, {
    session$setInputs(
      practice_name = "River DPC", email = "doc@example.com",
      password = "short1", confirm_password = "short1",
      submit = 1
    )
    expect_false(isTRUE(signup_done()))
  })
})

test_that("a rate-limited email shows a message and never calls practice_create", {
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con"),
    auth_event_log = function(...) invisible(NULL),
    signup_is_rate_limited = function(...) TRUE,
    practice_create = function(...) stop("practice_create() should not be called"),
    .package = "directCareAuth"
  )
  local_mocked_bindings(dbDisconnect = function(conn) invisible(NULL), .package = "DBI")

  testServer(mod_signup_server, {
    session$setInputs(
      practice_name = "River DPC", email = "spammer@example.com",
      password = "a-strong-password", confirm_password = "a-strong-password",
      submit = 1
    )
    expect_false(isTRUE(signup_done()))
  })
})
