# Unit tests for mod_signup_server (via testServer). All directCareAuth
# calls are mocked -- these never touch a real Postgres instance. Stripe
# Checkout itself (triggered from the "choose_plan" stage, not tested here)
# is exercised in directCareBilling's own test-checkout.R.

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
    expect_equal(signup_stage(), "form")
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
    expect_equal(signup_stage(), "form")
  })
})

test_that("a successful signup moves to the plan picker and logs a signup event", {
  logged <- NULL
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con"),
    extract_client_ip = function(session) NA_character_,
    signup_is_rate_limited = function(...) FALSE,
    signup_is_rate_limited_by_ip = function(...) FALSE,
    practice_create = function(con, practice_name, email, password) {
      list(ok = TRUE, id = 7L)
    },
    auth_event_log = function(con, event_type, email = NA, practice_id = NA,
                               detail = NA, ip_address = NA) {
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
    expect_equal(signup_stage(), "choose_plan")
    expect_equal(new_practice_id(), 7L)
    expect_equal(new_practice_email(), "doc@example.com")
    expect_equal(logged$event_type, "signup")
    expect_equal(logged$practice_id, 7L)
    expect_equal(logged$email, "doc@example.com")
  })
})

test_that("choosing Free from the plan picker moves to the done stage", {
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con"),
    extract_client_ip = function(session) NA_character_,
    signup_is_rate_limited = function(...) FALSE,
    signup_is_rate_limited_by_ip = function(...) FALSE,
    practice_create = function(...) list(ok = TRUE, id = 7L),
    auth_event_log = function(...) invisible(NULL),
    .package = "directCareAuth"
  )
  local_mocked_bindings(dbDisconnect = function(conn) invisible(NULL), .package = "DBI")

  testServer(mod_signup_server, {
    session$setInputs(
      practice_name = "River DPC", email = "doc@example.com",
      password = "a-strong-password", confirm_password = "a-strong-password",
      submit = 1
    )
    session$setInputs(plan_free = 1)
    expect_equal(signup_stage(), "done")
  })
})

test_that("choosing Starter from the plan picker starts a Checkout redirect", {
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con"),
    extract_client_ip = function(session) NA_character_,
    signup_is_rate_limited = function(...) FALSE,
    signup_is_rate_limited_by_ip = function(...) FALSE,
    practice_create = function(...) list(ok = TRUE, id = 7L),
    auth_event_log = function(...) invisible(NULL),
    .package = "directCareAuth"
  )
  local_mocked_bindings(dbDisconnect = function(conn) invisible(NULL), .package = "DBI")

  captured <- NULL
  local_mocked_bindings(
    stripe_create_checkout_session = function(practice_id, price_lookup_key, customer_email, ...) {
      captured <<- list(
        practice_id = practice_id, price_lookup_key = price_lookup_key,
        customer_email = customer_email
      )
      "https://checkout.stripe.com/test_session"
    },
    .package = "directCareBilling"
  )

  testServer(mod_signup_server, {
    session$setInputs(
      practice_name = "River DPC", email = "doc@example.com",
      password = "a-strong-password", confirm_password = "a-strong-password",
      submit = 1
    )
    session$setInputs(plan_starter = 1)
    # Still "choose_plan" -- a successful checkout-session creation redirects
    # the browser away rather than changing signup_stage() itself.
    expect_equal(signup_stage(), "choose_plan")
    expect_equal(captured$practice_id, 7L)
    expect_equal(captured$price_lookup_key, STRIPE_LOOKUP_KEY_STARTER)
    expect_equal(captured$customer_email, "doc@example.com")
  })
})

test_that("email_taken keeps the form up", {
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con"),
    extract_client_ip = function(session) NA_character_,
    signup_is_rate_limited = function(...) FALSE,
    signup_is_rate_limited_by_ip = function(...) FALSE,
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
    expect_equal(signup_stage(), "form")
  })
})

test_that("weak_password keeps the form up", {
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con"),
    extract_client_ip = function(session) NA_character_,
    signup_is_rate_limited = function(...) FALSE,
    signup_is_rate_limited_by_ip = function(...) FALSE,
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
    expect_equal(signup_stage(), "form")
  })
})

test_that("a rate-limited email shows a message and never calls practice_create", {
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con"),
    extract_client_ip = function(session) NA_character_,
    auth_event_log = function(...) invisible(NULL),
    signup_is_rate_limited = function(...) TRUE,
    signup_is_rate_limited_by_ip = function(...) FALSE,
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
    expect_equal(signup_stage(), "form")
  })
})

test_that("a rate-limited IP shows a message and never calls practice_create", {
  local_mocked_bindings(
    db_connect = function() structure(list(), class = "mock_con"),
    extract_client_ip = function(session) "203.0.113.7",
    auth_event_log = function(...) invisible(NULL),
    signup_is_rate_limited = function(...) FALSE,
    signup_is_rate_limited_by_ip = function(...) TRUE,
    practice_create = function(...) stop("practice_create() should not be called"),
    .package = "directCareAuth"
  )
  local_mocked_bindings(dbDisconnect = function(conn) invisible(NULL), .package = "DBI")

  testServer(mod_signup_server, {
    session$setInputs(
      practice_name = "River DPC", email = "someone@example.com",
      password = "a-strong-password", confirm_password = "a-strong-password",
      submit = 1
    )
    expect_equal(signup_stage(), "form")
  })
})
