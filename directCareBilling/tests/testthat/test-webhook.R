# Unit tests for R/webhook.R. Signature tests build a real, validly-signed
# header the same way Stripe itself would (via openssl HMAC-SHA256) --
# HMAC correctness is guaranteed by openssl; the point here is exercising
# *this package's* header-parsing, timestamp-tolerance, and
# tamper/wrong-secret rejection logic. All DB calls are mocked (matching
# directCareAuth's own local_mocked_bindings(..., .package = "DBI")
# pattern) -- these never touch a real Postgres instance, and any
# Stripe API call (the checkout-session line-items lookup) is mocked via
# httr2::local_mocked_responses(), matching directCareAuth's own
# test-email.R precedent.

.make_sig_header <- function(payload, secret, timestamp = as.integer(Sys.time())) {
  signed_payload <- paste0(timestamp, ".", payload)
  sig <- as.character(openssl::sha256(charToRaw(signed_payload), key = charToRaw(secret)))
  paste0("t=", timestamp, ",v1=", sig)
}

test_that("stripe_verify_webhook_signature accepts a validly-signed payload", {
  secret <- "whsec_test_123"
  payload <- '{"id":"evt_1","type":"checkout.session.completed"}'
  header <- .make_sig_header(payload, secret)

  expect_true(stripe_verify_webhook_signature(payload, header, secret))
})

test_that("stripe_verify_webhook_signature rejects a tampered payload", {
  secret <- "whsec_test_123"
  header <- .make_sig_header('{"amount":100}', secret)

  expect_false(stripe_verify_webhook_signature('{"amount":100000}', header, secret))
})

test_that("stripe_verify_webhook_signature rejects the wrong secret", {
  payload <- '{"id":"evt_1"}'
  header <- .make_sig_header(payload, "whsec_correct")

  expect_false(stripe_verify_webhook_signature(payload, header, "whsec_wrong"))
})

test_that("stripe_verify_webhook_signature rejects an expired timestamp", {
  secret <- "whsec_test_123"
  payload <- '{"id":"evt_1"}'
  old_timestamp <- as.integer(Sys.time()) - 600L
  header <- .make_sig_header(payload, secret, timestamp = old_timestamp)

  expect_false(stripe_verify_webhook_signature(payload, header, secret, tolerance_seconds = 300))
})

test_that("stripe_verify_webhook_signature accepts a within-tolerance timestamp", {
  secret <- "whsec_test_123"
  payload <- '{"id":"evt_1"}'
  recent_timestamp <- as.integer(Sys.time()) - 60L
  header <- .make_sig_header(payload, secret, timestamp = recent_timestamp)

  expect_true(stripe_verify_webhook_signature(payload, header, secret, tolerance_seconds = 300))
})

test_that("stripe_verify_webhook_signature rejects a malformed header", {
  secret <- "whsec_test_123"
  payload <- '{"id":"evt_1"}'

  expect_false(stripe_verify_webhook_signature(payload, "not-a-real-header", secret))
  expect_false(stripe_verify_webhook_signature(payload, "t=12345", secret))
  expect_false(stripe_verify_webhook_signature(payload, "", secret))
})

test_that("stripe_verify_webhook_signature matches any v1 value during a secret rotation", {
  secret <- "whsec_new"
  payload <- '{"id":"evt_1"}'
  timestamp <- as.integer(Sys.time())
  old_sig <- .make_sig_header(payload, "whsec_old", timestamp)
  new_sig <- .make_sig_header(payload, secret, timestamp)
  # Combine into one header the way Stripe does during a rotation window:
  # "t=...,v1=<old>,v1=<new>"
  combined <- paste0(old_sig, ",v1=", sub("^.*,v1=", "", new_sig))

  expect_true(stripe_verify_webhook_signature(payload, combined, secret))
})

test_that("stripe_handle_webhook_event checkout.session.completed updates the practice", {
  withr::local_envvar(STRIPE_PRICE_PRO = "pro_monthly")
  captured <- NULL
  local_mocked_bindings(
    dbExecute = function(conn, statement, params) {
      captured <<- params
      1L
    },
    .package = "DBI"
  )
  httr2::local_mocked_responses(function(req) {
    if (grepl("/line_items", req$url, fixed = TRUE)) {
      return(httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(jsonlite::toJSON(list(
          data = list(list(price = list(lookup_key = "pro_monthly")))
        ), auto_unbox = TRUE))
      ))
    }
    # The Subscription fetch -- checkout.session.completed's own payload
    # only carries the Checkout Session's status ("complete"), a
    # different, unrelated status than the Subscription's own
    # ("active" here) -- see .stripe_fetch_subscription()'s own comment.
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(jsonlite::toJSON(
        list(status = "active", current_period_end = 1755000000),
        auto_unbox = TRUE
      ))
    )
  })

  event <- list(
    type = "checkout.session.completed",
    data = list(object = list(
      id = "cs_123", client_reference_id = "42",
      customer = "cus_1", subscription = "sub_1", status = "complete"
    ))
  )

  result <- stripe_handle_webhook_event("mock_con", event)

  expect_true(result)
  expect_equal(captured[[1]], "cus_1")
  expect_equal(captured[[2]], "sub_1")
  expect_equal(captured[[3]], "pro")
  expect_equal(captured[[4]], "active")
  expect_equal(captured[[5]], as.POSIXct(1755000000, origin = "1970-01-01", tz = "UTC"))
  expect_equal(captured[[6]], 42L)
})

test_that("stripe_handle_webhook_event errors on an unmapped price lookup_key", {
  withr::local_envvar(STRIPE_PRICE_PRO = NA)
  local_mocked_bindings(
    dbExecute = function(...) stop("dbExecute() should not be called before tier resolution errors"),
    .package = "DBI"
  )
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(jsonlite::toJSON(list(
        data = list(list(price = list(lookup_key = "unmapped_tier")))
      ), auto_unbox = TRUE))
    )
  })

  event <- list(
    type = "checkout.session.completed",
    data = list(object = list(id = "cs_1", client_reference_id = "1", customer = "cus_1"))
  )

  expect_error(stripe_handle_webhook_event("mock_con", event), "unmapped_tier")
})

test_that("stripe_handle_webhook_event customer.subscription.updated refreshes status/tier/period_end", {
  withr::local_envvar(STRIPE_PRICE_STARTER = "starter_monthly")
  captured <- NULL
  local_mocked_bindings(
    dbExecute = function(conn, statement, params) {
      captured <<- params
      1L
    },
    .package = "DBI"
  )

  event <- list(
    type = "customer.subscription.updated",
    data = list(object = list(
      customer = "cus_1", status = "past_due",
      current_period_end = 1893456000,
      items = list(data = list(list(price = list(lookup_key = "starter_monthly"))))
    ))
  )

  result <- stripe_handle_webhook_event("mock_con", event)

  expect_true(result)
  expect_equal(captured[[1]], "past_due")
  expect_equal(captured[[2]], "starter")
  expect_equal(captured[[4]], "cus_1")
})

test_that("stripe_handle_webhook_event customer.subscription.updated tolerates an unresolved lookup_key", {
  # A lookup_key with no STRIPE_PRICE_<TIER> mapping (a Price without one
  # yet, or a stray fixture/test Price) shouldn't error -- dbExecute()'s
  # COALESCE($2, plan_tier) is what leaves plan_tier untouched in that
  # case, but only if this function passes NA (not a bare R NULL, which
  # DBI rejects as "Parameter 2 does not have length 1") -- confirmed
  # live against a real Postgres instance, not just this mock.
  withr::local_envvar(STRIPE_PRICE_STARTER = "starter_monthly")
  captured <- NULL
  local_mocked_bindings(
    dbExecute = function(conn, statement, params) {
      captured <<- params
      1L
    },
    .package = "DBI"
  )

  event <- list(
    type = "customer.subscription.updated",
    data = list(object = list(
      customer = "cus_1", status = "active",
      current_period_end = 1893456000,
      items = list(data = list(list(price = list(lookup_key = "some_unmapped_price"))))
    ))
  )

  result <- stripe_handle_webhook_event("mock_con", event)

  expect_true(result)
  expect_equal(captured[[1]], "active")
  expect_true(is.na(captured[[2]]))
  expect_equal(captured[[4]], "cus_1")
})

test_that("stripe_handle_webhook_event customer.subscription.deleted reverts to canceled/free", {
  captured <- NULL
  local_mocked_bindings(
    dbExecute = function(conn, statement, params) {
      captured <<- params
      1L
    },
    .package = "DBI"
  )

  event <- list(
    type = "customer.subscription.deleted",
    data = list(object = list(customer = "cus_1"))
  )

  result <- stripe_handle_webhook_event("mock_con", event)

  expect_true(result)
  expect_equal(captured[[1]], "cus_1")
})

test_that("stripe_handle_webhook_event is a no-op for an unrecognized event type", {
  local_mocked_bindings(
    dbExecute = function(...) stop("dbExecute() should not be called for an unrecognized event"),
    .package = "DBI"
  )

  event <- list(type = "payment_intent.succeeded", data = list(object = list()))

  expect_false(stripe_handle_webhook_event("mock_con", event))
})

test_that(".stripe_webhook_secret() prefers the _FILE variant over the plain env var", {
  tmp <- withr::local_tempfile(lines = "whsec_from_file")
  withr::local_envvar(
    STRIPE_WEBHOOK_SECRET_FILE = tmp,
    STRIPE_WEBHOOK_SECRET = "whsec_from_env"
  )
  expect_equal(directCareBilling:::.stripe_webhook_secret(), "whsec_from_file")
})

test_that(".stripe_webhook_secret() falls back to the plain env var when no file is set", {
  withr::local_envvar(STRIPE_WEBHOOK_SECRET_FILE = NA, STRIPE_WEBHOOK_SECRET = "whsec_from_env")
  expect_equal(directCareBilling:::.stripe_webhook_secret(), "whsec_from_env")
})

test_that(".stripe_webhook_secret() returns empty string when nothing is configured", {
  withr::local_envvar(STRIPE_WEBHOOK_SECRET_FILE = NA, STRIPE_WEBHOOK_SECRET = "")
  expect_equal(directCareBilling:::.stripe_webhook_secret(), "")
})
