# Unit tests for R/checkout.R. Uses httr2::local_mocked_responses() --
# matching directCareAuth's test-email.R precedent -- so these never make
# a real network call to Stripe. stripe_create_checkout_session() makes
# two chained requests (resolve the price, then create the session); the
# mock dispatches on req$url to answer each correctly.

test_that("stripe_create_checkout_session resolves the price and returns the session URL", {
  withr::local_envvar(STRIPE_SECRET_KEY_FILE = NA, STRIPE_SECRET_KEY = "sk_test_123")

  captured_session_req <- NULL
  httr2::local_mocked_responses(function(req) {
    if (grepl("/v1/prices", req$url, fixed = TRUE)) {
      return(httr2::response(
        status_code = 200,
      headers = list("Content-Type" = "application/json"),
        body = charToRaw(jsonlite::toJSON(
          list(data = list(list(id = "price_abc"))),
          auto_unbox = TRUE
        ))
      ))
    }
    captured_session_req <<- req
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(jsonlite::toJSON(
        list(id = "cs_123", url = "https://checkout.stripe.com/c/pay/cs_123"),
        auto_unbox = TRUE
      ))
    )
  })

  url <- stripe_create_checkout_session(
    practice_id = 42,
    price_lookup_key = "pro_monthly",
    customer_email = "doc@example.com",
    success_url = "https://app.directcareanalytics.com/?billing=success",
    cancel_url = "https://app.directcareanalytics.com/?billing=cancel"
  )

  expect_equal(url, "https://checkout.stripe.com/c/pay/cs_123")
  expect_true(grepl("/v1/checkout/sessions", captured_session_req$url, fixed = TRUE))
  # req_body_form() percent-encodes values and wraps them AsIs, reflecting
  # the actual outgoing application/x-www-form-urlencoded wire format --
  # decode before comparing.
  body_data <- lapply(captured_session_req$body$data, function(x) utils::URLdecode(as.character(x)))
  expect_equal(body_data$mode, "subscription")
  expect_equal(body_data$client_reference_id, "42")
  expect_equal(body_data$customer_email, "doc@example.com")
  expect_equal(body_data[["line_items[0][price]"]], "price_abc")
  expect_equal(body_data$allow_promotion_codes, "true")
})

test_that("stripe_create_checkout_session errors if no active price matches the lookup_key", {
  withr::local_envvar(STRIPE_SECRET_KEY_FILE = NA, STRIPE_SECRET_KEY = "sk_test_123")
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(jsonlite::toJSON(list(data = list()), auto_unbox = TRUE))
    )
  })

  expect_error(
    stripe_create_checkout_session(1, "nonexistent_tier", "x@example.com", "https://x", "https://x"),
    "nonexistent_tier"
  )
})

test_that("stripe_create_checkout_session errors if the secret key is not configured", {
  withr::local_envvar(STRIPE_SECRET_KEY_FILE = NA, STRIPE_SECRET_KEY = "")

  expect_error(
    stripe_create_checkout_session(1, "pro_monthly", "x@example.com", "https://x", "https://x"),
    "Stripe secret key"
  )
})

test_that("stripe_create_portal_session returns the portal URL", {
  withr::local_envvar(STRIPE_SECRET_KEY_FILE = NA, STRIPE_SECRET_KEY = "sk_test_123")
  captured_req <- NULL
  httr2::local_mocked_responses(function(req) {
    captured_req <<- req
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(jsonlite::toJSON(
        list(id = "bps_1", url = "https://billing.stripe.com/p/session/bps_1"),
        auto_unbox = TRUE
      ))
    )
  })

  url <- stripe_create_portal_session("cus_1", "https://app.directcareanalytics.com/account")

  expect_equal(url, "https://billing.stripe.com/p/session/bps_1")
  expect_equal(utils::URLdecode(as.character(captured_req$body$data$customer)), "cus_1")
  expect_equal(
    utils::URLdecode(as.character(captured_req$body$data$return_url)),
    "https://app.directcareanalytics.com/account"
  )
})
