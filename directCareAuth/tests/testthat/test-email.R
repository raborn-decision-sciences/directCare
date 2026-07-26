# Unit tests for R/email.R (send_password_reset_email()). Uses httr2's own
# local_mocked_responses() -- the officially supported way to intercept an
# httr2 request before it's ever sent, so these never make a real network
# call to ZeptoMail. (A naive testthat local_mocked_bindings() on the
# intermediate httr2 pipe functions doesn't work here: httr2's request-
# building calls are lazily evaluated as unforced promises inside the
# mocked req_perform() call, so they're never actually invoked unless
# req_perform() itself touches its argument -- local_mocked_responses()
# sidesteps this entirely by intercepting at the one point httr2 itself
# guarantees is reached.)

test_that("send_password_reset_email posts to ZeptoMail with the right auth header and body", {
  withr::local_envvar(
    ZEPTOMAIL_TOKEN_FILE = NA,
    ZEPTOMAIL_TOKEN = "test-token-123",
    FROM_EMAIL = "noreply@directcareanalytics.com"
  )

  captured_req <- NULL
  httr2::local_mocked_responses(function(req) {
    captured_req <<- req
    httr2::response(status_code = 200, body = charToRaw("{}"))
  })

  send_password_reset_email(
    "doc@example.com", "River DPC",
    "https://app.directcareanalytics.com/?reset=1&token=abc123"
  )

  expect_equal(captured_req$url, "https://api.zeptomail.com/v1.1/email")
  expect_false(is.null(captured_req$headers[["Authorization"]]))
  expect_equal(captured_req$body$data$from$address, "noreply@directcareanalytics.com")
  expect_equal(captured_req$body$data$to[[1]]$email_address$address, "doc@example.com")
  expect_true(grepl("River DPC", captured_req$body$data$htmlbody, fixed = TRUE))
  # The URL is HTML-escaped in the body (& becomes &amp;), so check for the
  # token substring rather than the raw, unescaped URL.
  expect_true(grepl("token=abc123", captured_req$body$data$htmlbody, fixed = TRUE))
})

test_that("send_password_reset_email HTML-escapes the practice name", {
  withr::local_envvar(
    ZEPTOMAIL_TOKEN_FILE = NA, ZEPTOMAIL_TOKEN = "tok",
    FROM_EMAIL = "noreply@example.com"
  )
  captured_req <- NULL
  httr2::local_mocked_responses(function(req) {
    captured_req <<- req
    httr2::response(status_code = 200, body = charToRaw("{}"))
  })

  send_password_reset_email("doc@example.com", "<script>bad</script>", "https://x/reset")

  htmlbody <- captured_req$body$data$htmlbody
  expect_false(grepl("<script>", htmlbody, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;", htmlbody, fixed = TRUE))
})

test_that("send_password_reset_email errors if the API token is not configured", {
  withr::local_envvar(
    ZEPTOMAIL_TOKEN_FILE = NA, ZEPTOMAIL_TOKEN = "",
    FROM_EMAIL = "noreply@example.com"
  )

  expect_error(
    send_password_reset_email("doc@example.com", "River DPC", "https://x/reset"),
    "ZeptoMail"
  )
})

test_that("send_password_reset_email errors if FROM_EMAIL is not configured", {
  withr::local_envvar(
    ZEPTOMAIL_TOKEN_FILE = NA, ZEPTOMAIL_TOKEN = "test-token",
    FROM_EMAIL = ""
  )

  expect_error(
    send_password_reset_email("doc@example.com", "River DPC", "https://x/reset"),
    "FROM_EMAIL"
  )
})
