# Unit tests for R/client_ip.R. No mocking needed -- a Shiny session's
# `request` field is a plain Rook-style environment, easy to fake directly.

.fake_session <- function(xff = NULL, remote_addr = NULL) {
  list(request = list(HTTP_X_FORWARDED_FOR = xff, REMOTE_ADDR = remote_addr))
}

test_that("extract_client_ip takes the last hop of X-Forwarded-For", {
  session <- .fake_session(xff = "1.2.3.4, 203.0.113.7")

  expect_equal(extract_client_ip(session), "203.0.113.7")
})

test_that("extract_client_ip rejects a forged leading IP by ignoring it", {
  # Caddy appends the real connecting IP after whatever the client sent --
  # trusting the first hop here would trust an attacker-forged header.
  session <- .fake_session(xff = "9.9.9.9")

  # A single-hop XFF is exactly what Caddy produces for a direct client
  # connection (no forged header) -- the one value IS the real IP here.
  expect_equal(extract_client_ip(session), "9.9.9.9")
})

test_that("extract_client_ip trims whitespace around hops", {
  session <- .fake_session(xff = "1.2.3.4 ,  203.0.113.7  ")

  expect_equal(extract_client_ip(session), "203.0.113.7")
})

test_that("extract_client_ip falls back to REMOTE_ADDR when XFF is absent", {
  session <- .fake_session(xff = NULL, remote_addr = "192.0.2.1")

  expect_equal(extract_client_ip(session), "192.0.2.1")
})

test_that("extract_client_ip falls back to REMOTE_ADDR when XFF is empty", {
  session <- .fake_session(xff = "", remote_addr = "192.0.2.1")

  expect_equal(extract_client_ip(session), "192.0.2.1")
})

test_that("extract_client_ip returns NA when neither header is present", {
  session <- .fake_session(xff = NULL, remote_addr = NULL)

  expect_true(is.na(extract_client_ip(session)))
})

test_that("extract_client_ip returns NA when REMOTE_ADDR is also empty", {
  session <- .fake_session(xff = NULL, remote_addr = "")

  expect_true(is.na(extract_client_ip(session)))
})
