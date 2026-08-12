#' Attach Stripe's own error message to a request's failure conditions
#'
#' httr2's default error message for a non-2xx response is just the status
#' line (e.g. "HTTP 400 Bad Request") -- unhelpful for debugging, since
#' Stripe's response body almost always carries the real reason
#' (`error.message`) even on failure. Every Stripe request in this package
#' pipes through this so a caller's `tryCatch(... conditionMessage(e) ...)`
#' (see utils_billing.R in both apps) surfaces something actionable instead
#' of a bare status code.
#' @noRd
.stripe_req_error <- function(req) {
  httr2::req_error(req, body = function(resp) {
    tryCatch(httr2::resp_body_json(resp)$error$message, error = function(e) NULL)
  })
}

#' Read the Stripe secret/restricted API key
#'
#' Mirrors directCareAuth::db_connect()'s `*_FILE`-env-var-with-fallback
#' pattern -- the key comes from a Docker secret file
#' (`STRIPE_SECRET_KEY_FILE`) when present, falling back to a plain
#' `STRIPE_SECRET_KEY` env var for local dev.
#' @noRd
.stripe_secret_key <- function() {
  key_file <- Sys.getenv("STRIPE_SECRET_KEY_FILE", unset = NA)
  if (!is.na(key_file) && file.exists(key_file)) {
    return(trimws(readLines(key_file, warn = FALSE)))
  }
  Sys.getenv("STRIPE_SECRET_KEY", unset = "")
}

#' Read the Stripe webhook endpoint's signing secret
#'
#' Same `_FILE`-with-fallback convention as `.stripe_secret_key()` --
#' `STRIPE_WEBHOOK_SECRET_FILE` (a Docker secret) takes precedence over a
#' plain `STRIPE_WEBHOOK_SECRET` env var for local dev. The billing-service
#' plumber route (Part 4) calls this rather than reading the env var
#' directly, so both secrets go through one consistent resolution path.
#' @noRd
.stripe_webhook_secret <- function() {
  secret_file <- Sys.getenv("STRIPE_WEBHOOK_SECRET_FILE", unset = NA)
  if (!is.na(secret_file) && file.exists(secret_file)) {
    return(trimws(readLines(secret_file, warn = FALSE)))
  }
  Sys.getenv("STRIPE_WEBHOOK_SECRET", unset = "")
}

#' Map Stripe Price `lookup_key`s to internal `plan_tier` values
#'
#' Env-var-driven, not hardcoded (see STRIPE_BILLING.md Part 3): every
#' `STRIPE_PRICE_<TIER>` env var (e.g. `STRIPE_PRICE_PRO=pro_monthly`)
#' contributes one `lookup_key -> tier` entry, with `<TIER>` lowercased
#' as the tier name. Keeps Test-mode vs Live-mode price references (and
#' the eventual real tier names, not yet decided) out of package code
#' entirely.
#' @noRd
.stripe_price_tier_map <- function() {
  all_env <- Sys.getenv()
  price_vars <- all_env[grepl("^STRIPE_PRICE_", names(all_env)) & nzchar(all_env)]
  if (length(price_vars) == 0) {
    return(list())
  }
  tiers <- tolower(sub("^STRIPE_PRICE_", "", names(price_vars)))
  stats::setNames(as.list(tiers), unname(price_vars))
}

#' Constant-time string comparison
#'
#' Ordinary `==` short-circuits on the first differing byte, which leaks
#' timing information an attacker could use to guess a valid signature
#' one byte at a time. Compares every byte regardless of where the first
#' mismatch is. Case-insensitive -- Stripe's hex digests are lowercase,
#' but this guards against a caller passing mixed case.
#' @noRd
.timing_safe_equal <- function(a, b) {
  a <- tolower(a)
  b <- tolower(b)
  if (nchar(a) != nchar(b)) {
    return(FALSE)
  }
  a_bytes <- as.integer(charToRaw(a))
  b_bytes <- as.integer(charToRaw(b))
  diff <- 0L
  for (i in seq_along(a_bytes)) {
    diff <- bitwOr(diff, bitwXor(a_bytes[i], b_bytes[i]))
  }
  diff == 0L
}

#' Verify a Stripe webhook signature
#'
#' Stripe signs each webhook with HMAC-SHA256 over `"{timestamp}.{raw_body}"`,
#' keyed on the endpoint's signing secret, sent as the `Stripe-Signature`
#' header (`t=...,v1=...`, possibly multiple `v1=` values during a secret
#' rotation -- matching any one of them is sufficient). Must be computed
#' against the *raw* request body text, not a re-serialized/re-parsed
#' version of it -- any whitespace/key-order difference from re-encoding
#' breaks the signature. Also rejects anything older than `tolerance_seconds`
#' (the `t=` timestamp) to block replay of a captured request.
#'
#' @param payload_raw The raw request body, as a single string (e.g.
#'   plumber's `req$postBody`) -- not parsed JSON, and not re-encoded.
#' @param sig_header The raw `Stripe-Signature` header value.
#' @param secret The endpoint's `whsec_...` signing secret.
#' @param tolerance_seconds Reject if `t=` is older than this. Default 300.
#' @return `TRUE`/`FALSE`.
#' @export
stripe_verify_webhook_signature <- function(payload_raw, sig_header, secret,
                                             tolerance_seconds = 300) {
  if (!is.character(sig_header) || length(sig_header) != 1 || !nzchar(sig_header)) {
    return(FALSE)
  }
  if (!is.character(secret) || length(secret) != 1 || !nzchar(secret)) {
    return(FALSE)
  }

  parts <- strsplit(sig_header, ",", fixed = TRUE)[[1]]
  timestamp_str <- NULL
  v1_sigs <- character(0)
  for (part in parts) {
    eq_pos <- regexpr("=", part, fixed = TRUE)
    if (eq_pos < 0) {
      next
    }
    key <- substr(part, 1, eq_pos - 1)
    value <- substr(part, eq_pos + 1, nchar(part))
    if (key == "t") {
      timestamp_str <- value
    } else if (key == "v1") {
      v1_sigs <- c(v1_sigs, value)
    }
  }

  if (is.null(timestamp_str) || length(v1_sigs) == 0) {
    return(FALSE)
  }

  timestamp <- suppressWarnings(as.numeric(timestamp_str))
  if (is.na(timestamp)) {
    return(FALSE)
  }
  if (abs(as.numeric(Sys.time()) - timestamp) > tolerance_seconds) {
    return(FALSE)
  }

  signed_payload <- paste0(timestamp_str, ".", payload_raw)
  expected <- as.character(openssl::sha256(charToRaw(signed_payload), key = charToRaw(secret)))

  any(vapply(v1_sigs, function(v1) .timing_safe_equal(expected, v1), logical(1)))
}

#' Fetch a Stripe Subscription object by id
#'
#' `checkout.session.completed`'s own payload only carries the
#' Subscription's id (`obj$subscription`), not its `status`/
#' `current_period_end` -- those live on the Subscription object itself,
#' fetched here the same way `.stripe_resolve_plan_tier()` fetches the
#' Checkout Session's line items. Do **not** use the Checkout Session's
#' own `status` field for `subscription_status` -- that's a *Checkout
#' Session* status (`open`/`complete`/`expired`), an entirely different
#' vocabulary from a *Subscription*'s status (`active`/`trialing`/
#' `past_due`/...) that this column is documented to hold. Confirmed live
#' in production (2026-08-12): every fresh Checkout was writing the
#' literal string `"complete"` into `subscription_status` instead of the
#' Subscription's real `"active"`, with `current_period_end` left blank
#' until (if ever) a later `customer.subscription.updated` event happened
#' to arrive and correct it.
#' @noRd
.stripe_fetch_subscription <- function(subscription_id) {
  httr2::request(paste0("https://api.stripe.com/v1/subscriptions/", subscription_id)) |>
    httr2::req_auth_bearer_token(.stripe_secret_key()) |>
    .stripe_req_error() |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

#' Look up the practice a Stripe Checkout Session's line item resolves to
#'
#' Fetches the session's line items (not included on the webhook event
#' payload by default) to read the purchased Price's `lookup_key`, then
#' maps it to an internal `plan_tier` via the env-var-driven price-tier map.
#' Errors loudly (not a silent "free" fallback) if the lookup_key has no
#' configured mapping -- a real purchase silently downgrading to free
#' would be a much worse failure mode than a webhook handler erroring
#' visibly in the logs.
#' @noRd
.stripe_resolve_plan_tier <- function(checkout_session_id) {
  secret_key <- .stripe_secret_key()
  resp <- httr2::request(paste0(
    "https://api.stripe.com/v1/checkout/sessions/", checkout_session_id, "/line_items"
  )) |>
    httr2::req_auth_bearer_token(secret_key) |>
    .stripe_req_error() |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (length(resp$data) == 0) {
    stop("Checkout Session ", checkout_session_id, " has no line items")
  }
  lookup_key <- resp$data[[1]]$price$lookup_key
  if (is.null(lookup_key) || !nzchar(lookup_key)) {
    stop("Checkout Session ", checkout_session_id, "'s Price has no lookup_key set")
  }

  tier_map <- .stripe_price_tier_map()
  tier <- tier_map[[lookup_key]]
  if (is.null(tier)) {
    stop(
      "No plan_tier configured for Stripe Price lookup_key '", lookup_key,
      "' (expected a STRIPE_PRICE_<TIER> env var pointing to it)"
    )
  }
  tier
}

#' Dispatch one verified Stripe event into `practices`
#'
#' Idempotency (see the `stripe_webhook_events` table and the webhook
#' route's own insert-before-processing check) is the caller's
#' responsibility -- this function assumes the event hasn't been applied
#' yet. Handles the three events that actually change a practice's
#' billing state; any other
#' `event$type` is a silent no-op (Stripe sends many event types this app
#' doesn't act on, and erroring on those would just make retries pile up
#' for no reason).
#'
#' - `checkout.session.completed`: first-time subscribe. Looks up the
#'   practice by `client_reference_id`, stores `stripe_customer_id`/
#'   `stripe_subscription_id`, resolves `plan_tier` from the session's
#'   line-item price lookup_key, sets `subscription_status`.
#' - `customer.subscription.updated`: plan change, renewal, past_due/
#'   unpaid transition -- refreshes `subscription_status`/`plan_tier`/
#'   `current_period_end` from the Subscription object, looked up by
#'   `stripe_customer_id` (not `client_reference_id` -- that field only
#'   exists on the original Checkout Session, not later Subscription
#'   events).
#' - `customer.subscription.deleted`: cancellation took effect --
#'   `subscription_status = "canceled"`, `plan_tier = "free"`.
#'
#' @param con An open `DBI` connection.
#' @param event A parsed Stripe event (a nested list, e.g. from
#'   `jsonlite::fromJSON(..., simplifyVector = FALSE)`).
#' @return Invisibly, `TRUE` if the event changed a row, `FALSE` if it
#'   was a recognized-but-no-op type or matched no practice.
#' @export
stripe_handle_webhook_event <- function(con, event) {
  obj <- event$data$object

  if (event$type == "checkout.session.completed") {
    practice_id <- obj$client_reference_id
    if (is.null(practice_id) || !nzchar(practice_id)) {
      stop("checkout.session.completed event has no client_reference_id")
    }
    plan_tier <- .stripe_resolve_plan_tier(obj$id)
    subscription <- .stripe_fetch_subscription(obj$subscription)
    period_end <- if (is.null(subscription$current_period_end)) {
      NA
    } else {
      as.POSIXct(subscription$current_period_end, origin = "1970-01-01", tz = "UTC")
    }
    n <- DBI::dbExecute(
      con,
      "UPDATE practices
         SET stripe_customer_id = $1, stripe_subscription_id = $2,
             plan_tier = $3, subscription_status = $4, current_period_end = $5
         WHERE id = $6",
      params = list(
        obj$customer, obj$subscription, plan_tier,
        subscription$status, period_end, as.integer(practice_id)
      )
    )
    return(invisible(n > 0))
  }

  if (event$type == "customer.subscription.updated") {
    lookup_key <- obj$items$data[[1]]$price$lookup_key
    tier_map <- .stripe_price_tier_map()
    # NA, not NULL -- an unresolved lookup_key (a Price with no
    # STRIPE_PRICE_<TIER> mapping yet, or none at all) is a real,
    # expected case, not a bug: DBI params must each be length-1, and a
    # bare NULL here (length 0) makes dbExecute() error ("Parameter 2
    # does not have length 1") rather than reach the SQL layer at all,
    # where COALESCE($2, plan_tier) is what actually implements "leave
    # plan_tier untouched when unresolved."
    plan_tier <- if (!is.null(lookup_key)) tier_map[[lookup_key]] else NA_character_
    if (is.null(plan_tier)) plan_tier <- NA_character_
    period_end <- if (is.null(obj$current_period_end)) {
      NA
    } else {
      as.POSIXct(obj$current_period_end, origin = "1970-01-01", tz = "UTC")
    }
    n <- DBI::dbExecute(
      con,
      "UPDATE practices
         SET subscription_status = $1,
             plan_tier = COALESCE($2, plan_tier),
             current_period_end = $3
         WHERE stripe_customer_id = $4",
      params = list(obj$status, plan_tier, period_end, obj$customer)
    )
    return(invisible(n > 0))
  }

  if (event$type == "customer.subscription.deleted") {
    n <- DBI::dbExecute(
      con,
      "UPDATE practices
         SET subscription_status = 'canceled', plan_tier = 'free'
         WHERE stripe_customer_id = $1",
      params = list(obj$customer)
    )
    return(invisible(n > 0))
  }

  invisible(FALSE)
}
