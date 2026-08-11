#' Resolve a Stripe Price `lookup_key` to its live `price_...` id
#'
#' Reference Prices by `lookup_key` everywhere in app code, never by the
#' raw id (see STRIPE_BILLING.md Part 1 #2) -- this is the one place that
#' resolution actually happens, via a live API call rather than an
#' env-var mapping, since a `lookup_key` can be re-pointed at a new Price
#' in the Dashboard at any time (a repricing, a promo) with no code
#' deploy.
#' @noRd
.stripe_resolve_price_id <- function(price_lookup_key, secret_key) {
  resp <- httr2::request("https://api.stripe.com/v1/prices") |>
    httr2::req_auth_bearer_token(secret_key) |>
    .stripe_req_error() |>
    httr2::req_url_query(
      "lookup_keys[]" = price_lookup_key,
      active = "true"
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (length(resp$data) == 0) {
    stop("No active Stripe price found for lookup_key '", price_lookup_key, "'")
  }
  resp$data[[1]]$id
}

#' Create a Stripe Checkout Session for a subscription and return its URL
#'
#' Hosted Checkout, `mode = "subscription"` -- see STRIPE_BILLING.md
#' Part 1 #3 for why this is used instead of a custom Elements form
#' (card data never touches the app; Stripe owns 3-D Secure/SCA and PCI
#' scope entirely).
#'
#' @param practice_id Threaded through as `client_reference_id` -- the
#'   only reliable link back to a `practices` row once the webhook fires
#'   (a Stripe Customer doesn't exist yet at session-creation time for a
#'   first-time subscriber, so nothing else is stable enough to key off).
#' @param price_lookup_key e.g. `"pro_monthly"` -- resolved to a live
#'   `price_...` id via a live API lookup, never hardcoded.
#' @param customer_email Prefills Checkout's email field.
#' @param success_url,cancel_url Where Stripe redirects afterward.
#' @return The Checkout Session's hosted URL -- caller redirects the
#'   browser there.
#' @export
stripe_create_checkout_session <- function(practice_id, price_lookup_key,
                                            customer_email, success_url, cancel_url) {
  secret_key <- .stripe_secret_key()
  if (!nzchar(secret_key)) {
    stop("Stripe secret key is not configured (STRIPE_SECRET_KEY_FILE/STRIPE_SECRET_KEY)")
  }
  price_id <- .stripe_resolve_price_id(price_lookup_key, secret_key)

  resp <- httr2::request("https://api.stripe.com/v1/checkout/sessions") |>
    httr2::req_auth_bearer_token(secret_key) |>
    .stripe_req_error() |>
    httr2::req_body_form(
      mode = "subscription",
      client_reference_id = as.character(practice_id),
      customer_email = customer_email,
      success_url = success_url,
      cancel_url = cancel_url,
      "line_items[0][price]" = price_id,
      "line_items[0][quantity]" = "1",
      # Off by default on every Checkout Session -- without this, Stripe
      # never renders an "Add promotion code" field on the hosted page at
      # all, regardless of what Coupons/Promotion Codes exist in the
      # Dashboard. Not a Dashboard setting; must be set per-session here.
      allow_promotion_codes = "true"
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  resp$url
}

#' Create a Billing Portal Session and return its URL
#'
#' Links a "Manage Billing" button to Stripe's own hosted Customer
#' Portal (plan changes, cancellation, card updates) -- see
#' STRIPE_BILLING.md Part 1 #4 for why no custom UI is built for this.
#'
#' @param stripe_customer_id The practice's `cus_...` id (`practices.stripe_customer_id`).
#' @param return_url Where Stripe redirects once the practice leaves the Portal.
#' @return The Portal Session's hosted URL.
#' @export
stripe_create_portal_session <- function(stripe_customer_id, return_url) {
  secret_key <- .stripe_secret_key()
  if (!nzchar(secret_key)) {
    stop("Stripe secret key is not configured (STRIPE_SECRET_KEY_FILE/STRIPE_SECRET_KEY)")
  }

  resp <- httr2::request("https://api.stripe.com/v1/billing_portal/sessions") |>
    httr2::req_auth_bearer_token(secret_key) |>
    .stripe_req_error() |>
    httr2::req_body_form(
      customer = stripe_customer_id,
      return_url = return_url
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  resp$url
}
