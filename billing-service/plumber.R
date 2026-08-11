# Stripe webhook receiver -- see STRIPE_BILLING.md Part 4 for why this is
# its own service rather than a route inside DCA/Planner: Shiny apps are
# built around a reactive websocket session model, with no clean way to
# serve an arbitrary raw-body JSON POST endpoint from inside shinyApp().
#
# Every route here is deliberately thin -- all real logic (signature
# verification, event dispatch) lives in `directCareBilling`, already unit-
# tested there (test-webhook.R). This file's only job is the plumbing:
# read the raw body/header, enforce webhook idempotency (the
# stripe_webhook_events insert-before-processing check), and translate the
# result into an HTTP response.

# Shared Postgres connection pool (Performance Backlog Phase 3) -- top-
# level code in a plumber router file runs once, when plumber::plumb()
# parses it, so this pre-warms the pool before the first request the same
# way both Shiny apps' run_app.R does. No onStop() equivalent here (this
# minimal setup has no such hook) -- the container's own process exit
# reclaims the connections, and this service has no other in-process
# state that needs a clean-shutdown path either.
directCareAuth::db_pool()

#* @get /healthz
#* @serializer unboxedJSON
function() {
  list(status = "ok")
}

#* @post /webhook
#* @serializer unboxedJSON
function(req, res) {
  # plumber exposes the raw, unparsed request body via `req$postBody` --
  # signature verification MUST run against these exact bytes (see
  # stripe_verify_webhook_signature()'s own docs), not a re-serialized
  # version of whatever jsonlite::fromJSON() below produces.
  sig_header <- req$HTTP_STRIPE_SIGNATURE
  secret <- directCareBilling:::.stripe_webhook_secret()

  ok <- directCareBilling::stripe_verify_webhook_signature(req$postBody, sig_header, secret)
  if (!isTRUE(ok)) {
    res$status <- 400
    return(list(error = "bad signature"))
  }

  event <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

  con <- directCareAuth::db_checkout()
  on.exit(directCareAuth::db_release(con), add = TRUE)

  # Idempotency: Stripe retries delivery for up to 72 hours on anything
  # other than a 2xx response, and can also just send the same event twice
  # in normal at-least-once-delivery operation. Insert BEFORE any
  # state-changing work; a unique-violation (0 rows affected, since this is
  # ON CONFLICT DO NOTHING rather than an error) means "already processed."
  inserted <- DBI::dbExecute(
    con,
    "INSERT INTO stripe_webhook_events (event_id, event_type) VALUES ($1, $2)
       ON CONFLICT (event_id) DO NOTHING",
    params = list(event$id, event$type)
  )
  if (inserted == 0) {
    return(list(status = "already processed"))
  }

  directCareBilling::stripe_handle_webhook_event(con, event)
  list(status = "ok")
}
