# -- Paywall gating helpers ----------------------------------------------
# Shared across every gate point in this app (currently just mod_upload.R's
# "Use Real Data" entry point -- see STRIPE_BILLING.md's v1 gating scope
# for the full list, most of which isn't wired up yet). Kept here, not in
# `directCareBilling`, since that package is a plain backend integration
# layer with no Shiny dependency (see its own DESCRIPTION) -- UI is each
# app's own concern per STRIPE_BILLING.md Part 5.

#' Whether a `plan_tier` value has access to gated (paid) features
#'
#' @param plan_tier A `plan_tier` string, e.g. `r$plan_tier` -- `"free"`,
#'   `"starter"`, or `"pro"` today, but deliberately not an exhaustive
#'   `%in%` allowlist of just those three: any *future* paid tier name
#'   should also pass without this function needing an update, so the
#'   actual test is "not free/NULL/unrecognized", not "is one of these
#'   specific paid names".
#' @return `TRUE`/`FALSE`.
#' @noRd
.has_paid_plan <- function(plan_tier) {
  isTRUE(!is.null(plan_tier) && nzchar(plan_tier) && plan_tier != "free")
}

#' Whether a `plan_tier` value has access to Pro-exclusive features
#'
#' Unlike `.has_paid_plan()`, this genuinely distinguishes Starter from
#' Pro -- see the goal-seek feature gated on it in mod_projections.R
#' (mirrors directCarePlanner's identical `.has_pro_plan()` and its
#' sensitivity-decomposition/goal-seek Pro features; see TODO.md's
#' "In-app Plans modal oversells Pro" entry for the history here).
#'
#' @param plan_tier A `plan_tier` string, e.g. `r$plan_tier`.
#' @return `TRUE`/`FALSE`.
#' @noRd
.has_pro_plan <- function(plan_tier) {
  isTRUE(!is.null(plan_tier) && identical(plan_tier, "pro"))
}

#' Stripe Price `lookup_key`s for the two paid tiers
#'
#' Hardcoded (not env-var-driven) deliberately -- these are values *this
#' app* sends to Stripe when starting a Checkout Session, distinct from
#' `directCareBilling`'s `STRIPE_PRICE_<TIER>` env vars, which the webhook
#' instead uses to map a `lookup_key` *back* to a `plan_tier` once a
#' purchase completes. Same string values on both sides is what makes a
#' Starter purchase actually land as `plan_tier = "starter"`; if these ever
#' change, `STRIPE_PRICE_STARTER`/`STRIPE_PRICE_PRO` (Docker secrets/env,
#' see STRIPE_BILLING.md Part 4) must change to match.
#' @noRd
STRIPE_LOOKUP_KEY_STARTER <- "starter_monthly"
#' @noRd
STRIPE_LOOKUP_KEY_PRO <- "pro_monthly"

#' Start a Stripe Checkout Session and redirect the browser to it
#'
#' Shared by every "Upgrade to ..." entry point (the plans modal below and
#' the post-signup plan picker, mod_signup.R) -- one place that calls
#' `directCareBilling::stripe_create_checkout_session()` and performs the
#' actual browser redirect, so neither call site duplicates the
#' success/cancel URL construction or error handling.
#'
#' A hosted Checkout Session URL isn't something `tags$a(href=...)` can
#' target from inside a `showModal()`/`renderUI()` -- the redirect has to
#' happen after the (synchronous, network-calling) session-creation request
#' returns, so this sends a custom message to the client-side
#' `redirectTo` handler (registered in both app_ui.R and mod_signup.R's
#' `signup_ui()`) rather than rendering a link.
#'
#' @param session The current Shiny session (top-level or a module's --
#'   `session$sendCustomMessage()` reaches the browser either way).
#' @param practice_id,email The purchasing practice's id/email --
#'   `client_reference_id`/prefilled email on the Checkout Session (see
#'   `stripe_create_checkout_session()`'s own docs for why
#'   `client_reference_id` matters).
#' @param price_lookup_key One of the `STRIPE_LOOKUP_KEY_*` constants above.
#' @noRd
.start_stripe_checkout <- function(session, practice_id, email, price_lookup_key) {
  base_url <- Sys.getenv("APP_BASE_URL", unset = "")
  url <- tryCatch(
    directCareBilling::stripe_create_checkout_session(
      practice_id = practice_id,
      price_lookup_key = price_lookup_key,
      customer_email = email,
      success_url = paste0(base_url, "/?billing=success"),
      cancel_url = paste0(base_url, "/?billing=cancelled")
    ),
    error = function(e) {
      warning("stripe_create_checkout_session() failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(url)) {
    showNotification(
      "Couldn't start checkout right now. Please try again in a moment.",
      type = "error",
      duration = 8
    )
    return(invisible(FALSE))
  }
  removeModal()
  session$sendCustomMessage("redirectTo", list(url = url))
  invisible(TRUE)
}

#' Start a Stripe Billing Portal Session and redirect the browser to it
#'
#' Same redirect mechanism as `.start_stripe_checkout()` -- see its own
#' comment. Used only by Account Settings' "Manage Billing" button, which
#' is itself only shown once `r$stripe_customer_id` is set (a practice with
#' no completed Checkout has no Stripe Customer for the Portal to manage).
#'
#' @param session The current Shiny session.
#' @param stripe_customer_id The practice's `cus_...` id (`r$stripe_customer_id`).
#' @param return_url Where Stripe sends the browser back after the Portal.
#' @noRd
.start_stripe_portal <- function(session, stripe_customer_id, return_url) {
  url <- tryCatch(
    directCareBilling::stripe_create_portal_session(
      stripe_customer_id = stripe_customer_id,
      return_url = return_url
    ),
    error = function(e) {
      warning("stripe_create_portal_session() failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(url)) {
    showNotification(
      "Couldn't open the billing portal right now. Please try again in a moment.",
      type = "error",
      duration = 8
    )
    return(invisible(FALSE))
  }
  removeModal()
  session$sendCustomMessage("redirectTo", list(url = url))
  invisible(TRUE)
}

#' Modal listing the plan tiers, shown from any "See plans" trigger
#'
#' Only ever shown to a free-tier practice (every call site gates on
#' `!.has_paid_plan(r$plan_tier)` before showing it), so both upgrade
#' buttons are always relevant -- no "already on this plan" branching
#' needed. The two buttons use plain, unnamespaced ids ("btn_checkout_
#' starter"/"btn_checkout_pro") rather than `ns(...)`-scoped ones:
#' `showModal()` content is appended directly to `<body>`, not into the
#' calling module's own DOM subtree, so Shiny's module namespacing doesn't
#' apply to it regardless of which module (mod_upload.R, mod_results.R, ...)
#' triggered the modal -- one pair of top-level `observeEvent()`s in
#' app_server.R handles both, matching the Account Settings modal's own
#' plain-id convention. Tier names/prices are hardcoded here rather than
#' read from Stripe live -- this is marketing copy, not a place that needs
#' to reflect a `lookup_key` repricing instantly; update both here and
#' STRIPE_BILLING.md together if pricing changes.
#' @noRd
.show_plans_modal <- function() {
  showModal(modalDialog(
    title = tagList(bs_icon("stars"), " Plans"),
    size = "l",
    easyClose = TRUE,
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        class = "h-100",
        card_header("Plan"),
        card_body(
          tags$p(class = "fw-bold fs-4 mb-1", "Free"),
          tags$p(class = "text-muted small mb-0", "Limited planning tools and calculators.")
        )
      ),
      card(
        class = "h-100 border-primary",
        card_header(tagList(bs_icon("graph-up-arrow"), " Starter")),
        card_body(
          tags$p(class = "fw-bold fs-4 mb-1", "$39/mo"),
          tags$p(
            class = "text-muted small mb-0",
            "Bookkeeping uploads, saved practice profile, historical ",
            "trends, break-even analysis, downloadable reports."
          ),
          # mt-auto pins the button to the bottom of the (flex-column, h-100)
          # card body, so it lines up with Pro's regardless of which
          # description wraps to more lines -- same technique as
          # mod_upload.R's path-selection cards (see .locked_feature_card()'s
          # own comment for the fuller rationale).
          tags$div(
            class = "mt-auto",
            # white-space:nowrap isn't a Bootstrap .btn default -- without
            # it, this wraps mid-word the moment the 3-column
            # layout_columns() squeezes narrower than the icon+label's
            # natural width (confirmed live: "Upgrade" alone still wrapped
            # to "Upg"/"rade" at narrow widths before this was added).
            # btn-sm's tighter padding buys back the horizontal room a 3-up
            # column at "l" modal size doesn't have to spare -- plain
            # .btn's default 1.5rem side padding left "Upgrade"+icon
            # clipped even with wrapping fixed.
            actionButton(
              "btn_checkout_starter", "Upgrade",
              icon = bs_icon("arrow-up-right-circle"),
              class = "btn-primary btn-sm w-100 mt-2",
              style = "white-space: nowrap;"
            )
          )
        )
      ),
      card(
        class = "h-100",
        card_header(tagList(bs_icon("lightbulb"), " Pro")),
        card_body(
          tags$p(class = "fw-bold fs-4 mb-1", "$79/mo"),
          tags$p(
            class = "text-muted small mb-0",
            "Everything in Starter, plus guided decision tools and ",
            "AI-generated financial interpretation."
          ),
          tags$div(
            class = "mt-auto",
            actionButton(
              "btn_checkout_pro", "Upgrade",
              icon = bs_icon("arrow-up-right-circle"),
              class = "btn-outline-primary btn-sm w-100 mt-2",
              style = "white-space: nowrap;"
            )
          )
        )
      )
    ),
    tags$p(
      class = "text-center text-muted small mt-3 mb-0",
      "You'll be taken to Stripe's secure checkout. Have questions about plans? ",
      tags$a(
        href = "mailto:anthony@raborndecisionsciences.com?subject=Question%20about%20plans",
        "Email us"
      ),
      "."
    ),
    footer = modalButton("Close")
  ))
}

#' Card shown in place of a gated feature for a free-tier, non-demo user
#'
#' @param title Card header text.
#' @param description Body copy explaining what the feature does.
#' @param ns The calling module's namespacing function (`session$ns`), so
#'   the "See plans" button gets a properly namespaced id per module.
#' @param btn_id Suffix for the "See plans" button's id. Only needs
#'   changing from the default if a single module ever renders more than
#'   one locked card at once (none do today).
#' @param extra Optional additional UI (e.g. a "Take the tour" link) shown
#'   below the "See plans" button, for parity with the real card it
#'   replaces.
#' @param class Passed straight to the wrapping `card()` -- matches
#'   whatever layout class the real card used (e.g. `"h-100"` inside a
#'   `layout_columns()`).
#' @noRd
.locked_feature_card <- function(title, description, ns, btn_id = "btn_see_plans",
                                  extra = NULL, class = "h-100") {
  card(
    class = class,
    card_header(tagList(bs_icon("lock-fill"), " ", title)),
    card_body(
      tags$p(description),
      tags$p(
        class = "small fw-semibold mb-3",
        style = "color: #B45309;",
        bs_icon("stars"), " Starter or Pro plan required"
      ),
      # mt-auto pins this to the bottom of the (flex-column) card body, so
      # its button+link line up with the other path-selection cards'
      # regardless of how many lines `description` wraps to -- see
      # mod_upload.R's own path-selection cards for the sibling markup this
      # has to match.
      tags$div(
        class = "mt-auto",
        actionButton(
          ns(btn_id),
          "See plans",
          icon = bs_icon("arrow-up-right-circle"),
          class = "btn-outline-primary w-100"
        ),
        extra
      )
    )
  )
}

#' Small inline upsell note for a Pro-only addition to content every tier
#' already sees
#'
#' Unlike `.locked_feature_card()`, which replaces an entire gated feature,
#' this sits alongside content that's already free (e.g. the base
#' break-even interpretation) -- it's advertising an extra paragraph, not
#' standing in for a missing one. Ported from directCarePlanner's identical
#' helper -- see its own comment for the fuller rationale.
#'
#' @param ns The calling module's namespacing function (`session$ns`).
#' @param text Body copy describing what upgrading to Pro adds.
#' @param btn_id Suffix for the "See plans" link's id. Must be unique per
#'   call site within the same module if it renders more than one.
#' @noRd
.pro_upsell_note <- function(ns, text, btn_id = "btn_see_plans_pro") {
  tags$p(
    class = "small text-muted mt-2 mb-0",
    bs_icon("stars"), " ",
    text,
    " ",
    actionLink(ns(btn_id), "See Pro plan", class = "small")
  )
}

#' Save/Load scenario-slot buttons -- real ones for a paid practice, a
#' locked placeholder for free
#'
#' `directCareScenarios::mod_scenario_slots_ui(id)` renders unconditionally
#' at every call site (DCA's Upload/Edit/Summary/Projections nav footers,
#' plus its own copy inside the Calculator module) -- this is the one place
#' that decides, per call site, whether the real buttons or a locked
#' stand-in renders, matching every other v1 gate's "swap the UI, not just
#' guard the server" treatment (see STRIPE_BILLING.md's gating scope).
#'
#' The actual enforcement lives server-side, in
#' `mod_scenario_slots_server()`'s own `has_access`/`on_access_denied`
#' params (STRIPE_BILLING.md notes why: that module's Save/Load observers
#' are registered once, unconditionally, regardless of what this function
#' renders) -- this UI swap alone is the "don't even show a free user a
#' button that would just bounce them" half.
#'
#' @param id The scenario-slots module id -- plain `"scenario"` for DCA's
#'   one global forecast-scenario instance (Upload/Edit/Summary/
#'   Projections all share it), or `ns("scenario")` for the Calculator's
#'   own self-namespaced instance.
#' @param plan_tier `r$plan_tier`.
#' @param btn_id The locked placeholder's button id -- plain
#'   `"btn_see_plans_scenario"` (matching `id = "scenario"`'s own
#'   unnamespaced convention, handled by one top-level observer in
#'   app_server.R) unless overridden by the Calculator's own `ns(...)`.
#' @noRd
.scenario_slots_ui <- function(id, plan_tier, btn_id = "btn_see_plans_scenario") {
  if (.has_paid_plan(plan_tier)) {
    directCareScenarios::mod_scenario_slots_ui(id)
  } else {
    .locked_scenario_slots_ui(btn_id)
  }
}

#' @noRd
.locked_scenario_slots_ui <- function(btn_id = "btn_see_plans_scenario") {
  actionButton(
    btn_id,
    tagList(bs_icon("lock-fill"), " Save/Load Scenario"),
    class = "btn-outline-secondary"
  )
}
