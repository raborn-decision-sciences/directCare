# -- Paywall gating helpers ----------------------------------------------
# Shared across every gate point in this app (currently mod_results.R's
# Market Context card and Download Report button -- see STRIPE_BILLING.md's
# v1 gating scope for the full list). Kept here, not in `directCareBilling`,
# since that package is a plain backend integration layer with no Shiny
# dependency (see its own DESCRIPTION) -- UI is each app's own concern per
# STRIPE_BILLING.md Part 5. Ported from directCareAnalytics's identical
# file -- see its own comments for the fuller rationale; only the icon
# namespace (`bsicons::bs_icon`, matching this app's own convention,
# vs. DCA's unqualified `bs_icon`) differs.

#' Whether a `plan_tier` value has access to gated (paid) features
#'
#' @param plan_tier A `plan_tier` string, e.g. `r$plan_tier` -- `"free"`,
#'   `"starter"`, or `"pro"` today, but deliberately not an exhaustive
#'   `%in%` allowlist of just those three: any *future* paid tier name
#'   should also pass without this function needing an update.
#' @return `TRUE`/`FALSE`.
#' @noRd
.has_paid_plan <- function(plan_tier) {
  isTRUE(!is.null(plan_tier) && nzchar(plan_tier) && plan_tier != "free")
}

#' Stripe Price `lookup_key`s for the two paid tiers
#'
#' See directCareAnalytics's identical constants for the full rationale
#' (must match `STRIPE_PRICE_STARTER`/`STRIPE_PRICE_PRO`, the webhook's own
#' reverse mapping).
#' @noRd
STRIPE_LOOKUP_KEY_STARTER <- "starter_monthly"
#' @noRd
STRIPE_LOOKUP_KEY_PRO <- "pro_monthly"

#' Start a Stripe Checkout Session and redirect the browser to it
#'
#' See directCareAnalytics's identical helper for the full rationale.
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
#' See directCareAnalytics's identical helper for the full rationale.
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
#' See directCareAnalytics's identical helper for the full rationale
#' (plain unnamespaced button ids, tier copy kept in sync manually).
#' @noRd
.show_plans_modal <- function() {
  showModal(modalDialog(
    title = tagList(bsicons::bs_icon("stars"), " Plans"),
    size = "l",
    easyClose = TRUE,
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        card_header("Plan"),
        card_body(
          tags$p(class = "fw-bold fs-4 mb-1", "Free"),
          tags$p(class = "text-muted small mb-0", "Limited planning tools and calculators.")
        )
      ),
      card(
        class = "border-primary",
        card_header(tagList(bsicons::bs_icon("graph-up-arrow"), " Starter")),
        card_body(
          tags$p(class = "fw-bold fs-4 mb-1", "$39/mo"),
          tags$p(
            class = "text-muted small mb-0",
            "Bookkeeping uploads, saved practice profile, historical ",
            "trends, break-even analysis, downloadable reports."
          ),
          actionButton(
            "btn_checkout_starter", "Upgrade to Starter",
            icon = bsicons::bs_icon("arrow-up-right-circle"),
            class = "btn-primary w-100 mt-2"
          )
        )
      ),
      card(
        card_header(tagList(bsicons::bs_icon("lightbulb"), " Pro")),
        card_body(
          tags$p(class = "fw-bold fs-4 mb-1", "$79/mo"),
          tags$p(
            class = "text-muted small mb-0",
            "Everything in Starter, plus guided decision tools and ",
            "AI-generated financial interpretation."
          ),
          actionButton(
            "btn_checkout_pro", "Upgrade to Pro",
            icon = bsicons::bs_icon("arrow-up-right-circle"),
            class = "btn-outline-primary w-100 mt-2"
          )
        )
      )
    ),
    tags$p(
      class = "text-center text-muted small mt-3 mb-0",
      "You'll be taken to Stripe's secure checkout. Questions? ",
      tags$a(
        href = "mailto:anthony@raborndecisionsciences.com?subject=Question%20about%20plans",
        "email us"
      ),
      "."
    ),
    footer = modalButton("Close")
  ))
}

#' Card shown in place of a gated feature for a free-tier practice
#'
#' @param title Card header text.
#' @param description Body copy explaining what the feature does.
#' @param ns The calling module's namespacing function (`session$ns`), so
#'   the "See plans" button gets a properly namespaced id per module.
#' @param btn_id Suffix for the "See plans" button's id. Must be unique
#'   per call site within the same module if it renders more than one
#'   locked card (`mod_results.R` does: Market Context uses
#'   `"btn_see_plans_market"`, distinct from the Download Report gate's
#'   own button).
#' @param extra Optional additional UI shown below the "See plans" button.
#' @param class Passed straight to the wrapping `card()`, matching
#'   whatever the real card it replaces used -- `NULL` for a plain
#'   `card()` with no extra class.
#' @noRd
.locked_feature_card <- function(title, description, ns, btn_id = "btn_see_plans",
                                  extra = NULL, class = "h-100") {
  card(
    class = class,
    card_header(tagList(bsicons::bs_icon("lock-fill"), " ", title)),
    card_body(
      tags$p(description),
      tags$p(
        class = "small fw-semibold mb-3",
        style = "color: #B45309;",
        bsicons::bs_icon("stars"), " Starter or Pro plan required"
      ),
      actionButton(
        ns(btn_id),
        "See plans",
        icon = bsicons::bs_icon("arrow-up-right-circle"),
        class = "btn-outline-primary w-100"
      ),
      extra
    )
  )
}
