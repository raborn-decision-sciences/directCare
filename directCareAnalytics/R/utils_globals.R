# Suppress R CMD check NOTEs for bare column names and imported symbols
# used in data-masking and UI contexts throughout the app.

#' @importFrom bslib bs_add_rules bs_theme card card_body card_footer card_header input_dark_mode input_switch input_task_button layout_column_wrap layout_columns nav_item nav_panel nav_spacer navset_card_underline navset_tab page_fluid page_navbar page_sidebar sidebar accordion accordion_panel tooltip value_box
#' @importFrom bsicons bs_icon
#' @importFrom shiny NS moduleServer observeEvent reactive reactiveVal reactiveValues renderUI req uiOutput updateSelectInput updateDateRangeInput showNotification downloadButton downloadHandler actionButton fileInput textInput numericInput selectInput sliderInput dateInput dateRangeInput radioButtons renderPlot plotOutput icon tagList tags hr p HTML
#' @importFrom dplyr where
#' @importFrom stats setNames
#' @importFrom scales dollar dollar_format
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
#' @importFrom tools toTitleCase
#' @importFrom utils head tail
NULL

# NULL-coalescing operator for input$ values that are NULL before first render
`%||%` <- function(x, y) if (is.null(x)) y else x

#' JS handler that redirects the browser to a Stripe-hosted URL
#'
#' Registers the client-side half of `.start_stripe_checkout()`/
#' `stripe_create_portal_session()`'s redirect (utils_billing.R,
#' app_server.R): both call `session$sendCustomMessage("redirectTo", ...)`
#' after a Checkout/Portal Session is created server-side, since the
#' target URL doesn't exist until that (network-calling) request returns --
#' a plain `tags$a(href=...)` can't target it. `window.top.location.href`
#' (not `window.location.href`) so this still navigates the whole tab even
#' if Shiny is ever iframed. Needed on both the main app (app_ui.R) and the
#' signup page (mod_signup.R's `signup_ui()`, not wrapped by
#' shinymanager::secure_app() and so not sharing app_ui.R's own copy of
#' this) -- factored out here so both include the exact same handler.
#' @noRd
.redirect_script <- function() {
  tags$script(HTML(
    "$(document).on('shiny:connected', function() {
      Shiny.addCustomMessageHandler('redirectTo', function(msg) {
        window.top.location.href = msg.url;
      });
    });"
  ))
}

#' Branded header shared by the login page and the signup page
#'
#' Factored out of run_app.R's `tags_top` so the signup page (a plain Shiny
#' UI, not wrapped by shinymanager::secure_app()) can reuse the exact same
#' logo/heading/dark-mode-toggle markup rather than the two pages drifting
#' apart visually over time. `dark_mode_id` defaults to the login page's
#' unnamespaced "dark_mode" (matching app_server.R's own toggle handling);
#' the signup module passes its own namespaced id instead.
#' @noRd
.auth_page_chrome <- function(title, dark_mode_id = "dark_mode") {
  tags$div(
    style = "text-align:center;position:relative;",
    tags$div(
      style = "position:absolute;top:0;right:0;",
      input_dark_mode(id = dark_mode_id)
    ),
    tags$img(src = "www/favicon.svg", height = "48px", alt = "RDS"),
    tags$h4(
      # var(--bs-emphasis-color), not a literal hex: the page background
      # (unlike the navbar) correctly follows the theme, so a hardcoded
      # light-navy color would go dark-on-dark once dark mode is picked.
      style = "margin-top:8px;font-weight:600;color:var(--bs-emphasis-color);",
      title
    )
  )
}

# Lower-cased, dash-separated slug. Used for the PDF report filename
# (mod_projections.R) -- relocated here from mod_upload.R when the manual
# Practice ID field (which this used to auto-derive) was removed in favor
# of sourcing practice identity from res_auth.
.slugify <- function(x) {
  x <- tolower(trimws(x %||% ""))
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("^-+|-+$", "", x)
}

# App-wide dollar formatter — always two decimal places.
fmt_dollar <- function(x, ...) scales::dollar(x, accuracy = 0.01, ...)
fmt_dollar_format <- function(...) scales::dollar_format(accuracy = 0.01, ...)

utils::globalVariables(c(
  "account_name",
  "amount",
  "category",
  "description",
  "full_account_name",
  "label",
  "month",
  "observed_overhead",
  "observed_revenue",
  "overhead_lower",
  "overhead_upper",
  "overhead_forecast",
  "Period",
  "period_start",
  "required_revenue",
  "revenue",
  "revenue_forecast",
  "revenue_lower",
  "revenue_upper",
  "source",
  "total",
  "total_overhead",
  "total_revenue",
  "week_start",
  "year"
))
