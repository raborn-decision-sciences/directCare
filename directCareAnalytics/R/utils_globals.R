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
#'
#' The RDS favicon and brand palette are deliberately identical across the
#' whole product family (see both apps' `rds_theme()`), so the title text
#' was the only thing telling DCA's and Planner's login/signup/password-
#' reset pages apart -- easy to miss at a glance since both start with
#' "Direct Care". The icon+tagline pair below is this app's own visual
#' identifier, distinct from Planner's (see its own `.auth_page_chrome()`).
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
    ),
    tags$p(
      class = "text-muted small mb-0",
      style = "max-width:320px;margin:2px auto 0;",
      bs_icon("graph-up-arrow"), " Know your break-even, revenue trend, and path to your income goal."
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

#' Time an expression and log its elapsed wall time
#'
#' Phase 0 of the performance-improvement plan (TODO.md, "UI/UX &
#' Performance Backlog") -- this app is a single `shiny::runApp()` process
#' per container, so any slow synchronous call blocks every concurrently
#' connected session, not just the one that triggered it. Wraps the
#' handful of known-heaviest synchronous call sites (goal-seek bisection,
#' PDF/typst generation) so degradation shows up in production logs
#' (`docker logs`/`docker compose logs`, already rotated per-service via
#' `docker-compose.yml`'s `logging:` block) instead of only being
#' discoverable by reading the code. `message()`, not `cat()`/`print()`,
#' so this goes to stderr like Shiny's own diagnostic output, and is
#' filterable independent of stdout. `on.exit()` (not a simple
#' before/after pair) so the elapsed time is still logged if `expr`
#' errors, since a slow *failing* call is exactly as relevant to this as
#' a slow successful one -- the error itself still propagates normally,
#' this only adds a log line around it.
#'
#' @param label Short identifier for the call site, e.g. `"goal_seek_breakeven"`.
#' @param expr The expression to time and return the value of.
#'
#' @noRd
.log_elapsed <- function(label, expr) {
  t0 <- proc.time()[["elapsed"]]
  on.exit(message(sprintf(
    "[perf] %s elapsed_ms=%s",
    label,
    round((proc.time()[["elapsed"]] - t0) * 1000)
  )), add = TRUE)
  expr
}

#' Directory for async report_task's generated PDF files
#'
#' `mod_projections.R`'s `report_task` (`ExtendedTask`) writes its PDF
#' output here instead of a bare `tempfile()` in the tempdir root, so
#' `run_app.R`'s periodic sweep (registered next to `mirai::daemons()`) can
#' find and clean up files whose session ended mid-generation -- the one
#' case per-session cleanup can't cover, since a mirai task isn't tied to
#' session lifecycle. `tempdir()` is stable per R process, so every caller
#' in this process (main process and, since mirai workers share
#' `.libPaths()`/filesystem, the workers too) resolves the same path.
#'
#' Creates the directory on every call (cheap/idempotent with
#' `showWarnings = FALSE`) rather than relying solely on `run_app()`'s
#' one-time setup -- `testServer()`-based tests invoke module servers
#' directly without ever calling `run_app()`, so they'd otherwise hit a
#' missing-directory write failure the first time a test generates a report.
#' @noRd
.report_output_dir <- function() {
  dir <- file.path(tempdir(), "dca_reports")
  dir.create(dir, showWarnings = FALSE)
  dir
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
