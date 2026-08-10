# Suppress R CMD check NOTEs for bare imported symbols and data-masking
# column names used in ggplot2 contexts throughout the app.

#' @importFrom bslib bs_theme card card_body card_header layout_columns accordion accordion_panel input_task_button value_box page_fluid page_navbar nav_panel nav_spacer nav_item input_dark_mode
#' @importFrom bsicons bs_icon
#' @importFrom stats setNames
NULL

# NULL-coalescing operator for input$ values that are NULL before first
# render. Matches directCareAnalytics's R/utils_globals.R convention.
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Time an expression and log its elapsed wall time
#'
#' Matches directCareAnalytics's R/utils_globals.R identical helper -- see
#' that file's own roxygen comment for the full rationale (Phase 0 of the
#' performance-improvement plan in TODO.md). Any call wrapped here that
#' isn't routed through `mod_results.R`'s `report_task` `ExtendedTask` is
#' still a full-app-blocking call, same as before that was added.
#'
#' @param label Short identifier for the call site, e.g.
#'   `"goal_seek_projection_recovery"`.
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
#' Matches directCareAnalytics's R/utils_globals.R identical helper -- see
#' that file's own roxygen comment for the full rationale. `mod_results.R`'s
#' `report_task` (`ExtendedTask`) writes its PDF output here instead of a
#' bare `tempfile()` in the tempdir root, so `run_app.R`'s periodic sweep
#' can find and clean up files whose session ended mid-generation. Creates
#' the directory on every call (cheap/idempotent) rather than relying
#' solely on `run_app()`'s one-time setup, since `testServer()`-based tests
#' invoke module servers directly without ever calling `run_app()`.
#' @noRd
.report_output_dir <- function() {
  dir <- file.path(tempdir(), "planner_reports")
  dir.create(dir, showWarnings = FALSE)
  dir
}

#' JS handler that redirects the browser to a Stripe-hosted URL
#'
#' Matches directCareAnalytics's R/utils_globals.R convention -- see that
#' file's own roxygen comment for the full rationale.
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
#' Matches directCareAnalytics's R/utils_globals.R convention -- see that
#' file's own roxygen comment for the full rationale, including why the
#' icon+tagline pair below exists (this app's own visual identifier,
#' distinct from DCA's, since the RDS favicon/palette are identical across
#' the product family).
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
      style = "margin-top:8px;font-weight:600;color:var(--bs-emphasis-color);",
      title
    ),
    tags$p(
      class = "text-muted small mb-0",
      style = "max-width:320px;margin:2px auto 0;",
      bs_icon("compass"), " Model your launch costs, revenue, and timeline before you open."
    )
  )
}

utils::globalVariables(c("month", "net_income", "scenario"))

# Coerce a potentially-NULL or NA numeric input to 0, so a cleared
# itemized field contributes $0 to a running total instead of propagating
# NA into it (a cleared numericInput sends NA, not NULL, so %||% alone
# doesn't catch it). Shared by mod_plan_inputs.R's live form totals and
# .assumptions_from_saved_inputs() below, which both need identical
# NULL/NA handling against either live `input$x` values or a saved
# scenario's plain-list `inputs`.
.nn0 <- function(x) {
  v <- x %||% 0
  if (length(v) != 1L || !is.finite(v)) 0 else v
}

# `vals` is a plain named list -- either the live form's values (built
# fresh by mod_plan_inputs.R's overhead_total_r() reactive) or a saved
# scenario's restored `inputs` (see plan_inputs_for_save()) -- so this
# stays a pure function with no reactive dependency, usable from either
# context.
.overhead_total_from <- function(vals) {
  if (identical(vals$overhead_mode, "single")) {
    .nn0(vals$overhead_monthly)
  } else {
    sum(
      c(
        .nn0(vals$overhead_rent),
        .nn0(vals$overhead_staff),
        .nn0(vals$overhead_ehr),
        .nn0(vals$overhead_malpractice),
        .nn0(vals$overhead_supplies),
        .nn0(vals$overhead_other)
      )
    )
  }
}

#' Rebuild a `project_practice()` assumptions list from a saved scenario's inputs
#'
#' `plan_scenarios` stores inputs only, not a results snapshot (see
#' `SAVED_SCENARIOS.md`), so comparing saved scenarios' recovery timing
#' means re-running the projection pipeline for each one. This rebuilds
#' the same `assumptions` list `mod_plan_inputs.R`'s `.build_plan()`
#' constructs from live form inputs, but from a saved scenario's restored
#' `inputs` list instead -- used only by `mod_results.R`'s scenario
#' comparison, which needs `assumptions` directly rather than the full
#' plan (market context, capital, interpretations) `.build_plan()` also
#' computes.
#'
#' @param inputs A plain list in the shape `plan_inputs_for_save()`
#'   produces (see `mod_plan_inputs.R`), as returned by
#'   `directCareScenarios::scenario_load_json()`.
#'
#' @noRd
.assumptions_from_saved_inputs <- function(inputs) {
  membership_args <- if (isTRUE(inputs$include_membership)) {
    list(
      panel_size = inputs$panel_size,
      fee = inputs$fee,
      ramp_months = inputs$ramp_months,
      ramp_shape = inputs$ramp_shape
    )
  }
  fee_args <- if (isTRUE(inputs$include_fee)) {
    list(
      visit_volume = inputs$visit_volume,
      new_visit_fee = inputs$new_visit_fee,
      follow_up_fee = inputs$follow_up_fee,
      new_visit_pct = inputs$new_visit_pct / 100,
      # Matches .build_plan()'s own %||% 1L fallback -- a scenario saved
      # before fee-for-service ramp existed won't have this key at all.
      ramp_months = inputs$fee_ramp_months %||% 1L,
      ramp_shape = inputs$fee_ramp_shape %||% "linear"
    )
  }
  list(
    membership_args = membership_args,
    fee_args = fee_args,
    overhead_monthly = .overhead_total_from(inputs),
    overhead_growth_rate = inputs$overhead_growth_rate / 100
  )
}
