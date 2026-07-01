#' Project a Single Financial Scenario
#'
#' Layer 1 of the projections module. Takes a complete set of practice
#' launch assumptions and returns a month-by-month data frame of revenue,
#' overhead, and net income for the given horizon.
#'
#' @param assumptions A `dcPlanR_assumptions` list describing panel growth,
#'   fees, overhead, and startup costs.
#' @param horizon_months Integer number of months to project. Defaults to
#'   24.
#'
#' @return A tibble with one row per month and columns for revenue,
#'   overhead, and net income components.
#'
#' @export
project_practice <- function(assumptions, horizon_months = 24L) {
  rlang::abort(
    "project_practice() is not yet implemented.",
    class = "dcPlanR_not_implemented"
  )
}

#' Project Conservative, Base, and Optimistic Scenarios
#'
#' Layer 2 of the projections module. Calls [project_practice()] three
#' times using either explicit variation parameters or sensible defaults
#' (e.g. conservative = slower panel growth and higher overhead), returning
#' scenario as a first-class dimension of the result.
#'
#' @param assumptions A `dcPlanR_assumptions` list describing panel growth,
#'   fees, overhead, and startup costs, used as the base scenario.
#' @param horizon_months Integer number of months to project. Defaults to
#'   24.
#' @param scenario_params Optional named list with `conservative` and
#'   `optimistic` elements, each a list of assumption overrides relative to
#'   `assumptions`. Defaults to `NULL`, which applies built-in defaults.
#'
#' @return A tibble with one row per month per scenario, with a `scenario`
#'   column taking values `"conservative"`, `"base"`, and `"optimistic"`.
#'
#' @export
project_scenarios <- function(
  assumptions,
  horizon_months = 24L,
  scenario_params = NULL
) {
  rlang::abort(
    "project_scenarios() is not yet implemented.",
    class = "dcPlanR_not_implemented"
  )
}
