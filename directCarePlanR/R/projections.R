#' Project a Single Financial Scenario
#'
#' Layer 1 of the projections module. Takes a complete set of practice
#' launch assumptions and returns a month-by-month data frame of revenue,
#' overhead, and net income for the given horizon.
#'
#' @param assumptions A plain list describing the practice's launch
#'   assumptions:
#'   \describe{
#'     \item{membership_args}{List or `NULL`, passed to
#'       [calc_membership_revenue()] (excluding `horizon_months`, which
#'       this function controls).}
#'     \item{fee_args}{List or `NULL`, passed to [calc_fee_revenue()]
#'       (excluding `horizon_months`).}
#'     \item{overhead_monthly}{Required. Non-negative numeric starting
#'       monthly overhead.}
#'     \item{overhead_growth_rate}{Optional numeric monthly compounding
#'       growth rate applied to overhead (e.g. `0.005` for 0.5%/month).
#'       Defaults to `0` (flat overhead) if omitted.}
#'   }
#'   At least one of `membership_args`/`fee_args` must be supplied (see
#'   [calc_mixed_revenue()]).
#' @param horizon_months Integer number of months to project. Defaults to
#'   24.
#'
#' @return A `dcPlanR_projection`-classed tibble with one row per month and
#'   columns `month`, `membership_revenue`, `fee_revenue`, `total_revenue`,
#'   `overhead`, `net_income`, and `cumulative_net_income`.
#'
#' @export
project_practice <- function(assumptions, horizon_months = 24L) {
  horizon_months <- .validate_positive_int(horizon_months, "horizon_months")
  .validate_nonneg_scalar(assumptions$overhead_monthly, "assumptions$overhead_monthly")
  overhead_growth_rate <- rlang::`%||%`(assumptions$overhead_growth_rate, 0)
  .validate_numeric_scalar(overhead_growth_rate, "assumptions$overhead_growth_rate")

  revenue <- calc_mixed_revenue(
    membership_args = assumptions$membership_args,
    fee_args = assumptions$fee_args,
    horizon_months = horizon_months
  )

  overhead <- assumptions$overhead_monthly * (1 + overhead_growth_rate)^(seq_len(horizon_months) - 1)
  net_income <- revenue$total$total_revenue - overhead

  # tibble::new_tibble() instead of tibble::tibble() -- see
  # calc_membership_revenue() (revenue.R) for the full rationale. This
  # function is the goal-seek bisection's own direct hot-path call (up to
  # 25x per lever x 3 levers), on top of the tibble() calls already avoided
  # inside calc_mixed_revenue()/calc_membership_revenue()/calc_fee_revenue().
  tibble::new_tibble(
    list(
      month = seq_len(horizon_months),
      membership_revenue = revenue$total$membership_revenue,
      fee_revenue = revenue$total$fee_revenue,
      total_revenue = revenue$total$total_revenue,
      overhead = overhead,
      net_income = net_income,
      cumulative_net_income = cumsum(net_income)
    ),
    nrow = horizon_months,
    class = "dcPlanR_projection"
  )
}

#' Apply Conservative/Optimistic Variation to a Set of Assumptions
#'
#' @param assumptions Base assumptions list, as passed to
#'   [project_practice()].
#' @param params List with `ramp_months_multiplier` and
#'   `overhead_multiplier` elements.
#'
#' @noRd
.apply_scenario_variation <- function(assumptions, params) {
  if (!is.null(assumptions$membership_args$ramp_months)) {
    assumptions$membership_args$ramp_months <- max(
      1L,
      round(assumptions$membership_args$ramp_months * params$ramp_months_multiplier)
    )
  }
  assumptions$overhead_monthly <- assumptions$overhead_monthly * params$overhead_multiplier
  assumptions
}

#' Project Conservative, Base, and Optimistic Scenarios
#'
#' Layer 2 of the projections module. Calls [project_practice()] three
#' times using either explicit variation parameters or sensible defaults
#' (conservative = slower panel growth and higher overhead; optimistic =
#' faster panel growth and lower overhead), returning scenario as a
#' first-class dimension of the result.
#'
#' @param assumptions A list as described in [project_practice()], used as
#'   the base scenario.
#' @param horizon_months Integer number of months to project. Defaults to
#'   24.
#' @param scenario_params Optional named list with `conservative` and/or
#'   `optimistic` elements, each a list with `ramp_months_multiplier` and
#'   `overhead_multiplier`. Any field omitted falls back to the built-in
#'   default for that scenario. Defaults to `NULL`, which applies the
#'   built-in defaults entirely:
#'   `list(conservative = list(ramp_months_multiplier = 1.5, overhead_multiplier = 1.1), optimistic = list(ramp_months_multiplier = 0.75, overhead_multiplier = 0.9))`.
#'   The `ramp_months_multiplier` only affects scenarios with a
#'   `membership_args$ramp_months` to scale.
#'
#' @return A `dcPlanR_scenario_projection`-classed tibble with one row per
#'   month per scenario, with a `scenario` column taking values
#'   `"conservative"`, `"base"`, and `"optimistic"`.
#'
#' @export
project_scenarios <- function(
  assumptions,
  horizon_months = 24L,
  scenario_params = NULL
) {
  if (!is.null(scenario_params)) {
    if (!is.list(scenario_params) || !all(names(scenario_params) %in% c("conservative", "optimistic"))) {
      rlang::abort(
        "`scenario_params` must be a list with only `conservative` and/or `optimistic` elements.",
        class = "dcPlanR_invalid_argument"
      )
    }
  }

  default_params <- list(
    conservative = list(ramp_months_multiplier = 1.5, overhead_multiplier = 1.1),
    optimistic = list(ramp_months_multiplier = 0.75, overhead_multiplier = 0.9)
  )
  params <- utils::modifyList(default_params, rlang::`%||%`(scenario_params, list()))

  conservative_assumptions <- .apply_scenario_variation(assumptions, params$conservative)
  optimistic_assumptions <- .apply_scenario_variation(assumptions, params$optimistic)

  dplyr::bind_rows(
    conservative = project_practice(conservative_assumptions, horizon_months),
    base = project_practice(assumptions, horizon_months),
    optimistic = project_practice(optimistic_assumptions, horizon_months),
    .id = "scenario"
  ) |>
    structure(class = c("dcPlanR_scenario_projection", "tbl_df", "tbl", "data.frame"))
}
