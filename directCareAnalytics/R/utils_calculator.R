#' Quick Calculator computation helpers
#'
#' `mod_calculator_server()` computes its own results directly from live
#' `input$...` reactives (see its `results_ui` renderer). This file exists so
#' the same arithmetic can also run against a *saved* scenario's raw inputs
#' -- e.g. `directCareScenarios::scenario_load_json()`'s decoded JSON -- for
#' comparing scenarios that aren't the one currently on screen. Keeping this
#' as a pure function (not another reactive) is what makes that reuse
#' possible.
#'
#' @noRd
NULL

#' Recompute Quick Calculator results from a raw inputs list.
#'
#' @param inputs A list shaped like `mod_calculator_server()`'s
#'   `calc_inputs_for_save()`: `ovhd_multi`, `monthly_overhead`,
#'   `overhead_items` (list of `label`/`amount`), `tiers` (list of
#'   `label`/`members`/`fee`), `income_items` (list of `label`/`amount`),
#'   `target_income`. Also accepts the shape returned by
#'   `jsonlite::fromJSON(..., simplifyVector = FALSE)`, which is what a
#'   saved `dca_calculator_scenarios` row round-trips to.
#'
#' @return A list: `total_overhead`, `other_income`, `tier_total_members`,
#'   `tier_total_revenue`, `avg_fee_per_member`, `total_revenue`, `net`,
#'   `target_income`, `members_for_breakeven`, `members_for_target`,
#'   `fee_for_breakeven`, `fee_for_target`.
#' @noRd
compute_calculator_results <- function(inputs) {
  .nn <- function(x) {
    v <- x %||% 0
    if (length(v) != 1L || !is.finite(v)) 0 else v
  }

  overhead_items <- inputs$overhead_items %||% list()
  tiers <- inputs$tiers %||% list()
  income_items <- inputs$income_items %||% list()

  total_overhead <- if (isTRUE(inputs$ovhd_multi)) {
    sum(vapply(overhead_items, function(x) .nn(x$amount), numeric(1)))
  } else {
    .nn(inputs$monthly_overhead)
  }

  tier_total_members <- sum(vapply(tiers, function(x) .nn(x$members), numeric(1)))
  tier_total_revenue <- sum(vapply(
    tiers,
    function(x) .nn(x$members) * .nn(x$fee),
    numeric(1)
  ))
  avg_fee_per_member <- if (tier_total_members > 0) {
    tier_total_revenue / tier_total_members
  } else {
    0
  }

  other_income <- sum(vapply(income_items, function(x) .nn(x$amount), numeric(1)))

  total_revenue <- tier_total_revenue + other_income
  net <- total_revenue - total_overhead
  target_income <- .nn(inputs$target_income)

  # Other income already covers part of overhead/target, so only the
  # remainder needs to come from membership dues -- mirrors
  # mod_calculator.R's own results_ui logic.
  net_needed_breakeven <- max(0, total_overhead - other_income)
  net_needed_target <- max(0, total_overhead + target_income - other_income)

  members_for_breakeven <- if (avg_fee_per_member > 0) {
    ceiling(net_needed_breakeven / avg_fee_per_member)
  } else {
    NA_integer_
  }
  members_for_target <- if (avg_fee_per_member > 0) {
    ceiling(net_needed_target / avg_fee_per_member)
  } else {
    NA_integer_
  }
  fee_for_breakeven <- if (tier_total_members > 0) {
    net_needed_breakeven / tier_total_members
  } else {
    NA_real_
  }
  fee_for_target <- if (tier_total_members > 0) {
    net_needed_target / tier_total_members
  } else {
    NA_real_
  }

  list(
    total_overhead = total_overhead,
    other_income = other_income,
    tier_total_members = tier_total_members,
    tier_total_revenue = tier_total_revenue,
    avg_fee_per_member = avg_fee_per_member,
    total_revenue = total_revenue,
    net = net,
    target_income = target_income,
    members_for_breakeven = members_for_breakeven,
    members_for_target = members_for_target,
    fee_for_breakeven = fee_for_breakeven,
    fee_for_target = fee_for_target
  )
}
