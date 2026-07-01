#' Sum Startup Costs
#'
#' Totals a list of one-time startup cost line items (e.g. EHR setup,
#' equipment, licensing, marketing) into a single startup capital
#' requirement.
#'
#' @param cost_items A named numeric vector or list of startup cost line
#'   items.
#'
#' @return A list with `total` and `line_items` (the input, preserved for
#'   display).
#'
#' @export
calc_startup_costs <- function(cost_items) {
  rlang::abort(
    "calc_startup_costs() is not yet implemented.",
    class = "dcPlanR_not_implemented"
  )
}

#' Calculate Personal Runway
#'
#' Multiplies monthly personal living expenses by the number of months of
#' coverage needed, to size the personal (non-practice) cash reserve a
#' physician needs before launch.
#'
#' @param monthly_expenses Numeric monthly personal living expenses.
#' @param months_coverage Integer number of months of coverage needed.
#'
#' @return A list with `total` and the input parameters.
#'
#' @export
calc_personal_runway <- function(monthly_expenses, months_coverage) {
  rlang::abort(
    "calc_personal_runway() is not yet implemented.",
    class = "dcPlanR_not_implemented"
  )
}

#' Calculate Loan Amortization
#'
#' Produces a month-by-month amortization schedule for a fixed-rate loan,
#' for practices that are debt-financed.
#'
#' @param principal Numeric loan principal.
#' @param annual_rate Numeric annual interest rate, expressed as a decimal
#'   (e.g. 0.08 for 8%).
#' @param term_months Integer loan term in months.
#'
#' @return A tibble with one row per month and columns for payment,
#'   principal paid, interest paid, and remaining balance.
#'
#' @export
calc_loan_amortization <- function(principal, annual_rate, term_months) {
  rlang::abort(
    "calc_loan_amortization() is not yet implemented.",
    class = "dcPlanR_not_implemented"
  )
}
