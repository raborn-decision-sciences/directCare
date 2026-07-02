#' Sum Startup Costs
#'
#' Totals a list of one-time startup cost line items (e.g. EHR setup,
#' equipment, licensing, marketing) into a single startup capital
#' requirement.
#'
#' @param cost_items A named numeric vector or list of startup cost line
#'   items. Every element must be named and non-negative.
#'
#' @return A `dcPlanR_startup_costs` list with `total` and `line_items`
#'   (the input, preserved for display).
#'
#' @export
calc_startup_costs <- function(cost_items) {
  if (!is.numeric(unlist(cost_items)) || length(cost_items) == 0L) {
    rlang::abort(
      "`cost_items` must be a non-empty named numeric vector or list.",
      class = "dcPlanR_invalid_argument"
    )
  }
  item_names <- names(cost_items)
  if (is.null(item_names) || any(!nzchar(item_names)) || anyDuplicated(item_names) > 0) {
    rlang::abort(
      "`cost_items` must have unique, non-empty names for every element.",
      class = "dcPlanR_invalid_argument"
    )
  }
  values <- unlist(cost_items)
  if (anyNA(values) || any(values < 0)) {
    rlang::abort(
      "`cost_items` values must be non-negative and non-missing.",
      class = "dcPlanR_invalid_argument"
    )
  }

  structure(
    list(total = sum(values), line_items = cost_items),
    class = "dcPlanR_startup_costs"
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
#' @return A `dcPlanR_personal_runway` list with `total`,
#'   `monthly_expenses`, and `months_coverage`.
#'
#' @export
calc_personal_runway <- function(monthly_expenses, months_coverage) {
  .validate_nonneg_scalar(monthly_expenses, "monthly_expenses")
  months_coverage <- .validate_positive_int(months_coverage, "months_coverage")

  structure(
    list(
      total = monthly_expenses * months_coverage,
      monthly_expenses = monthly_expenses,
      months_coverage = months_coverage
    ),
    class = "dcPlanR_personal_runway"
  )
}

#' Calculate Loan Amortization
#'
#' Produces a month-by-month amortization schedule for a fixed-rate loan,
#' for practices that are debt-financed.
#'
#' @param principal Numeric loan principal.
#' @param annual_rate Numeric annual interest rate, expressed as a decimal
#'   (e.g. 0.08 for 8%). `0` is allowed (interest-free financing).
#' @param term_months Integer loan term in months.
#'
#' @return A `dcPlanR_loan_amortization`-classed tibble with one row per
#'   month and columns `month`, `payment`, `principal_paid`,
#'   `interest_paid`, and `remaining_balance` (`0` in the final month,
#'   after rounding).
#'
#' @export
calc_loan_amortization <- function(principal, annual_rate, term_months) {
  .validate_nonneg_scalar(principal, "principal")
  .validate_nonneg_scalar(annual_rate, "annual_rate")
  term_months <- .validate_positive_int(term_months, "term_months")

  monthly_rate <- annual_rate / 12
  payment <- if (monthly_rate == 0) {
    principal / term_months
  } else {
    principal * monthly_rate / (1 - (1 + monthly_rate)^(-term_months))
  }

  balance <- principal
  schedule <- vector("list", term_months)
  for (m in seq_len(term_months)) {
    interest_paid <- balance * monthly_rate
    principal_paid <- payment - interest_paid
    balance <- balance - principal_paid
    # Guard against floating-point residue on the final payment.
    if (m == term_months) {
      principal_paid <- principal_paid + balance
      balance <- 0
    }
    schedule[[m]] <- list(
      month = m,
      payment = payment,
      principal_paid = principal_paid,
      interest_paid = interest_paid,
      remaining_balance = balance
    )
  }

  structure(
    dplyr::bind_rows(schedule),
    class = c("dcPlanR_loan_amortization", "tbl_df", "tbl", "data.frame")
  )
}
