#' Calculate Membership Revenue
#'
#' Projects membership revenue as panel size times membership fee, applying
#' a ramp function so panel size builds up over time rather than starting
#' full on day one.
#'
#' @param panel_size Integer target panel size (number of members) once
#'   ramp-up is complete.
#' @param fee Numeric monthly membership fee per member.
#' @param ramp_months Integer number of months to reach `panel_size`.
#'   Defaults to 12.
#' @param ramp_shape Character string specifying the ramp shape: `"linear"`
#'   (default) or `"s_curve"`.
#'
#' @return A `dcPlanR_revenue` list with `membership` and `total`
#'   components; see [calc_mixed_revenue()] for the full structure.
#'
#' @export
calc_membership_revenue <- function(
  panel_size,
  fee,
  ramp_months = 12L,
  ramp_shape = c("linear", "s_curve")
) {
  ramp_shape <- match.arg(ramp_shape)

  rlang::abort(
    "calc_membership_revenue() is not yet implemented.",
    class = "dcPlanR_not_implemented"
  )
}

#' Calculate Fee-for-Visit Revenue
#'
#' Projects fee-based revenue as visit volume times visit fee, using an
#' explicit new-patient vs. follow-up visit mix since the two are typically
#' billed at different rates.
#'
#' @param visit_volume Integer number of visits per month.
#' @param new_visit_fee Numeric fee charged for a new-patient visit.
#' @param follow_up_fee Numeric fee charged for a follow-up visit.
#' @param new_visit_pct Numeric between 0 and 1: the share of visits that
#'   are new-patient visits. Defaults to 0.2.
#'
#' @return A `dcPlanR_revenue` list with `fee_for_service` and `total`
#'   components; see [calc_mixed_revenue()] for the full structure.
#'
#' @export
calc_fee_revenue <- function(
  visit_volume,
  new_visit_fee,
  follow_up_fee,
  new_visit_pct = 0.2
) {
  rlang::abort(
    "calc_fee_revenue() is not yet implemented.",
    class = "dcPlanR_not_implemented"
  )
}

#' Calculate Combined Membership and Fee-for-Service Revenue
#'
#' Combines membership and fee-for-service revenue into a single structured
#' object, with an explicit boundary marking which services are covered by
#' membership fees versus billed separately.
#'
#' @param membership_args A named list of arguments passed to
#'   [calc_membership_revenue()], or `NULL` to omit membership revenue.
#' @param fee_args A named list of arguments passed to [calc_fee_revenue()],
#'   or `NULL` to omit fee-for-service revenue.
#'
#' @return A `dcPlanR_revenue` list with:
#'   \item{membership}{Membership revenue detail, or `NULL`}
#'   \item{fee_for_service}{Fee-for-service revenue detail, or `NULL`}
#'   \item{total}{Combined revenue}
#'
#' @export
calc_mixed_revenue <- function(membership_args = NULL, fee_args = NULL) {
  rlang::abort(
    "calc_mixed_revenue() is not yet implemented.",
    class = "dcPlanR_not_implemented"
  )
}
