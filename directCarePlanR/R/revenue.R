#' Validate a Non-Negative Scalar Argument
#'
#' @param x Value to validate.
#' @param name Argument name, used in the error message.
#'
#' @noRd
.validate_nonneg_scalar <- function(x, name) {
  if (!rlang::is_scalar_double(x) && !rlang::is_scalar_integer(x)) {
    rlang::abort(
      paste0("`", name, "` must be a single number."),
      class = "dcPlanR_invalid_argument"
    )
  }
  if (is.na(x) || x < 0) {
    rlang::abort(
      paste0("`", name, "` must be non-negative."),
      class = "dcPlanR_invalid_argument"
    )
  }
  invisible(x)
}

#' Validate a Positive Integer Argument
#'
#' @param x Value to validate.
#' @param name Argument name, used in the error message.
#'
#' @noRd
.validate_positive_int <- function(x, name) {
  if (
    (!rlang::is_scalar_double(x) && !rlang::is_scalar_integer(x)) ||
      is.na(x) ||
      x != round(x) ||
      x < 1
  ) {
    rlang::abort(
      paste0("`", name, "` must be a single positive whole number."),
      class = "dcPlanR_invalid_argument"
    )
  }
  invisible(as.integer(x))
}

#' Validate a Proportion Argument
#'
#' @param x Value to validate.
#' @param name Argument name, used in the error message.
#'
#' @noRd
.validate_proportion <- function(x, name) {
  if (!rlang::is_scalar_double(x) && !rlang::is_scalar_integer(x)) {
    rlang::abort(
      paste0("`", name, "` must be a single number."),
      class = "dcPlanR_invalid_argument"
    )
  }
  if (is.na(x) || x < 0 || x > 1) {
    rlang::abort(
      paste0("`", name, "` must be between 0 and 1."),
      class = "dcPlanR_invalid_argument"
    )
  }
  invisible(x)
}

#' Validate a Finite Numeric Scalar Argument
#'
#' Unlike `.validate_nonneg_scalar()`, negative values are allowed (e.g. a
#' shrinking overhead growth rate).
#'
#' @param x Value to validate.
#' @param name Argument name, used in the error message.
#'
#' @noRd
.validate_numeric_scalar <- function(x, name) {
  if (!rlang::is_scalar_double(x) && !rlang::is_scalar_integer(x)) {
    rlang::abort(
      paste0("`", name, "` must be a single number."),
      class = "dcPlanR_invalid_argument"
    )
  }
  if (is.na(x) || is.infinite(x)) {
    rlang::abort(
      paste0("`", name, "` must be finite."),
      class = "dcPlanR_invalid_argument"
    )
  }
  invisible(x)
}

#' Ramp Fraction by Month
#'
#' Shared by [calc_membership_revenue()] and [calc_fee_revenue()]: the
#' fraction of a target quantity (panel size, visit volume) reached in each
#' month of a ramp-up period, so both revenue streams build up the same way
#' rather than starting full on day one.
#'
#' @param month Integer vector of month numbers (1-indexed).
#' @param ramp_months Integer number of months to reach the full target.
#' @param ramp_shape Character string: `"linear"` or `"s_curve"` (a
#'   smoothstep curve: slow to start, accelerating through the middle,
#'   easing into the target).
#'
#' @return Numeric vector, same length as `month`, in `[0, 1]`.
#'
#' @noRd
.ramp_fraction <- function(month, ramp_months, ramp_shape) {
  frac <- pmin(month / ramp_months, 1)
  if (ramp_shape == "s_curve") {
    frac <- 3 * frac^2 - 2 * frac^3
  }
  frac
}

#' Calculate Membership Revenue
#'
#' Projects membership revenue as panel size times membership fee, applying
#' a ramp function so panel size builds up over `ramp_months` rather than
#' starting full on day one.
#'
#' @param panel_size Integer target panel size (number of members) once
#'   ramp-up is complete.
#' @param fee Numeric monthly membership fee per member.
#' @param horizon_months Integer number of months to project. Defaults to
#'   24.
#' @param ramp_months Integer number of months to reach `panel_size`.
#'   Defaults to 12. If greater than `horizon_months`, the ramp will not be
#'   complete by the end of the projection.
#' @param ramp_shape Character string specifying the ramp shape: `"linear"`
#'   (default) or `"s_curve"` (a smoothstep curve: slow to start,
#'   accelerating through the middle, easing into `panel_size`).
#'
#' @return A `dcPlanR_membership_revenue`-classed tibble with one row per
#'   month and columns `month`, `panel_size`, and `revenue`.
#'
#' @export
calc_membership_revenue <- function(
  panel_size,
  fee,
  horizon_months = 24L,
  ramp_months = 12L,
  ramp_shape = c("linear", "s_curve")
) {
  ramp_shape <- match.arg(ramp_shape)
  .validate_nonneg_scalar(panel_size, "panel_size")
  .validate_nonneg_scalar(fee, "fee")
  horizon_months <- .validate_positive_int(horizon_months, "horizon_months")
  ramp_months <- .validate_positive_int(ramp_months, "ramp_months")

  month <- seq_len(horizon_months)
  ramped_panel_size <- round(panel_size * .ramp_fraction(month, ramp_months, ramp_shape))

  # tibble::new_tibble() instead of tibble::tibble(): every column here is
  # already a fully-computed, equal-length vector, so there's no tidy-eval
  # column expression to evaluate -- tibble()'s quosure-capture/recycling
  # machinery is pure overhead in that case (~40x slower, confirmed via
  # microbenchmark: ~186us vs ~5us for a 24-row/3-column call). This
  # function is on the goal-seek bisection's hot path (called up to 25x per
  # lever), where profvis identified tibble() construction as the single
  # largest contributor to total cost.
  tibble::new_tibble(
    list(
      month = month,
      panel_size = ramped_panel_size,
      revenue = ramped_panel_size * fee
    ),
    nrow = horizon_months,
    class = "dcPlanR_membership_revenue"
  )
}

#' Calculate Fee-for-Visit Revenue
#'
#' Projects fee-based revenue as visit volume times visit fee, using an
#' explicit new-patient vs. follow-up visit mix since the two are typically
#' billed at different rates. Applies the same ramp function as
#' [calc_membership_revenue()] so visit volume builds up to `visit_volume`
#' over `ramp_months` rather than starting full on day one -- a new
#' fee-for-service practice fills its schedule gradually, same as a
#' membership panel.
#'
#' @param visit_volume Integer target number of visits per month once
#'   ramp-up is complete.
#' @param new_visit_fee Numeric fee charged for a new-patient visit.
#' @param follow_up_fee Numeric fee charged for a follow-up visit.
#' @param new_visit_pct Numeric between 0 and 1: the share of visits that
#'   are new-patient visits. Defaults to 0.2.
#' @param horizon_months Integer number of months to project. Defaults to
#'   24.
#' @param ramp_months Integer number of months to reach `visit_volume`.
#'   Defaults to 12. If greater than `horizon_months`, the ramp will not be
#'   complete by the end of the projection.
#' @param ramp_shape Character string specifying the ramp shape: `"linear"`
#'   (default) or `"s_curve"` (a smoothstep curve: slow to start,
#'   accelerating through the middle, easing into `visit_volume`).
#'
#' @return A `dcPlanR_fee_revenue`-classed tibble with one row per month
#'   and columns `month`, `visit_volume`, and `revenue`.
#'
#' @export
calc_fee_revenue <- function(
  visit_volume,
  new_visit_fee,
  follow_up_fee,
  new_visit_pct = 0.2,
  horizon_months = 24L,
  ramp_months = 12L,
  ramp_shape = c("linear", "s_curve")
) {
  ramp_shape <- match.arg(ramp_shape)
  .validate_nonneg_scalar(visit_volume, "visit_volume")
  .validate_nonneg_scalar(new_visit_fee, "new_visit_fee")
  .validate_nonneg_scalar(follow_up_fee, "follow_up_fee")
  .validate_proportion(new_visit_pct, "new_visit_pct")
  horizon_months <- .validate_positive_int(horizon_months, "horizon_months")
  ramp_months <- .validate_positive_int(ramp_months, "ramp_months")

  month <- seq_len(horizon_months)
  ramped_visit_volume <- round(visit_volume * .ramp_fraction(month, ramp_months, ramp_shape))

  revenue <- (ramped_visit_volume * new_visit_pct) *
    new_visit_fee +
    (ramped_visit_volume * (1 - new_visit_pct)) * follow_up_fee

  # See calc_membership_revenue()'s comment on new_tibble() vs tibble().
  tibble::new_tibble(
    list(
      month = month,
      visit_volume = ramped_visit_volume,
      revenue = revenue
    ),
    nrow = horizon_months,
    class = "dcPlanR_fee_revenue"
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
#' @param horizon_months Integer number of months to project. Defaults to
#'   24. Overrides any `horizon_months` supplied inside `membership_args`
#'   or `fee_args`, so both components always align on the same months.
#'
#' @return A `dcPlanR_revenue` list with:
#'   \item{membership}{Tibble from [calc_membership_revenue()], or `NULL`}
#'   \item{fee_for_service}{Tibble from [calc_fee_revenue()], or `NULL`}
#'   \item{total}{Tibble with `month`, `membership_revenue`, `fee_revenue`,
#'     and `total_revenue` (0, not `NA`, for an omitted component)}
#'
#' @export
calc_mixed_revenue <- function(
  membership_args = NULL,
  fee_args = NULL,
  horizon_months = 24L
) {
  if (is.null(membership_args) && is.null(fee_args)) {
    rlang::abort(
      "At least one of `membership_args` or `fee_args` must be supplied.",
      class = "dcPlanR_missing_revenue_component"
    )
  }
  horizon_months <- .validate_positive_int(horizon_months, "horizon_months")

  membership <- if (!is.null(membership_args)) {
    rlang::exec(
      calc_membership_revenue,
      !!!utils::modifyList(membership_args, list(horizon_months = horizon_months))
    )
  }
  fee_for_service <- if (!is.null(fee_args)) {
    rlang::exec(
      calc_fee_revenue,
      !!!utils::modifyList(fee_args, list(horizon_months = horizon_months))
    )
  }

  # rep(0, horizon_months), not a bare 0: unlike tibble::tibble(),
  # tibble::new_tibble() below does NOT recycle short columns to nrow --
  # it's a low-level constructor that trusts the caller already sized
  # everything correctly (that's the whole point of skipping tibble()'s
  # validation/recycling overhead). A bare `0` here would silently produce
  # a length-1 column that only *displays* as if broadcast to every row.
  membership_revenue <- if (!is.null(membership)) membership$revenue else rep(0, horizon_months)
  fee_revenue <- if (!is.null(fee_for_service)) fee_for_service$revenue else rep(0, horizon_months)

  total <- tibble::new_tibble(
    list(
      month = seq_len(horizon_months),
      membership_revenue = membership_revenue,
      fee_revenue = fee_revenue,
      total_revenue = membership_revenue + fee_revenue
    ),
    nrow = horizon_months
  )

  structure(
    list(
      membership = membership,
      fee_for_service = fee_for_service,
      total = total
    ),
    class = "dcPlanR_revenue"
  )
}
