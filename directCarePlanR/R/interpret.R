#' Interpret Revenue Projections
#'
#' Generates narrative text describing a revenue projection, framed
#' decisionally: what the fee structure requires to hit target revenue,
#' rather than a diagnostic description of the numbers alone.
#'
#' @param revenue A `dcPlanR_revenue` object, as returned by
#'   [calc_mixed_revenue()].
#'
#' @return A character string of narrative text.
#'
#' @export
interpret_revenue <- function(revenue) {
  rlang::abort(
    "interpret_revenue() is not yet implemented.",
    class = "dcPlanR_not_implemented"
  )
}

#' Interpret a Financial Projection
#'
#' Generates narrative text describing a scenario projection, calling out
#' the most sensitive assumption and what would need to be true for the
#' practice to break even within the projection horizon.
#'
#' @param projection A tibble as returned by [project_scenarios()].
#'
#' @return A character string of narrative text.
#'
#' @export
interpret_projection <- function(projection) {
  rlang::abort(
    "interpret_projection() is not yet implemented.",
    class = "dcPlanR_not_implemented"
  )
}

#' Interpret Capital Requirements
#'
#' Generates narrative text describing startup capital and personal runway
#' requirements, framed around what financing decision they imply.
#'
#' @param startup_costs A list as returned by [calc_startup_costs()].
#' @param personal_runway A list as returned by [calc_personal_runway()].
#'
#' @return A character string of narrative text.
#'
#' @export
interpret_capital <- function(startup_costs, personal_runway) {
  rlang::abort(
    "interpret_capital() is not yet implemented.",
    class = "dcPlanR_not_implemented"
  )
}
