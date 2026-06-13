#' Validate an Income Tibble
#'
#' Checks a normalized income tibble for common data quality issues. Negative
#' revenue rows (cancellations, chargebacks) are flagged rather than treated as
#' errors. Throws an error for unrecoverable problems and a warning for
#' recoverable ones.
#'
#' This function is called automatically by \code{ingest_manual(type =
#' "income")}. For GnuCash data the equivalent checks are applied during
#' \code{ingest_gnucash_csv()}, so you only need to call this directly when
#' building income tibbles by hand or in custom ingest pipelines.
#'
#' @param data A tibble as returned by \code{normalize_gnucash_income()} or
#'   \code{ingest_manual(type = "income")}. Required columns:
#'   \code{practice_id}, \code{date}, \code{week_start}, \code{month},
#'   \code{year}, \code{revenue}, \code{source}.
#'
#' @return The input tibble, invisibly, with an \code{is_refund} column added.
#'   Rows with negative revenue are marked \code{TRUE}.
#'
#' @export
validate_income <- function(data) {
  required_cols <- c(
    "practice_id",
    "date",
    "week_start",
    "month",
    "year",
    "revenue",
    "source"
  )
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    rlang::abort(
      paste0(
        "Income tibble is missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      class = "dcForecastR_missing_columns",
      missing_columns = missing_cols
    )
  }

  # --- Unrecoverable errors ---------------------------------------------------

  if (anyNA(data$date)) {
    rlang::abort(
      paste0(
        sum(is.na(data$date)),
        " row(s) have unparseable dates. Check the source file."
      ),
      class = "dcForecastR_invalid_dates"
    )
  }

  if (anyNA(data$revenue)) {
    rlang::abort(
      paste0(
        sum(is.na(data$revenue)),
        " row(s) have missing revenue values. Check the source file."
      ),
      class = "dcForecastR_missing_amounts"
    )
  }

  # --- Recoverable warnings ---------------------------------------------------

  refund_rows <- data$revenue < 0
  if (any(refund_rows)) {
    rlang::warn(
      paste0(
        sum(refund_rows),
        " negative revenue row(s) detected and flagged as cancellations/chargebacks. ",
        "These will reduce revenue totals in the affected period(s)."
      ),
      class = "dcForecastR_refunds_detected",
      refunds = data[refund_rows, ]
    )
  }

  zero_rows <- data$revenue == 0
  if (any(zero_rows)) {
    rlang::warn(
      paste0(
        sum(zero_rows),
        " zero-revenue row(s) detected. These will be ignored in summaries."
      ),
      class = "dcForecastR_zero_amounts"
    )
  }

  future_rows <- data$date > Sys.Date()
  if (any(future_rows)) {
    rlang::warn(
      paste0(
        sum(future_rows),
        " row(s) have future dates. Verify these are not data entry errors."
      ),
      class = "dcForecastR_future_dates",
      future_rows = data[future_rows, ]
    )
  }

  # --- Tag cancellations and return -------------------------------------------

  data |>
    dplyr::mutate(is_refund = revenue < 0) |>
    invisible()
}
