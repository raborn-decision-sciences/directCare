
#' Validate Overhead Tibble
#'
#' Checks a normalized overhead tibble for common data quality issues.
#' Negative amounts are flagged as refunds rather than treated as errors.
#' Throws an error for unrecoverable problems, warns for recoverable ones.
#'
#' @param data A tibble as returned by \code{ingest_gnucash_csv()} or another
#'   ingest function.
#'
#' @return The input tibble, invisibly, with a \code{is_refund} column added.
#'   Rows with negative amounts are marked \code{TRUE}.
#'
#' @export
validate_overhead <- function(data) {

  required_cols <- c(
    "practice_id", "date", "week_start", "month", "year",
    "amount", "category", "source"
  )
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    rlang::abort(
      paste0(
        "Overhead tibble is missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      class    = "dcForecastR_missing_columns",
      missing_columns = missing_cols
    )
  }

  # --- Unrecoverable errors --------------------------------------------------

  if (anyNA(data$date)) {
    rlang::abort(
      paste0(
        nrow(data[is.na(data$date), ]),
        " rows have unparseable dates. Check the source file."
      ),
      class = "dcForecastR_invalid_dates"
    )
  }

  if (anyNA(data$amount)) {
    rlang::abort(
      paste0(
        sum(is.na(data$amount)),
        " rows have missing amounts. Check the source file."
      ),
      class = "dcForecastR_missing_amounts"
    )
  }

  # --- Recoverable warnings --------------------------------------------------

  refund_rows <- data$amount < 0
  if (any(refund_rows)) {
    rlang::warn(
      paste0(
        sum(refund_rows),
        " negative amount(s) detected and flagged as refunds. ",
        "These will reduce overhead totals in the affected month(s)."
      ),
      class   = "dcForecastR_refunds_detected",
      refunds = data[refund_rows, ]   # attach the rows for UI inspection
    )
  }

  zero_rows <- data$amount == 0
  if (any(zero_rows)) {
    rlang::warn(
      paste0(
        sum(zero_rows),
        " zero-amount row(s) detected. These will be ignored in summaries."
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

  # --- Tag refunds and return ------------------------------------------------

  data |>
    dplyr::mutate(is_refund = amount < 0) |>
    invisible()
}
