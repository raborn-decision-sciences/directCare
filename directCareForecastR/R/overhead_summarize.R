#' Summarize Overhead Expenses by Month
#'
#' Aggregates a normalized overhead tibble to one row per practice per month,
#' computing total overhead, gross overhead (before refunds), and the total
#' value of any refunds/credits. The output is the form expected by
#' \code{forecast_breakeven()} and \code{forecast_target()} on the expense side.
#'
#' @param overhead_tbl A data frame of overhead transactions as returned by
#'   \code{filter_gnucash_overhead()} or \code{ingest_manual(type = "overhead")}.
#'   Required columns: \code{practice_id}, \code{year}, \code{month},
#'   \code{amount}, \code{is_refund}.
#' @param include_refunds Logical. If \code{FALSE}, refund rows are dropped
#'   before aggregation so that \code{total_overhead} reflects only positive
#'   expense transactions (default: \code{TRUE}).
#'
#' @return A tibble with one row per \code{practice_id}/\code{year}/\code{month}
#'   combination and columns:
#'   \describe{
#'     \item{practice_id}{Practice identifier}
#'     \item{year}{Calendar year}
#'     \item{month}{Calendar month (integer 1--12)}
#'     \item{total_overhead}{Net overhead after subtracting refunds}
#'     \item{gross_overhead}{Overhead from positive expense transactions only}
#'     \item{total_refunds}{Magnitude of refunds/credits (always non-negative)}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' overhead <- ingest_gnucash_csv("expenses.csv", practice_id = 1) |>
#'   filter_gnucash_overhead()
#'
#' summarize_overhead_monthly(overhead)
#' }
summarize_overhead_monthly <- function(overhead_tbl, include_refunds = TRUE) {
  required_cols <- c("practice_id", "year", "month", "amount")
  missing_cols <- setdiff(required_cols, names(overhead_tbl))
  if (length(missing_cols) > 0) {
    rlang::abort(
      paste0(
        "Overhead tibble is missing required columns: ",
        paste(missing_cols, collapse = ", "),
        ". Did the data pass through filter_gnucash_overhead() or ingest_manual()?"
      ),
      class = "dcForecastR_missing_columns",
      missing_columns = missing_cols
    )
  }

  # Derive is_refund from amount sign if the column is absent. This mirrors the
  # behaviour of summarize_income_monthly() and guards against calling this
  # function directly on a tibble that hasn't passed through validate_overhead().
  if (!"is_refund" %in% names(overhead_tbl)) {
    overhead_tbl <- dplyr::mutate(overhead_tbl, is_refund = amount < 0)
  }

  if (!include_refunds) {
    overhead_tbl <- dplyr::filter(overhead_tbl, !is_refund)
  }

  overhead_tbl |>
    dplyr::group_by(practice_id, year, month) |>
    dplyr::summarise(
      total_overhead = sum(amount),
      gross_overhead = sum(amount[!is_refund]),
      total_refunds = -sum(amount[is_refund]),
      .groups = "drop"
    )
}


#' Summarize Overhead Expenses by Week
#'
#' Aggregates a normalized overhead tibble to one row per practice per week,
#' computing total overhead, gross overhead (before refunds), and the total
#' value of any refunds/credits. The output is the form expected by
#' \code{forecast_breakeven()} and \code{forecast_target()} when using weekly
#' data.
#'
#' @param overhead_tbl A data frame of overhead transactions as returned by
#'   \code{filter_gnucash_overhead()} or \code{ingest_manual(type = "overhead")}.
#'   Required columns: \code{practice_id}, \code{week_start}, \code{amount},
#'   \code{is_refund}.
#' @param include_refunds Logical. If \code{FALSE}, refund rows are dropped
#'   before aggregation so that \code{total_overhead} reflects only positive
#'   expense transactions (default: \code{TRUE}).
#'
#' @return A tibble with one row per \code{practice_id}/\code{week_start}
#'   combination and columns:
#'   \describe{
#'     \item{practice_id}{Practice identifier}
#'     \item{week_start}{Date of the Monday that opens the week}
#'     \item{total_overhead}{Net overhead after subtracting refunds}
#'     \item{gross_overhead}{Overhead from positive expense transactions only}
#'     \item{total_refunds}{Magnitude of refunds/credits (always non-negative)}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' overhead <- ingest_gnucash_csv("expenses.csv", practice_id = 1) |>
#'   filter_gnucash_overhead()
#'
#' summarize_overhead_weekly(overhead)
#' }
summarize_overhead_weekly <- function(overhead_tbl, include_refunds = TRUE) {
  if (!"week_start" %in% names(overhead_tbl)) {
    rlang::abort(
      paste0(
        "Overhead tibble is missing the 'week_start' column. ",
        "Did the data pass through ingest_gnucash_csv() or ingest_manual()?"
      ),
      class = "dcForecastR_missing_columns"
    )
  }

  if (!"is_refund" %in% names(overhead_tbl)) {
    overhead_tbl <- dplyr::mutate(overhead_tbl, is_refund = amount < 0)
  }

  if (!include_refunds) {
    overhead_tbl <- dplyr::filter(overhead_tbl, !is_refund)
  }

  overhead_tbl |>
    dplyr::group_by(practice_id, week_start) |>
    dplyr::summarise(
      total_overhead = sum(amount),
      gross_overhead = sum(amount[!is_refund]),
      total_refunds = -sum(amount[is_refund]),
      .groups = "drop"
    )
}
