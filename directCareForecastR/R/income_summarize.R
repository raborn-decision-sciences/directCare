#' Summarize Income by Month
#'
#' Aggregates a normalized income tibble to one row per practice per month,
#' computing total revenue, gross revenue (before refunds/chargebacks), and the
#' total value of any refunds. The output schema matches what
#' \code{forecast_breakeven()} and \code{forecast_target()} expect on the
#' income side.
#'
#' Negative revenue rows (e.g. cancelled memberships, chargebacks) are treated
#' as refunds. If the tibble already contains an \code{is_refund} column (added
#' by \code{ingest_gnucash_csv()}) it is used directly; otherwise it is derived
#' from the sign of \code{revenue}.
#'
#' @param income_tbl A data frame of income transactions as returned by
#'   \code{normalize_gnucash_income()} or \code{ingest_manual(type = "income")}.
#'   Required columns: \code{practice_id}, \code{year}, \code{month},
#'   \code{revenue}.
#' @param include_refunds Logical. If \code{FALSE}, refund rows are dropped
#'   before aggregation so that \code{total_revenue} reflects only positive
#'   transactions (default: \code{TRUE}).
#'
#' @return A tibble with one row per \code{practice_id}/\code{year}/\code{month}
#'   combination and columns:
#'   \describe{
#'     \item{practice_id}{Practice identifier}
#'     \item{year}{Calendar year}
#'     \item{month}{Calendar month (integer 1–12)}
#'     \item{total_revenue}{Net revenue after refunds}
#'     \item{gross_revenue}{Revenue from positive transactions only}
#'     \item{total_refunds}{Magnitude of refunds (always non-negative)}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' income <- ingest_gnucash_csv("income.csv", practice_id = 1) |>
#'   normalize_gnucash_income()
#'
#' monthly <- summarize_income_monthly(income)
#' }
summarize_income_monthly <- function(income_tbl, include_refunds = TRUE) {
  required_cols <- c("practice_id", "year", "month", "revenue")
  missing_cols <- setdiff(required_cols, names(income_tbl))
  if (length(missing_cols) > 0) {
    rlang::abort(
      paste0(
        "Income tibble is missing required columns: ",
        paste(missing_cols, collapse = ", "),
        ". Did the data pass through normalize_gnucash_income() or ingest_manual()?"
      ),
      class = "dcForecastR_missing_columns",
      missing_columns = missing_cols
    )
  }

  # Derive is_refund from revenue sign if not already present
  if (!"is_refund" %in% names(income_tbl)) {
    income_tbl <- dplyr::mutate(income_tbl, is_refund = revenue < 0)
  }

  if (!include_refunds) {
    income_tbl <- dplyr::filter(income_tbl, !is_refund)
  }

  income_tbl |>
    dplyr::group_by(practice_id, year, month) |>
    dplyr::summarise(
      total_revenue = sum(revenue),
      gross_revenue = sum(revenue[!is_refund]),
      total_refunds = -sum(revenue[is_refund]),
      .groups = "drop"
    )
}


#' Summarize Income by Week
#'
#' Aggregates a normalized income tibble to one row per practice per week,
#' computing total revenue, gross revenue, and refunds. Use this when you want
#' to feed weekly income data into \code{forecast_breakeven()} or
#' \code{forecast_revenue()}.
#'
#' Negative revenue rows are treated as refunds. If the tibble already contains
#' an \code{is_refund} column it is used directly; otherwise it is derived from
#' the sign of \code{revenue}.
#'
#' @param income_tbl A data frame of income transactions as returned by
#'   \code{normalize_gnucash_income()} or \code{ingest_manual(type = "income")}.
#'   Required columns: \code{practice_id}, \code{week_start}, \code{revenue}.
#' @param include_refunds Logical. If \code{FALSE}, refund rows are dropped
#'   before aggregation (default: \code{TRUE}).
#'
#' @return A tibble with one row per \code{practice_id}/\code{week_start}
#'   combination and columns:
#'   \describe{
#'     \item{practice_id}{Practice identifier}
#'     \item{week_start}{Date of the Monday that opens the week}
#'     \item{total_revenue}{Net revenue after refunds}
#'     \item{gross_revenue}{Revenue from positive transactions only}
#'     \item{total_refunds}{Magnitude of refunds (always non-negative)}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' income <- ingest_gnucash_csv("income.csv", practice_id = 1) |>
#'   normalize_gnucash_income()
#'
#' weekly <- summarize_income_weekly(income)
#' }
summarize_income_weekly <- function(income_tbl, include_refunds = TRUE) {
  required_cols <- c("practice_id", "week_start", "revenue")
  missing_cols <- setdiff(required_cols, names(income_tbl))
  if (length(missing_cols) > 0) {
    rlang::abort(
      paste0(
        "Income tibble is missing required columns: ",
        paste(missing_cols, collapse = ", "),
        ". Did the data pass through normalize_gnucash_income() or ingest_manual()?"
      ),
      class = "dcForecastR_missing_columns",
      missing_columns = missing_cols
    )
  }

  # Derive is_refund from revenue sign if not already present
  if (!"is_refund" %in% names(income_tbl)) {
    income_tbl <- dplyr::mutate(income_tbl, is_refund = revenue < 0)
  }

  if (!include_refunds) {
    income_tbl <- dplyr::filter(income_tbl, !is_refund)
  }

  income_tbl |>
    dplyr::group_by(practice_id, week_start) |>
    dplyr::summarise(
      total_revenue = sum(revenue),
      gross_revenue = sum(revenue[!is_refund]),
      total_refunds = -sum(revenue[is_refund]),
      .groups = "drop"
    )
}
