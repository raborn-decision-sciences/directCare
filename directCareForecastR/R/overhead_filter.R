#' Filter Transactions to Overhead Expenses
#'
#' Filters a normalized transaction tibble to rows whose
#' \code{full_account_name} contains \code{"Expenses"}, returning only expense
#' transactions. Pipe the result into \code{summarize_overhead_monthly()} to
#' produce the monthly totals used by the forecasting functions.
#'
#' @param data A tibble as returned by \code{ingest_gnucash_csv()} or
#'   \code{ingest_manual(type = "overhead")}, with a \code{full_account_name}
#'   column.
#'
#' @return A filtered tibble containing only expense transactions, with the
#'   same columns as the input.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' overhead <- ingest_gnucash_csv("path/to/gnucash_export.csv", practice_id = 1) |>
#'   filter_gnucash_overhead() |>
#'   summarize_overhead_monthly()
#' }
filter_gnucash_overhead <- function(data) {
  data[which(grepl("Expenses", data$full_account_name)), ] |>
    validate_overhead()
}
