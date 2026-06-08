#' Ingest GnuCash CSV Export
#'
#' Reads a GnuCash CSV export, maps account names to internal expense
#' categories, validates the result, and returns a normalized transaction
#' tibble. The tibble contains both income and expense rows; use
#' \code{normalize_gnucash_income()} or \code{filter_gnucash_overhead()} to
#' split them before summarizing.
#'
#' @param path Character string specifying the path to the GnuCash CSV file.
#' @param practice_id Character or integer practice identifier added to every
#'   row of the output.
#' @param account_map A tibble of account mapping rules as returned by
#'   \code{default_account_map()}. Override to customize category assignments
#'   for a specific practice.
#'
#' @return A tibble with columns: \code{practice_id}, \code{date},
#'   \code{week_start}, \code{month}, \code{year}, \code{full_account_name},
#'   \code{account_name}, \code{description}, \code{amount}, \code{category},
#'   and \code{source}. The \code{is_refund} column is added downstream by
#'   \code{filter_gnucash_overhead()} and \code{normalize_gnucash_income()}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load all transactions and split into income and overhead streams
#' transactions <- ingest_gnucash_csv(
#'   path        = "path/to/gnucash_export.csv",
#'   practice_id = "practice_001"
#' )
#'
#' income   <- normalize_gnucash_income(transactions)
#' overhead <- filter_gnucash_overhead(transactions)
#' }
ingest_gnucash_csv <- function(path,
                                practice_id,
                                account_map = default_account_map()) {
  raw_data <- readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::rename(
      account = "Account Name",
      amount  = "Amount Num.",
      date    = "Date"
    )
  mapped_data <- map_accounts(raw_data, account_map)
  normalize_gnucash_csv(mapped_data, practice_id, source = "gnucash_csv")
}


#' Ingest GnuCash XML File
#'
#' @param path Character string specifying the path to the GnuCash XML file.
#' @param practice_id Character or integer practice identifier.
#' @param account_map A tibble of account mapping rules as returned by
#'   \code{default_account_map()}.
#'
#' @noRd
ingest_gnucash_xml <- function(path, practice_id, account_map = default_account_map()) {
  rlang::abort(
    "GNUCash XML upload is not yet implemented.",
    class = "dcForecastR_not_implemented"
  )
}
