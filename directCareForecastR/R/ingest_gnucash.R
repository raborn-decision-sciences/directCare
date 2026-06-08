#' Ingest GnuCash CSV File
#'
#' Reads a GnuCash CSV export, maps account names to internal categories,
#' and returns a normalized overhead tibble.
#'
#' @param path Character string specifying the path to the GnuCash CSV file.
#' @param practice_id Character or integer practice identifier.
#' @param account_map A tibble of account mapping rules as returned by
#'   \code{default_account_map}. Override to customize category
#'   assignments for a specific practice.
#'
#' @return A tibble conforming to the canonical overhead record format with
#'   columns: \code{practice_id}, \code{date}, \code{week_start},
#'   \code{month}, \code{year}, \code{full_account_name},
#'   \code{account_name}, \code{description}, \code{amount},
#'   \code{category}, \code{source}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' overhead <- ingest_gnucash_csv(
#'   path        = "path/to/gnucash_export.csv",
#'   practice_id = "practice_001"
#' )
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
  normalized  <- normalize_gnucash_csv(mapped_data, practice_id, source = "gnucash_csv")
  
  validate_overhead(normalized)
  
}

#' Ingest GNUCash XML file
#' Note yet implemented!
#' 
#' @param path Character string specifying the path to the GnuCash XML file.
#' @param practice_id Character or integer practice identifier.
#' @param account_map A tibble of account mapping rules as returned by
#'   \code{default_account_map}. Override to customize category
#'   assignments for a specific practice.
#' @noRd
#' @keywords internal
ingest_gnucash_xml <- function(path, practice_id, account_map = default_account_map()) {
  rlang::abort(
    "GNUCash XML upload is not yet implemented.",
    class = "directCareForecastR_not_implemented"
  )
}
