#' Ingest QuickBooks Online CSV Export
#'
#' Reads a QuickBooks Online "Transaction List by Date" report exported to
#' CSV (Reports \eqn{\to} Transaction List by Date \eqn{\to} Export to CSV),
#' maps account names to internal expense categories, and returns a
#' normalized transaction tibble in the same shape as
#' \code{ingest_gnucash_csv()}.
#'
#' @param path Character string specifying the path to the QuickBooks CSV
#'   file.
#' @param practice_id Character or integer practice identifier added to every
#'   row of the output.
#' @param account_map A tibble of account mapping rules as returned by
#'   \code{default_account_map()}. Override to customize category assignments
#'   for a specific practice.
#'
#' @return A tibble with columns: \code{practice_id}, \code{date},
#'   \code{week_start}, \code{month}, \code{year}, \code{full_account_name},
#'   \code{account_name}, \code{description}, \code{amount}, \code{category},
#'   and \code{source} (\code{"quickbooks_csv"}). The \code{is_refund} column
#'   is added downstream by \code{filter_gnucash_overhead()} and
#'   \code{normalize_gnucash_income()}.
#'
#' @section Sign convention:
#' QuickBooks Online's "Transaction List by Date" report shows money leaving
#' the practice (bill payments, checks, expenses) as a negative amount, and
#' money coming in (invoices, sales receipts, deposits) as a positive amount.
#' This function uses that sign to classify each row: negative amounts become
#' overhead (stored as a positive dollar value under an \code{"Expenses:"}
#' account path) and positive amounts become income (stored under an
#' \code{"Income:"} account path) \eqn{-} consistent with
#' \code{filter_gnucash_overhead()} and \code{normalize_gnucash_income()},
#' which both key off that path prefix.
#'
#' @section Column detection:
#' Column names are matched case-insensitively against the default QuickBooks
#' Online export headers, with a few common aliases:
#' \describe{
#'   \item{Date}{\code{"Date"}}
#'   \item{Account}{\code{"Account"} or \code{"Account Name"}}
#'   \item{Amount}{\code{"Amount"} or \code{"Amount Num."}}
#'   \item{Description}{\code{"Memo/Description"}, \code{"Memo"}, or
#'     \code{"Description"} (optional)}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' transactions <- ingest_quickbooks_csv(
#'   path        = "path/to/quickbooks_export.csv",
#'   practice_id = "practice_001"
#' )
#'
#' income   <- normalize_gnucash_income(transactions)
#' overhead <- filter_gnucash_overhead(transactions)
#' }
ingest_quickbooks_csv <- function(
  path,
  practice_id,
  account_map = default_account_map()
) {
  raw <- readr::read_csv(path, show_col_types = FALSE)
  cols <- .qb_resolve_columns(raw)

  raw_data <- raw |>
    dplyr::rename(
      date = dplyr::all_of(cols$date),
      account = dplyr::all_of(cols$account),
      amount = dplyr::all_of(cols$amount)
    ) |>
    dplyr::mutate(
      description = if (!is.null(cols$description)) {
        as.character(.data[[cols$description]])
      } else {
        NA_character_
      },
      amount = suppressWarnings(as.numeric(amount))
    ) |>
    dplyr::filter(
      !is.na(amount),
      !is.na(account),
      nzchar(trimws(as.character(account)))
    )

  if (nrow(raw_data) == 0L) {
    rlang::abort(
      "No usable transaction rows found in the QuickBooks CSV export.",
      class = "dcForecastR_no_data"
    )
  }

  raw_data <- raw_data |>
    dplyr::mutate(
      account = as.character(account),
      full_account_name = paste0(
        ifelse(amount > 0, "Income:", "Expenses:"),
        account
      ),
      amount = abs(amount)
    )

  mapped_data <- map_accounts(raw_data, account_map)

  mapped_data |>
    dplyr::transmute(
      practice_id = practice_id,
      date = .qb_parse_date(date),
      full_account_name,
      account_name = account,
      description,
      amount,
      category,
      source = "quickbooks_csv"
    ) |>
    dplyr::mutate(
      week_start = lubridate::floor_date(date, "week", week_start = 1),
      month = lubridate::month(date),
      year = lubridate::year(date)
    ) |>
    dplyr::relocate(
      practice_id,
      date,
      week_start,
      month,
      year,
      full_account_name,
      account_name,
      description,
      amount,
      category,
      source
    )
}


# Internal helpers ------------------------------------------------------------

# Resolve the Date / Account / Amount / (optional) Description columns from a
# raw QuickBooks CSV export, matching case-insensitively against the default
# header and a few known aliases. Aborts with a descriptive error listing the
# columns actually present when a required column can't be found.
.qb_resolve_columns <- function(raw) {
  names_lower <- tolower(trimws(names(raw)))

  find_col <- function(candidates) {
    for (cand in candidates) {
      idx <- match(tolower(cand), names_lower)
      if (!is.na(idx)) {
        return(names(raw)[idx])
      }
    }
    NULL
  }

  date_col <- find_col("date")
  account_col <- find_col(c("account", "account name"))
  amount_col <- find_col(c("amount", "amount num."))
  description_col <- find_col(c("memo/description", "memo", "description"))

  missing <- c(
    if (is.null(date_col)) "Date",
    if (is.null(account_col)) "Account",
    if (is.null(amount_col)) "Amount"
  )
  if (length(missing) > 0L) {
    rlang::abort(
      paste0(
        "QuickBooks CSV is missing expected columns: ",
        paste(missing, collapse = ", "),
        ". Columns found: ",
        paste(names(raw), collapse = ", "),
        ". Was this exported as a Transaction List by Date report?"
      ),
      class = "dcForecastR_missing_columns",
      missing_columns = missing
    )
  }

  list(
    date = date_col,
    account = account_col,
    amount = amount_col,
    description = description_col
  )
}

# Parse QuickBooks export dates, which are typically MM/DD/YYYY but
# occasionally YYYY-MM-DD depending on locale/export settings.
.qb_parse_date <- function(x) {
  parsed <- suppressWarnings(lubridate::parse_date_time(
    x,
    orders = c("mdy", "ymd")
  ))
  as.Date(parsed)
}


#' Ingest Quickbooks file
#' Note yet implemented!
#' @noRd
ingest_quickbooks_xml <- function(
  path,
  practice_id,
  account_map = default_account_map()
) {
  rlang::abort(
    "Quickbooks upload is not yet implemented.",
    class = "dcForecastR_not_implemented"
  )
}
