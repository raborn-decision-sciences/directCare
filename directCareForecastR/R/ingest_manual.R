#' Ingest Manually Entered Financial Data
#'
#' Accepts a user-supplied data frame of overhead or income transactions and
#' normalizes it into the canonical schema used throughout the package. This is
#' the entry point for practices that do not use GnuCash or QuickBooks, or for
#' adding one-off transactions that are not in an existing bookkeeping file.
#'
#' @param df A data frame of transactions. Required columns differ by
#'   \code{type}:
#'   \itemize{
#'     \item \strong{overhead}: \code{date}, \code{full_account_name},
#'       \code{account_name}, \code{description}, \code{amount}. An optional
#'       \code{category} column may also be provided.
#'     \item \strong{income}: same columns but with \code{revenue} (or
#'       \code{amount}) in place of \code{amount}.
#'   }
#'   Column names are matched case-insensitively and spaces are converted to
#'   underscores, so \code{"Full Account Name"} and \code{full_account_name}
#'   both work.
#' @param practice_id Character or integer practice identifier added to every
#'   row of the output.
#' @param type Character string: \code{"overhead"} (default) or
#'   \code{"income"}.
#'
#' @return A tibble in the canonical normalized schema with columns
#'   \code{practice_id}, \code{date}, \code{week_start}, \code{month},
#'   \code{year}, \code{full_account_name}, \code{account_name},
#'   \code{description}, \code{amount} (overhead) or \code{revenue} (income),
#'   \code{category}, and \code{source} (set to \code{"manual"}).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' overhead_df <- data.frame(
#'   date             = as.Date(c("2025-01-15", "2025-02-01")),
#'   full_account_name = c("Expenses:Rent", "Expenses:Utilities"),
#'   account_name     = c("Rent", "Utilities"),
#'   description      = c("Office rent", "Electric bill"),
#'   amount           = c(1200, 150)
#' )
#'
#' ingest_manual(overhead_df, practice_id = 1, type = "overhead")
#' }
ingest_manual <- function(df, practice_id, type = c("overhead", "income")) {
  type <- match.arg(type)
  if (type == "overhead") {
    normalize_overhead_manual(df, practice_id, source = "manual")
  } else {
    normalize_income_manual(df, practice_id, source = "manual") |>
      validate_income()
  }
}

#' Normalize Manual Overhead Data
#'
#' Processes manually entered overhead data to match the structure of normalized
#' GNUCash data. Expects a data frame with columns for date, account information,
#' description, and amount.
#'
#' @param df A data frame containing manual overhead entries with columns:
#'   date (or Date), full_account_name (or Full Account Name), account_name
#'   (or Account Name), description (or Description), amount (or Amount),
#'   and optionally category.
#' @param practice_id Character string identifying the practice.
#' @param source Character string identifying the data source (default: "manual").
#'
#' @return A data frame with columns: practice_id, date, week_start, month, year,
#'   full_account_name, account_name, description, amount, category, and source.
#'
#' @noRd
normalize_overhead_manual <- function(df, practice_id, source = "manual") {

  # Standardize column names if they come in different formats
  names(df) <- tolower(gsub(" ", "_", names(df)))

  required_cols <- c("date", "full_account_name", "account_name", "description", "amount")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    rlang::abort(
      paste0(
        "Manual overhead data is missing expected columns: ",
        paste(missing_cols, collapse = ", "),
        ". Please provide: date, full_account_name, account_name, description, amount"
      ),
      class = "directCareForecastR_missing_columns",
      missing_columns = missing_cols
    )
  }

  # Add category column if not present
  if (!"category" %in% names(df)) {
    df$category <- NA_character_
  }

  df |>
    dplyr::select(
      full_account_name,
      account_name,
      description,
      amount,
      category,
      date
    ) |>
    dplyr::mutate(
      practice_id = practice_id,
      source      = source,
      date        = lubridate::as_date(date),
      week_start  = lubridate::floor_date(date, "week", week_start = 1),
      month       = lubridate::month(date),
      year        = lubridate::year(date)
    ) |>
    dplyr::relocate(practice_id, date, week_start, month, year)

}

#' Normalize Manual Income Data
#'
#' Processes manually entered income data to match the structure of normalized
#' GNUCash income data. Expects a data frame with columns for date, account
#' information, description, and revenue.
#'
#' @param df A data frame containing manual income entries with columns:
#'   date (or Date), full_account_name (or Full Account Name), account_name
#'   (or Account Name), description (or Description), revenue (or Revenue or
#'   amount or Amount), and optionally category.
#' @param practice_id Character string identifying the practice.
#' @param source Character string identifying the data source (default: "manual").
#'
#' @return A data frame with columns: practice_id, date, week_start, month, year,
#'   full_account_name, account_name, description, revenue, category, and source.
#'
#' @noRd
normalize_income_manual <- function(df, practice_id, source = "manual") {

  # Standardize column names if they come in different formats
  names(df) <- tolower(gsub(" ", "_", names(df)))

  # Handle revenue/amount column name variations
  if ("revenue" %in% names(df)) {
    amount_col <- "revenue"
  } else if ("amount" %in% names(df)) {
    amount_col <- "amount"
  } else {
    rlang::abort(
      "Manual income data must have either 'revenue' or 'amount' column",
      class = "directCareForecastR_missing_columns"
    )
  }

  required_cols <- c("date", "full_account_name", "account_name", "description", amount_col)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    rlang::abort(
      paste0(
        "Manual income data is missing expected columns: ",
        paste(missing_cols, collapse = ", "),
        ". Please provide: date, full_account_name, account_name, description, revenue"
      ),
      class = "directCareForecastR_missing_columns",
      missing_columns = missing_cols
    )
  }

  # Add category column if not present
  if (!"category" %in% names(df)) {
    df$category <- NA_character_
  }

  df |>
    dplyr::select(
      full_account_name,
      account_name,
      description,
      revenue = dplyr::all_of(amount_col),
      category,
      date
    ) |>
    dplyr::mutate(
      practice_id = practice_id,
      source      = source,
      date        = lubridate::as_date(date),
      week_start  = lubridate::floor_date(date, "week", week_start = 1),
      month       = lubridate::month(date),
      year        = lubridate::year(date)
    ) |>
    dplyr::relocate(practice_id, date, week_start, month, year)

}
