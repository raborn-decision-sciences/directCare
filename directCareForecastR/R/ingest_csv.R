#' Ingest a Generic CSV File
#'
#' Reads a CSV file whose column names are specified by the caller, maps them
#' to the internal schema, and runs the appropriate validation. This is the
#' entry point for practices that export data from a system other than GnuCash
#' — e.g. a simple spreadsheet export or a bank statement download.
#'
#' The default (\code{type = "both"}) expects a single file containing both
#' income and expense rows, identified by a column whose values match
#' \code{overhead_pattern} or \code{income_pattern}. Set \code{type =
#' "overhead"} or \code{type = "income"} to load a file that contains only one
#' type of transaction.
#'
#' @param path Character string. Path to the CSV file.
#' @param practice_id Character or integer practice identifier added to every
#'   row of the output.
#' @param col_date Name of the date column in the CSV (character scalar).
#' @param col_amount Name of the amount/revenue column in the CSV (character
#'   scalar). Values should be positive for both expenses and income; negative
#'   values are treated as refunds.
#' @param type Character string: \code{"both"} (default), \code{"overhead"},
#'   or \code{"income"}. \code{"both"} splits the file into overhead and income
#'   based on \code{col_type} and returns a named list. The single-type options
#'   return a validated tibble directly.
#' @param col_type Name of the column in the CSV that identifies whether each
#'   row is an expense or income transaction. Required when \code{type =
#'   "both"}; ignored otherwise.
#' @param overhead_pattern A case-insensitive regular expression matched
#'   against \code{col_type} values to identify overhead/expense rows. Defaults
#'   to \code{"expense"}.
#' @param income_pattern A case-insensitive regular expression matched against
#'   \code{col_type} values to identify income rows. Defaults to
#'   \code{"income"}.
#' @param col_category Name of a category column in the CSV. Optional. When
#'   omitted, overhead rows are assigned \code{"other"}; income rows do not
#'   require a category.
#' @param col_description Name of a description column in the CSV. Optional.
#'   When omitted, description is set to \code{NA}.
#' @param date_format A \code{strptime} format string (e.g.
#'   \code{"\%m/\%d/\%Y"}) for the date column. When \code{NULL} (the default)
#'   the function tries the four most common formats in order:
#'   \code{"\%m/\%d/\%Y"}, \code{"\%Y-\%m-\%d"}, \code{"\%m/\%d/\%y"},
#'   \code{"\%d/\%m/\%Y"}.
#'
#' @return When \code{type = "both"}: a named list with elements
#'   \code{$overhead} and \code{$income}, each a validated tibble in the
#'   canonical schema. When \code{type = "overhead"} or \code{"income"}: a
#'   single validated tibble with \code{amount} or \code{revenue} respectively.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Mixed file — one upload covers both income and overhead
#' txn_path <- system.file("extdata", "sample_transactions.csv",
#'                         package = "directCareForecastR")
#' result <- ingest_csv_generic(
#'   path             = txn_path,
#'   practice_id      = 1,
#'   col_date         = "date",
#'   col_amount       = "amount",
#'   col_type         = "type",
#'   col_category     = "category",
#'   col_description  = "description"
#' )
#' result$overhead  # validated overhead tibble
#' result$income    # validated income tibble
#'
#' # Overhead-only file
#' overhead <- ingest_csv_generic(
#'   path        = "overhead.csv",
#'   practice_id = 1,
#'   col_date    = "date",
#'   col_amount  = "amount",
#'   type        = "overhead"
#' )
#' }
ingest_csv_generic <- function(
  path,
  practice_id,
  col_date,
  col_amount,
  type = c("both", "overhead", "income"),
  col_type = NULL,
  overhead_pattern = "expense",
  income_pattern = "income",
  col_category = NULL,
  col_description = NULL,
  date_format = NULL
) {
  type <- match.arg(type)

  raw <- readr::read_csv(path, show_col_types = FALSE)

  # When type = "both", col_type must be provided before any CSV read.
  if (type == "both" && is.null(col_type)) {
    rlang::abort(
      paste0(
        "col_type must be specified when type = 'both'. ",
        "Supply the name of the column in your CSV that identifies whether ",
        "each row is an expense or income transaction ",
        "(e.g. col_type = \"type\")."
      ),
      class = "dcForecastR_missing_columns",
      missing_columns = "col_type"
    )
  }

  # Validate required columns are present in the CSV.
  required_in <- c(col_date, col_amount, if (type == "both") col_type else NULL)
  missing_in <- setdiff(required_in, names(raw))
  if (length(missing_in) > 0) {
    rlang::abort(
      paste0(
        "The following columns were not found in ",
        basename(path),
        ": ",
        paste(missing_in, collapse = ", "),
        ". ",
        "Check that col_date, col_amount",
        if (type == "both") ", and col_type" else "",
        " match the actual column names."
      ),
      class = "dcForecastR_missing_columns",
      missing_columns = missing_in
    )
  }

  # Warn and nullify any optional column argument that points to a missing column.
  optional_in <- c(col_category, col_description)
  missing_opt <- setdiff(optional_in, names(raw))
  if (length(missing_opt) > 0) {
    rlang::warn(
      paste0(
        "Optional column(s) not found in ",
        basename(path),
        " and will be ignored: ",
        paste(missing_opt, collapse = ", ")
      ),
      class = "dcForecastR_missing_optional_columns"
    )
    if (!is.null(col_category) && col_category %in% missing_opt) {
      col_category <- NULL
    }
    if (!is.null(col_description) && col_description %in% missing_opt) {
      col_description <- NULL
    }
  }

  # Parse dates.
  date_raw <- raw[[col_date]]
  dates <- if (!is.null(date_format)) {
    as.Date(date_raw, format = date_format)
  } else {
    as.Date(
      lubridate::parse_date_time(
        date_raw,
        orders = c("mdy", "ymd", "mdy HM", "ymd HMS"),
        quiet = TRUE
      )
    )
  }

  if (anyNA(dates)) {
    rlang::abort(
      paste0(
        sum(is.na(dates)),
        " date(s) in column '",
        col_date,
        "' could not be parsed. ",
        "Supply date_format (e.g. date_format = \"%d/%m/%Y\") to specify the format explicitly."
      ),
      class = "dcForecastR_invalid_dates"
    )
  }

  # Build a base tibble with amount (renamed to revenue later for income rows).
  base_tbl <- tibble::tibble(
    practice_id = practice_id,
    date = dates,
    week_start = lubridate::floor_date(dates, "week", week_start = 1),
    month = lubridate::month(dates),
    year = lubridate::year(dates),
    full_account_name = NA_character_,
    account_name = NA_character_,
    description = if (!is.null(col_description)) {
      as.character(raw[[col_description]])
    } else {
      NA_character_
    },
    amount = as.numeric(raw[[col_amount]]),
    category = if (!is.null(col_category)) {
      as.character(raw[[col_category]])
    } else {
      "other"
    },
    source = "generic_csv"
  )

  # Fill NA categories from the col_category column with "other".
  # (Income rows in a mixed file often have a blank category cell.)
  base_tbl$category[is.na(base_tbl$category)] <- "other"

  if (type != "both") {
    # Single-type path: rename amount to revenue for income, then validate.
    if (type == "income") {
      base_tbl <- dplyr::rename(base_tbl, revenue = amount)
      return(validate_income(base_tbl))
    } else {
      return(validate_overhead(base_tbl))
    }
  }

  # ── type = "both" ──────────────────────────────────────────────────────────
  # col_type existence was already checked in the required-columns guard above.

  type_vals <- as.character(raw[[col_type]])
  is_overhead <- grepl(overhead_pattern, type_vals, ignore.case = TRUE)
  is_income <- grepl(income_pattern, type_vals, ignore.case = TRUE)

  # When a row matches both patterns, overhead takes priority (first-match-wins,
  # matching the behaviour of map_accounts()).
  is_income[is_overhead] <- FALSE

  n_unmatched <- sum(!is_overhead & !is_income)
  if (n_unmatched > 0) {
    unmatched_vals <- unique(type_vals[!is_overhead & !is_income])
    rlang::warn(
      paste0(
        n_unmatched,
        " row(s) in column '",
        col_type,
        "' matched neither '",
        overhead_pattern,
        "' (overhead) nor '",
        income_pattern,
        "' (income) ",
        "and were dropped. Unrecognised value(s): ",
        paste(unmatched_vals, collapse = ", "),
        ". ",
        "Adjust overhead_pattern or income_pattern if these rows should be included."
      ),
      class = "dcForecastR_unclassified_rows",
      n_unmatched = n_unmatched,
      unmatched_values = unmatched_vals
    )
  }

  overhead_tbl <- base_tbl[is_overhead, ] |> validate_overhead()
  income_tbl <- base_tbl[is_income, ] |>
    dplyr::rename(revenue = amount) |>
    validate_income()

  list(overhead = overhead_tbl, income = income_tbl)
}
