
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