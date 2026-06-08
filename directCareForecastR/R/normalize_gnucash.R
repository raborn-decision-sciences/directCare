#' Normalize GNUCash CSV Data
#'
#' Processes a GNUCash data frame to select key columns, parse dates, and add
#' time-based columns. Renames "Amount Num." to "Amount".
#'
#' @param data A data frame from \code{ingest_gnucash_csv()} containing GNUCash
#'   transaction data.
#'
#' @return A data frame with columns: Date, Full Account Name, Account Name,
#'   Description, Amount, Week, Month, Year, and Source.
#'
#' @noRd
normalize_gnucash_csv <- function(data, practice_id, source) {

  required_cols <- c(
    "date", "Full Account Name", "account", "Description", "amount", "category"
  )
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    rlang::abort(
      paste0(
        "GnuCash CSV is missing expected columns: ",
        paste(missing_cols, collapse = ", "),
        ". Was this exported from GnuCash with all columns included?"
      ),
      class = "directCareForecastR_missing_columns",
      missing_columns = missing_cols
    )
  }

  data |>
    dplyr::select(
      full_account_name = "Full Account Name",
      account_name      = "account",
      description       = "Description",
      amount,
      category
    ) |>
    dplyr::mutate(
      practice_id = practice_id,
      source      = source,
      date        = lubridate::mdy(data$date),
      week_start  = lubridate::floor_date(date, "week", week_start = 1),
      month       = lubridate::month(date),
      year        = lubridate::year(date)
    ) |>
    dplyr::relocate(practice_id, date, week_start, month, year)

}