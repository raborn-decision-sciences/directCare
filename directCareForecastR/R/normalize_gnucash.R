#' Normalize GnuCash CSV Data
#'
#' Selects and renames the columns produced by \code{map_accounts()}, parses
#' dates, and adds derived time columns (\code{week_start}, \code{month},
#' \code{year}). Called internally by \code{ingest_gnucash_csv()}.
#'
#' @param data A data frame from \code{map_accounts()} with columns
#'   \code{date}, \code{Full Account Name}, \code{account},
#'   \code{Description}, \code{amount}, and \code{category}.
#' @param practice_id Character or integer practice identifier.
#' @param source Character string recording the import source tag.
#'
#' @return A tibble with columns: \code{practice_id}, \code{date},
#'   \code{week_start}, \code{month}, \code{year}, \code{full_account_name},
#'   \code{account_name}, \code{description}, \code{amount}, \code{category},
#'   \code{source}.
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