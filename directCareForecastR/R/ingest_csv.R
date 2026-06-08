#' Ingest a Generic CSV File
#'
#' @param path Path to the CSV file.
#' @param practice_id Practice identifier.
#' @param col_date Name of the date column in the CSV.
#' @param col_amount Name of the amount column in the CSV.
#' @param col_category Optional name of a category column.
#' @param col_description Optional name of a description column.
#'
#' @return A normalized tibble (not yet implemented).
#'
#' @noRd
ingest_csv_generic <- function(path, practice_id,
                               col_date, col_amount,
                               col_category = NULL,
                               col_description = NULL) {
  rlang::abort(
    "Generic CSV upload is not yet implemented.",
    class = "dcForecastR_not_implemented"
  )
}
