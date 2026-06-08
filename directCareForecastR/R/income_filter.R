
#' Filter GNUCash Data for Income
#'
#' Filters a GNUCash data frame to include only rows where the Full Account Name
#' contains "Income" and renames the "Amount" column to "Revenue".
#'
#' @param data A data frame with a "full_account_name" and "amount" column,
#'   typically from \code{ingest_gnucash_csv()}.
#'
#' @return A filtered data frame containing only income transactions with the
#'   "amount" column renamed to "revenue".
#'
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- ingest_gnucash_csv("path/to/gnucash_export.csv")
#' normalized_data <- normalize_gnucash_csv(raw_data)
#' income <- normalize_gnucash_income(normalized_data)
#' }
normalize_gnucash_income <- function(data) {
  new_data <- data[which(grepl("Income", data$full_account_name)), ]
  names(new_data)[names(new_data) == "amount"] <- "revenue"
  validate_income(new_data)
}
