
#' Filter GNUCash Data for Overhead Expenses
#'
#' Filters a GNUCash data frame to include only rows where the Full Account Name
#' contains "Expenses".
#'
#' @param data A data frame with a "full_account_name" column, typically from
#'   \code{normalize_gnucash_csv()}.
#'
#' @return A filtered data frame containing only expense transactions.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- ingest_gnucash_csv("path/to/gnucash_export.csv")
#' normalized_data <- normalize_gnucash_csv(raw_data)
#' overhead <- filter_gnucash_overhead(normalized_data)
#' }
filter_gnucash_overhead <- function(data) {
  
  new_data <- data[which(grepl("Expenses", data$full_account_name)),]

  new_data

}

