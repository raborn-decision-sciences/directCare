# Suppress R CMD check NOTEs for bare imported symbols and data-masking
# column names used in ggplot2 contexts throughout the app.

#' @importFrom bslib bs_theme card card_body card_header layout_columns accordion accordion_panel input_task_button value_box page_navbar nav_panel nav_spacer nav_item input_dark_mode
#' @importFrom bsicons bs_icon
#' @importFrom stats setNames
NULL

# NULL-coalescing operator for input$ values that are NULL before first
# render. Matches directCareAnalytics's R/utils_globals.R convention.
`%||%` <- function(x, y) if (is.null(x)) y else x

utils::globalVariables(c("month", "net_income", "scenario"))
