# Suppress R CMD check NOTEs for bare column names and imported symbols
# used in data-masking and UI contexts throughout the app.

#' @importFrom bslib bs_theme card card_body card_footer card_header input_switch input_task_button layout_column_wrap layout_columns nav_item nav_panel nav_spacer navset_card_underline navset_tab page_navbar page_sidebar sidebar accordion accordion_panel tooltip value_box
#' @importFrom bsicons bs_icon
#' @importFrom shiny NS moduleServer observeEvent reactive reactiveVal reactiveValues renderUI req uiOutput updateSelectInput updateDateRangeInput showNotification downloadButton downloadHandler actionButton fileInput textInput numericInput selectInput sliderInput dateInput dateRangeInput radioButtons renderPlot plotOutput icon tagList tags hr p HTML
#' @importFrom dplyr where
#' @importFrom stats setNames
#' @importFrom scales dollar dollar_format
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
#' @importFrom tools toTitleCase
#' @importFrom utils head tail
NULL

# NULL-coalescing operator for input$ values that are NULL before first render
`%||%` <- function(x, y) if (is.null(x)) y else x

# App-wide dollar formatter — always two decimal places.
fmt_dollar <- function(x, ...) scales::dollar(x, accuracy = 0.01, ...)
fmt_dollar_format <- function(...) scales::dollar_format(accuracy = 0.01, ...)

utils::globalVariables(c(
  "account_name",
  "amount",
  "category",
  "description",
  "full_account_name",
  "label",
  "month",
  "observed_overhead",
  "observed_revenue",
  "overhead_lower",
  "overhead_upper",
  "overhead_forecast",
  "Period",
  "period_start",
  "required_revenue",
  "revenue",
  "revenue_forecast",
  "revenue_lower",
  "revenue_upper",
  "source",
  "total",
  "total_overhead",
  "total_revenue",
  "week_start",
  "year"
))
