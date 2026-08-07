# Suppress R CMD check NOTEs for bare imported symbols and data-masking
# column names used in ggplot2 contexts throughout the app.

#' @importFrom bslib bs_theme card card_body card_header layout_columns accordion accordion_panel input_task_button value_box page_fluid page_navbar nav_panel nav_spacer nav_item input_dark_mode
#' @importFrom bsicons bs_icon
#' @importFrom stats setNames
NULL

# NULL-coalescing operator for input$ values that are NULL before first
# render. Matches directCareAnalytics's R/utils_globals.R convention.
`%||%` <- function(x, y) if (is.null(x)) y else x

#' JS handler that redirects the browser to a Stripe-hosted URL
#'
#' Matches directCareAnalytics's R/utils_globals.R convention -- see that
#' file's own roxygen comment for the full rationale.
#' @noRd
.redirect_script <- function() {
  tags$script(HTML(
    "$(document).on('shiny:connected', function() {
      Shiny.addCustomMessageHandler('redirectTo', function(msg) {
        window.top.location.href = msg.url;
      });
    });"
  ))
}

#' Branded header shared by the login page and the signup page
#'
#' Matches directCareAnalytics's R/utils_globals.R convention -- see that
#' file's own roxygen comment for the full rationale, including why the
#' icon+tagline pair below exists (this app's own visual identifier,
#' distinct from DCA's, since the RDS favicon/palette are identical across
#' the product family).
#' @noRd
.auth_page_chrome <- function(title, dark_mode_id = "dark_mode") {
  tags$div(
    style = "text-align:center;position:relative;",
    tags$div(
      style = "position:absolute;top:0;right:0;",
      input_dark_mode(id = dark_mode_id)
    ),
    tags$img(src = "www/favicon.svg", height = "48px", alt = "RDS"),
    tags$h4(
      style = "margin-top:8px;font-weight:600;color:var(--bs-emphasis-color);",
      title
    ),
    tags$p(
      class = "text-muted small mb-0",
      style = "max-width:320px;margin:2px auto 0;",
      bs_icon("compass"), " Model your launch costs, revenue, and timeline before you open."
    )
  )
}

utils::globalVariables(c("month", "net_income", "scenario"))
