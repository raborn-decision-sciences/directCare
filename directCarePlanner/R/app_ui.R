#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`. DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    page_navbar(
      title = tagList(
        tags$img(
          src = "www/favicon.svg",
          height = "28px",
          width = "28px",
          alt = "RDS"
        ),
        tags$span(
          style = "font-weight:600;font-size:0.95rem;white-space:nowrap;",
          "Direct Care Practice Launch Planner"
        )
      ),
      id = "main_nav",
      theme = rds_theme(),
      nav_panel(
        title = tagList(bsicons::bs_icon("clipboard-data"), " Plan Inputs"),
        value = "plan_inputs",
        mod_plan_inputs_ui("plan_inputs")
      ),
      nav_panel(
        title = tagList(bsicons::bs_icon("graph-up-arrow"), " Results"),
        value = "results",
        mod_results_ui("results")
      )
    )
  )
}

#' Raborn Decision Sciences bslib theme
#'
#' Ported from directCareAnalytics's `rds_theme()` -- same brand colors,
#' shared across the RDS product family. No `_brand.yml` exists there
#' either; this reuses the same literal-hex pattern, not a new packaging
#' of it.
#'
#' @noRd
rds_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = "#F8FAFC",
    fg = "#172033",
    primary = "#14B8A6",
    secondary = "#2F3A4A",
    success = "#16A34A",
    info = "#2563EB",
    warning = "#F59E0B",
    danger = "#DC2626",
    base_font = bslib::font_google("Atkinson Hyperlegible"),
    code_font = bslib::font_google("Fira Code"),
    "font-size-base" = "1rem",
    "line-height-base" = "1.6",
    "headings-color" = "#172033",
    "link-color" = "#0d9488",
    "link-hover-color" = "#0f766e",
    "letter-spacing" = "-0.005em",
    "navbar-bg" = "#172033",
    "navbar-light-color" = "#F8FAFC",
    "navbar-light-active-color" = "#F8FAFC",
    "navbar-light-hover-color" = "#2DD4BF",
    "navbar-light-brand-color" = "#F8FAFC",
    "navbar-light-brand-hover-color" = "#2DD4BF",
    "code-bg" = "#EEF2F7",
    "code-color" = "#2F3A4A"
  )
}

#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))

  tags$head(
    tags$link(rel = "icon", type = "image/svg+xml", href = "www/favicon.svg"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Direct Care Practice Launch Planner | Raborn Decision Sciences"
    ),
    tags$link(rel = "stylesheet", href = "www/custom.css")
  )
}

# Branded footer, matching directCareAnalytics's mod_upload.R. Appended to
# each tab's own UI (not as a page_navbar() sibling) since page_navbar()
# manages full-page fill layout and a sibling div's placement there is
# untested.
#' @noRd
.branded_footer <- function() {
  tags$div(
    class = "mt-5 pt-3 border-top d-flex align-items-center justify-content-center gap-3",
    style = "opacity:0.55;",
    tags$img(
      src = "www/logo-rds-alt.svg",
      height = "36px",
      alt = "Raborn Decision Sciences"
    )
  )
}
