#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`. DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    page_navbar(
      title = tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('brand_click', Math.random(), {priority: 'event'}); return false;",
        style = "display: flex; align-items: center; gap: 8px; color: inherit; text-decoration: none;",
        tags$img(
          src = "www/rds-logo.png",
          height = "28px",
          alt = "Raborn Decision Sciences"
        ),
        "Direct Care Analytics"
      ),
      id = "main_nav",
      theme = rds_theme(),
      # -- Tabs --------------------------------------------------------------
      nav_panel(
        title = tagList(bs_icon("upload"), " Upload"),
        value = "upload",
        mod_upload_ui("upload")
      ),
      nav_panel(
        title = tagList(bs_icon("pencil-square"), " Review & Edit"),
        value = "edit",
        mod_edit_ui("edit")
      ),
      nav_panel(
        title = tagList(bs_icon("bar-chart-line"), " Summary"),
        value = "summary",
        mod_summary_ui("summary")
      ),
      nav_panel(
        title = tagList(bs_icon("graph-up-arrow"), " Projections"),
        value = "projections",
        mod_projections_ui("projections")
      ),
      # -- Right-side items --------------------------------------------------
      nav_spacer(),
      nav_item(
        tags$a(
          bs_icon("question-circle", title = "Help"),
          href = "#",
          title = "Help",
          class = "nav-link",
          onclick = "Shiny.setInputValue('help_click', Math.random(), {priority: 'event'}); return false;"
        )
      )
    )
  )
}

#' Raborn Decision Sciences bslib theme
#' @noRd
rds_theme <- function() {
  bs_theme(
    version = 5,
    # -- Brand colours (_brand.yml) -----------------------------------------
    bg = "#F8FAFC", # off-white
    fg = "#172033", # deep-navy
    primary = "#14B8A6", # teal
    secondary = "#2F3A4A", # charcoal-slate
    success = "#16A34A",
    info = "#2563EB",
    warning = "#F59E0B", # amber
    danger = "#DC2626",
    # -- Typography ---------------------------------------------------------
    base_font = bslib::font_google("Atkinson Hyperlegible"),
    code_font = bslib::font_google("Fira Code"),
    "font-size-base" = "1rem",
    "line-height-base" = "1.6",
    "headings-color" = "#172033",
    "link-color" = "#0d9488", # teal darkened ~8%
    "link-hover-color" = "#0f766e",
    "letter-spacing" = "-0.005em",
    # -- Navbar -------------------------------------------------------------
    "navbar-bg" = "#172033",
    "navbar-light-color" = "#F8FAFC",
    "navbar-light-active-color" = "#F8FAFC",
    "navbar-light-hover-color" = "#2DD4BF",
    "navbar-light-brand-color" = "#F8FAFC",
    "navbar-light-brand-hover-color" = "#2DD4BF",
    # -- Code ---------------------------------------------------------------
    "code-bg" = "#EEF2F7",
    "code-color" = "#2F3A4A"
  )
}

#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Direct Care Analytics | Raborn Decision Sciences"
    ),
    tags$link(rel = "stylesheet", href = "www/custom.css")
  )
}
