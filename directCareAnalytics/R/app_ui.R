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
        style = "display:inline-flex;align-items:center;gap:8px;color:inherit;text-decoration:none;line-height:1;",
        tags$img(
          src = "www/favicon.svg",
          height = "28px",
          width = "28px",
          alt = "RDS"
        ),
        tags$span(
          style = "font-weight:600;font-size:0.95rem;white-space:nowrap;",
          "Direct Care Analytics"
        )
      ),
      id = "main_nav",
      theme = rds_theme(),
      # -- Tabs (hidden from navbar; navigation via Next/Back buttons) ---------
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
#' @importFrom golem add_resource_path activate_js bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))

  tags$head(
    tags$link(rel = "icon", type = "image/svg+xml", href = "www/favicon.svg"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Direct Care Analytics | Raborn Decision Sciences"
    ),
    tags$link(rel = "stylesheet", href = "www/custom.css"),
    # Hide workflow tabs from the navbar — navigation is via Next/Back buttons.
    # JS runs after DOM construction and is not subject to :has() browser support.
    tags$script(HTML(
      "(function() {
        var vals = ['upload', 'edit', 'summary', 'projections'];
        function hideNavTabs() {
          vals.forEach(function(v) {
            var a = document.querySelector('.navbar-nav a[data-value=\"' + v + '\"]');
            if (a && a.parentElement) {
              a.parentElement.style.setProperty('display', 'none', 'important');
            }
          });
        }
        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', hideNavTabs);
        } else {
          hideNavTabs();
        }
      })();"
    ))
  )
}
