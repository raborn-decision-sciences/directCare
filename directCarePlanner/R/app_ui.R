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
      # Renders the saved-scenario Save/Load buttons and the "viewing saved
      # scenario" banner exactly once, globally, instead of inside each
      # nav_panel's own content -- bslib keeps every nav_panel mounted in the
      # DOM simultaneously, so rendering the same bare "scenario"-id module
      # UI once per tab would produce duplicate DOM ids (confirmed in
      # directCareAnalytics, which hit this exact bug first; see its own
      # app_ui.R for the full story). A single global instance here is what
      # makes Save/Load available on both Plan Inputs and Results, not just
      # Plan Inputs.
      header = tagList(
        tags$div(
          class = "d-flex justify-content-end gap-2 px-3 py-2 border-bottom",
          directCareScenarios::mod_scenario_slots_ui("scenario")
        ),
        tags$div(
          class = "px-3 pt-2",
          directCareScenarios::mod_scenario_banner_ui("scenario")
        )
      ),
      nav_panel(
        title = tagList(bsicons::bs_icon("clipboard-data"), " Plan Inputs"),
        value = "plan_inputs",
        mod_plan_inputs_ui("plan_inputs")
      ),
      nav_panel(
        title = tagList(bsicons::bs_icon("graph-up-arrow"), " Results"),
        value = "results",
        mod_results_ui("results")
      ),
      nav_spacer(),
      # mode left unset (not "light"): bslib's <bslib-input-dark-mode>
      # already checks window.matchMedia("(prefers-color-scheme: dark)")
      # itself whenever no mode is explicitly set on the element, live
      # (it also listens for OS-level preference changes), falling back to
      # light when that signal is unavailable -- no custom JS needed.
      nav_item(input_dark_mode(id = "dark_mode")),
      nav_item(uiOutput("account_menu", inline = TRUE))
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

#' Raborn Decision Sciences bslib theme -- dark variant
#'
#' bs_theme()'s bg/fg args always compile into the `:root,
#' [data-bs-theme="light"]` bucket, regardless of how dark the colors
#' actually are -- Bootstrap 5.3 then auto-generates a `[data-bs-theme=
#' "dark"]` bucket from them via its own color-inversion heuristics, and
#' that auto-generated bucket wins once input_dark_mode() sets
#' data-bs-theme="dark" on <html> (confirmed empirically: passing dark
#' colors directly to bg/fg produced a washed-out near-white auto-inverted
#' background, not the dark navy requested). The fix is to start from the
#' normal light rds_theme() and layer explicit `[data-bs-theme="dark"]`
#' CSS variable overrides on top via bs_add_rules(), which compile after
#' -- and therefore win the cascade over -- Bootstrap's auto-generated
#' block. The navbar is deliberately left out of the overrides -- it's
#' already dark-navy in the light theme, so it reads correctly unchanged.
#' @noRd
rds_theme_dark <- function() {
  bslib::bs_add_rules(rds_theme(), .dark_mode_css_rules())
}

#' Shared `[data-bs-theme="dark"]` CSS variable overrides
#'
#' Factored out of `rds_theme_dark()` so the exact same rules can *also* be
#' injected as a static `<style>` on the shinymanager login page
#' (`run_app.R`'s `head_auth`) -- `session$setCurrentTheme()` (which is how
#' the authenticated app picks these up) doesn't reach the login page, since
#' shinymanager appears to serve its login UI's theme CSS as a page-level
#' static bundle rather than through bslib's per-session dynamic theme
#' mechanism. The login page's `input_dark_mode()` toggle still correctly
#' flips the client-side `data-bs-theme` attribute and Bootstrap's own
#' built-in dark-mode component styles (confirmed live: the toggle icon and
#' form-control colors update immediately) -- it was specifically these
#' *custom* RDS variable overrides that went missing there without this.
#' @noRd
.dark_mode_css_rules <- function() {
  # !important on every property: when this is injected as a static
  # <style> in the login page's head_auth, it lands *before*
  # bootstrap.min.css in the DOM (head_auth's content is written before
  # shinymanager adds its own theme <link> tags) -- confirmed live
  # (inspecting document.head.children) that without !important, Bootstrap
  # 5.3's own auto-generated [data-bs-theme="dark"] bucket (the same
  # color-inversion behavior documented on rds_theme_dark() above) loads
  # after and wins on equal specificity, silently reverting every one of
  # these back to Bootstrap's washed-out auto-inverted colors. Harmless for
  # the bs_add_rules()/rds_theme_dark() path (authenticated app), which
  # already wins the cascade on ordering alone.
  #
  # The `.panel-auth` rule is a separate fix for a separate bug: shinymanager
  # ships its own static styles-auth.css with a hardcoded
  # `.panel-auth { background-color: #fff; }` rule targeting the
  # position:fixed, viewport-covering div that wraps the entire login form.
  # None of the `--bs-*` variable overrides above touch it (it's not
  # controlled by any Bootstrap variable), so without this the login page's
  # text correctly re-colors for dark mode while the fixed white panel
  # behind it does not -- producing light-gray-on-white "washed out" text
  # (confirmed live via getComputedStyle + querying document.styleSheets for
  # the exact rule/selector). Irrelevant on the authenticated app (no
  # `.panel-auth` element exists there), so safe to include unconditionally.
  #
  # `.login-footer-logo` mirrors directCareAnalytics's identical fix -- see
  # its app_ui.R for the full rationale (custom.css's `.footer-logo` filter
  # never reaches the login page, since it's rendered outside app_ui() by
  # shinymanager's secure_app()).
  "[data-bs-theme=\"dark\"] {
      color-scheme: dark;
      --bs-body-bg: #0F172A !important;
      --bs-body-color: #E2E8F0 !important;
      --bs-emphasis-color: #E2E8F0 !important;
      --bs-heading-color: #E2E8F0 !important;
      --bs-secondary: #64748B !important;
      --bs-secondary-rgb: 100, 116, 139 !important;
      --bs-secondary-color: rgba(226, 232, 240, 0.75) !important;
      --bs-secondary-color-rgb: 226, 232, 240 !important;
      --bs-secondary-bg: #1E293B !important;
      --bs-tertiary-bg: #1E293B !important;
      --bs-border-color: #334155 !important;
      --bs-link-color: #2DD4BF !important;
      --bs-link-hover-color: #5EEAD4 !important;
      --bs-code-color: #CBD5E1 !important;
      --bs-code-bg: #1E293B !important;
    }
    [data-bs-theme=\"dark\"] .panel-auth {
      background-color: #0F172A !important;
    }
    [data-bs-theme=\"dark\"] .login-footer-logo {
      filter: brightness(0) invert(1);
    }"
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
    tags$link(rel = "stylesheet", href = "www/custom.css"),
    # Hide workflow tabs from the navbar — navigation is via the "Build My
    # Plan" submit button and the Back button on Results. Ported from
    # directCareAnalytics's app_ui.R.
    tags$script(HTML(
      "(function() {
        var vals = ['plan_inputs', 'results'];
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
