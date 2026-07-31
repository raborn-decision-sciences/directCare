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
      # header (not per-tab content): renders exactly once, outside every
      # nav_panel(), so it's the correct place for the single, consolidated
      # sticky nav bar (Back/Next/Submit + Save/Load, see app_server.R's
      # output$main_nav_footer) to live. Rendering per-tab footer markup
      # inside each of Upload/Edit/Summary/Projections' own content instead
      # (the original design) produced *duplicate* DOM ids -- this app
      # forces several tabs' outputs to suspendWhenHidden = FALSE, so bslib
      # keeps every nav_panel's content mounted in the DOM at once, not just
      # the active one, and Shiny's client-side output binding only ever
      # updates the first matching id -- confirmed live, the "viewing saved
      # scenario" banner silently stayed empty on every tab but one. A
      # single central output switching on input$main_nav (and, for Upload,
      # its internal path_chosen()) avoids that: every tab module still owns
      # its own Back/Next buttons and observeEvent()s, it just returns them
      # via a `nav_footer` reactive instead of rendering them in place, and
      # only the currently-active tab's version is ever actually inserted
      # into the DOM.
      header = tagList(
        uiOutput("main_nav_footer"),
        tags$div(
          class = "px-3 pt-2",
          uiOutput("main_scenario_banner")
        )
      ),
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
          bs_icon("arrow-counterclockwise", title = "Start Over"),
          " Start Over",
          href = "#",
          title = "Start Over",
          class = "nav-link fw-semibold",
          style = "color: #FBBF24 !important;",
          onclick = "Shiny.setInputValue('global_start_over_click', Math.random(), {priority: 'event'}); return false;"
        )
      ),
      nav_item(
        tags$a(
          bs_icon("question-circle", title = "Help"),
          href = "#",
          title = "Help",
          class = "nav-link",
          onclick = "Shiny.setInputValue('help_click', Math.random(), {priority: 'event'}); return false;"
        )
      ),
      # Hidden in demo mode itself (via server-side renderUI) -- the
      # account menu already shows a Demo Mode badge + Exit Demo there, so
      # a second "try the demo" link would be redundant.
      nav_item(uiOutput("demo_nav_item", inline = TRUE)),
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
  bs_add_rules(rds_theme(), .dark_mode_css_rules())
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
  # `.login-footer-logo` mirrors custom.css's `.footer-logo` rule (the
  # authenticated app's Upload-tab footer logo, item E in the archive) --
  # same dark navy SVG, same brightness(0)+invert(1) fix, needed again here
  # because the login page never loads custom.css at all (it's rendered by
  # shinymanager's secure_app(), outside app_ui()) -- without this the
  # login page's own footer logo (run_app.R's tags_bottom) stayed its
  # original dark color and was hard to see against the dark background,
  # while the authenticated app's matching logo was already fixed.
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
      app_title = "Direct Care Analytics | Raborn Decision Sciences"
    ),
    # Cache-busted via a content hash query string -- without this, a
    # returning visitor's browser can keep serving a pre-deploy custom.css
    # from its own disk cache indefinitely (confirmed live: an already-open
    # browser profile kept showing pre-deploy nav-bar CSS even after a hard
    # refresh, while a fresh private window picked up the new file
    # immediately). The URL path itself never changes, so nothing else
    # about how it's served needs to change.
    tags$link(
      rel = "stylesheet",
      href = paste0(
        "www/custom.css?v=",
        substr(tools::md5sum(app_sys("app/www/custom.css")), 1, 10)
      )
    ),
    # Loads driver.js (cicerone's underlying JS/CSS) for the guided-tour
    # walkthroughs launched from the help modal -- see R/utils_tours.R.
    cicerone::use_cicerone(),
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
    )),
    # Real-readiness signal for guided-tour chapter transitions (see
    # `.tour_wait_and_switch()` in app_server.R): replaces a fixed
    # later::later(delay = N) guess with an actual poll for the next
    # chapter's first target element having nonzero dimensions -- the same
    # test driver.js's own canHighlight() uses internally to silently skip
    # a still-hidden/zero-size element. Registered on shiny:connected since
    # this script tag lives in <head>, potentially before shiny.js itself
    # has finished loading; safe either way since the actual custom message
    # is only ever sent later, in response to a user-driven tour action.
    tags$script(HTML(
      "$(document).on('shiny:connected', function() {
        // Welcome tour prompt (see app_server.R's tour_prompt_checked
        // observer): 'seen' is tracked here, not server-side, since it's a
        // soft one-time nudge rather than durable account state. Marking
        // it seen the moment we decide to show it (not on dismiss) means
        // closing the modal any way -- Escape, outside click, or either
        // button -- all correctly count as 'seen', with no extra
        // round-trip needed.
        Shiny.addCustomMessageHandler('checkTourPromptSeen', function(msg) {
          var seen = false;
          try {
            seen = localStorage.getItem('dca_tour_prompt_seen') === '1';
          } catch (e) {}
          if (!seen) {
            try {
              localStorage.setItem('dca_tour_prompt_seen', '1');
            } catch (e) {}
            Shiny.setInputValue('show_tour_prompt', Math.random(), {priority: 'event'});
          }
        });
        Shiny.addCustomMessageHandler('tourWaitForElement', function(msg) {
          var attempts = 0;
          var maxAttempts = 100; // 15s safety cap (100 x 150ms) -- a
                                  // bounded last resort, not the normal path
          var iv = setInterval(function() {
            attempts++;
            var el = document.getElementById(msg.id);
            var ready = false;
            if (el) {
              var rect = el.getBoundingClientRect();
              ready = rect.width > 0 && rect.height > 0;
            }
            if (ready || attempts >= maxAttempts) {
              clearInterval(iv);
              Shiny.setInputValue(
                'tour_element_ready',
                {id: msg.id, token: msg.token, ok: ready},
                {priority: 'event'}
              );
            }
          }, 150);
        });
      });"
    )),
    # Scroll-driven pin for the consolidated nav bar (see custom.css's
    # `.tour-nav-footer` comment for why this replaced a plain CSS
    # `position: sticky`): toggles `.tour-nav-pinned` (position: fixed)
    # once the real navbar has scrolled out of view, inserting a
    # same-height placeholder so content doesn't jump. Re-synced after
    # every re-render of output$main_nav_footer (app_server.R), since
    # Shiny replaces that uiOutput's entire contents -- and any pinned
    # class/placeholder tied to the old element -- on every tab switch.
    tags$script(HTML(
      "(function() {
        var PIN_CLASS = 'tour-nav-pinned';
        var placeholder = null;

        function pin(footer) {
          if (footer.classList.contains(PIN_CLASS)) return;
          placeholder = document.createElement('div');
          placeholder.style.height = footer.getBoundingClientRect().height + 'px';
          placeholder.className = 'tour-nav-footer-placeholder';
          footer.parentNode.insertBefore(placeholder, footer);
          footer.classList.add(PIN_CLASS);
        }

        function unpin(footer) {
          if (placeholder) {
            placeholder.remove();
            placeholder = null;
          }
          if (footer) footer.classList.remove(PIN_CLASS);
        }

        function sync() {
          var footer = document.querySelector('.tour-nav-footer');
          if (!footer) return;
          var navbar = document.querySelector('.navbar');
          var navbarBottom = navbar ? navbar.getBoundingClientRect().bottom : 0;
          if (navbarBottom <= 0) {
            pin(footer);
          } else {
            unpin(footer);
          }
        }

        window.addEventListener('scroll', sync, { passive: true });
        window.addEventListener('resize', sync);

        $(document).on('shiny:value', function(e) {
          if (e.name === 'main_nav_footer') {
            placeholder = null;
            setTimeout(sync, 0);
          }
        });

        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', sync);
        } else {
          sync();
        }
      })();"
    ))
  )
}
