#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  # Resource path must be registered before secure_app() wraps the UI --
  # golem_add_external_resources() (which normally does this) only runs
  # inside app_ui(), which shinymanager doesn't call until after a
  # successful login, so the login page itself would 404 on www/ assets
  # (favicon, logo) otherwise.
  golem::add_resource_path("www", app_sys("app/www"))

  # The Username/Password fields and Login button are self-explanatory;
  # shinymanager's default "Please authenticate" heading above them is
  # redundant. set_labels() is shinymanager's own mechanism for overriding
  # its built-in UI strings (see ?shinymanager::set_labels) -- blanking the
  # string here leaves the <h3> element in place but empty, rather than
  # fighting shinymanager's internal id/CSS to remove the element itself.
  shinymanager::set_labels(language = "en", "Please authenticate" = "")

  secured_ui <- shinymanager::secure_app(
    app_ui,
    theme = rds_theme(),
    # The default floating logout button is disabled here; app_server()
    # renders a Logout link in the navbar instead (see account_menu).
    fab_position = "none",
    head_auth = tagList(
      # golem_add_external_resources() sets the browser-tab favicon, but it
      # only runs inside app_ui(), which shinymanager doesn't call until
      # after login -- without this, the login page falls back to a
      # default/generic tab icon instead of matching the app.
      tags$link(rel = "icon", type = "image/svg+xml", href = "www/favicon.svg"),
      # Same dark-mode CSS variable overrides as rds_theme_dark(), baked in
      # statically here rather than relying on session$setCurrentTheme()
      # -- see .dark_mode_css_rules()'s own roxygen comment for why.
      tags$style(HTML(.dark_mode_css_rules()))
    ),
    tags_top = tags$div(
      style = "text-align:center;position:relative;",
      # Reuses the same input id as app_ui()'s navbar toggle -- the two are
      # never in the DOM at once (shinymanager shows exactly one of the
      # login form / authenticated app per session), and app_server()'s
      # existing observeEvent(input$dark_mode, ...) already runs regardless
      # of auth state (it's the same Shiny session throughout; app_server()
      # starts executing immediately, well before any login completes), so
      # no extra server-side wiring is needed here -- picking a mode on the
      # login page carries straight through to the authenticated app.
      tags$div(
        style = "position:absolute;top:0;right:0;",
        input_dark_mode(id = "dark_mode")
      ),
      tags$img(src = "www/favicon.svg", height = "48px", alt = "RDS"),
      tags$h4(
        # var(--bs-emphasis-color), not a literal hex: the page background
        # (unlike the navbar) correctly follows the theme, so a hardcoded
        # light-navy color would go dark-on-dark once dark mode is picked.
        style = "margin-top:8px;font-weight:600;color:var(--bs-emphasis-color);",
        "Direct Care Analytics"
      )
    ),
    tags_bottom = tags$div(
      style = "text-align:center;margin-top:16px;",
      tags$a(
        href = "?demo=1",
        class = "btn btn-outline-primary btn-sm",
        tagList(bs_icon("play-circle"), " Try the demo")
      ),
      tags$div(
        style = "opacity:0.6;margin-top:16px;",
        tags$img(src = "www/logo-rds-alt.svg", height = "28px", alt = "Raborn Decision Sciences")
      )
    )
  )

  with_golem_options(
    app = shinyApp(
      # secure_app()'s return value is itself a function(request) (Shiny
      # calls any function `ui` per-session with the originating request) --
      # branching here, before delegating to it, lets `?demo=1` skip the
      # login gate entirely and go straight to app_ui(), rather than fighting
      # shinymanager's internal auth-token routing to add a bypass there.
      ui = function(request) {
        query <- parseQueryString(request$QUERY_STRING)
        if (isTRUE(query$demo == "1")) {
          app_ui(request)
        } else {
          secured_ui(request)
        }
      },
      # No demo-mode branch needed here: secure_server()'s observers stay
      # dormant for a demo session (its login-form inputs never exist
      # client-side, since the UI above skipped rendering them), so it's
      # safe to always wire it up the same way. app_server() detects demo
      # mode itself, server-side, from session$clientData$url_search --
      # session$request's QUERY_STRING reflects the websocket upgrade
      # request, not the original page URL, so it can't be read here.
      server = function(input, output, session) {
        res_auth <- shinymanager::secure_server(check_credentials = check_credentials_db)
        app_server(input, output, session, res_auth = res_auth)
      },
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
