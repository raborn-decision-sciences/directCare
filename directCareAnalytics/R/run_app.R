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
  # Sanitizes uncaught R error messages shown in the browser (raw file
  # paths, package internals, query fragments) whenever GOLEM_CONFIG_ACTIVE
  # /R_CONFIG_ACTIVE is "production" (see inst/golem-config.yml and
  # docker-compose.yml, which sets this env var). Left off outside
  # production so local development still sees real error messages.
  options(shiny.sanitize.errors = get_golem_config("app_prod"))

  # Resource path must be registered before secure_app() wraps the UI --
  # golem_add_external_resources() (which normally does this) only runs
  # inside app_ui(), which shinymanager doesn't call until after a
  # successful login, so the login page itself would 404 on www/ assets
  # (favicon, logo) otherwise.
  golem::add_resource_path("www", app_sys("app/www"))

  # Serves the bundled demo GnuCash CSV as a plain static download
  # (demo-data/demo-gnucash.csv) for the Historical Data walkthrough's file-
  # upload step (see R/utils_tours.R) -- a static resource path, rather than
  # a downloadHandler, since the tour's step description is a plain HTML
  # string embedded directly in a cicerone popover, not a renderUI() output.
  golem::add_resource_path("demo-data", app_sys("extdata"))

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
    # Reuses the same input id as app_ui()'s navbar toggle -- the two are
    # never in the DOM at once (shinymanager shows exactly one of the login
    # form / authenticated app per session), and app_server()'s existing
    # observeEvent(input$dark_mode, ...) already runs regardless of auth
    # state (it's the same Shiny session throughout; app_server() starts
    # executing immediately, well before any login completes), so no extra
    # server-side wiring is needed here -- picking a mode on the login page
    # carries straight through to the authenticated app.
    tags_top = .auth_page_chrome("Direct Care Analytics"),
    tags_bottom = tags$div(
      style = "text-align:center;margin-top:16px;",
      tags$a(
        href = "?demo=1",
        class = "btn btn-outline-primary btn-sm",
        tagList(bs_icon("play-circle"), " Try the demo")
      ),
      tags$div(
        style = "margin-top:12px;",
        tags$a(href = "?signup=1", class = "small", "Don't have an account? Sign up")
      ),
      tags$div(
        style = "margin-top:4px;",
        tags$a(href = "?reset=1", class = "small", "Forgot password?")
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
        } else if (isTRUE(query$signup == "1")) {
          signup_ui(request)
        } else if (isTRUE(query$reset == "1")) {
          password_reset_ui(request)
        } else {
          secured_ui(request)
        }
      },
      # No demo-mode/signup-mode/reset-mode branch needed here:
      # secure_server()'s observers stay dormant for a demo, signup, or
      # reset session (its login-form inputs never exist client-side,
      # since the UI above skipped rendering them), so it's safe to always
      # wire it up the same way. mod_signup_server()'s and
      # mod_password_reset_server()'s own observers are likewise dormant
      # outside their respective sessions, for the same reason (their
      # inputs only exist in the DOM when their own UI was the chosen one).
      # app_server() detects demo mode itself, server-side, from
      # session$clientData$url_search -- session$request's QUERY_STRING
      # reflects the websocket upgrade request, not the original page URL,
      # so it can't be read here.
      server = function(input, output, session) {
        res_auth <- shinymanager::secure_server(check_credentials = check_credentials_db)
        app_server(input, output, session, res_auth = res_auth)
        mod_signup_server("signup")
        mod_password_reset_server("reset")
      },
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
