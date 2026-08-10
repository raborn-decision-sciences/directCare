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

  # Persistent background-process pool for mod_projections.R's Run Forecast
  # ExtendedTask (see its own comment for the full rationale) -- set up once
  # per app process, here, rather than lazily on first use, so the first
  # click of a session doesn't pay a ~1s worker-spawn cost on top of the
  # forecast itself. n = 2 is a deliberately modest pool: this app has no
  # CPU/memory limit set in docker-compose.yml, and a small practice-facing
  # tool has no need for more than a couple of concurrent background
  # forecasts at once.
  mirai::daemons(n = 2)

  # mod_projections.R's Generate Report ExtendedTask writes PDFs into this
  # dedicated subdirectory (.report_output_dir(), utils_globals.R) rather
  # than the tempdir root, so they're easy to sweep separately from
  # anything else using tempfile(). Per-session cleanup (onSessionEnded,
  # mod_projections.R) handles the common cases, but a mirai task isn't
  # tied to session lifecycle, so a session ending mid-generation orphans
  # its eventual output with nothing left to clean it up -- this recurring
  # sweep is the actual backstop for that case, not a redundant nicety.
  # 2 hours / 30-minute cadence are Phase-1 starting values, not tuned
  # against real usage. (.report_output_dir() creates the directory itself
  # if needed, so no separate dir.create() call here.)
  sweep_old_reports <- function() {
    old <- list.files(.report_output_dir(), full.names = TRUE)
    old <- old[file.info(old)$mtime < Sys.time() - 2 * 60 * 60]
    if (length(old) > 0L) unlink(old)
    later::later(sweep_old_reports, delay = 30 * 60)
  }
  later::later(sweep_old_reports, delay = 30 * 60)

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
        tags$img(
          src = "www/logo-rds-alt.svg", height = "28px",
          alt = "Raborn Decision Sciences", class = "login-footer-logo"
        )
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
        # Closure over `session` so check_credentials_db() can see the
        # client IP for lockout purposes (see IP_LOGIN_LOCKOUT.md) --
        # shinymanager calls this as a plain function(user, password), so
        # capturing session via lexical scoping here is the only way to
        # thread it through; check_credentials_db() itself has no session
        # access.
        res_auth <- shinymanager::secure_server(
          check_credentials = function(user, password) {
            check_credentials_db(user, password, ip = directCareAuth::extract_client_ip(session))
          }
        )
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
