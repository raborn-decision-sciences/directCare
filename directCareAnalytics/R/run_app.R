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

  secured_ui <- shinymanager::secure_app(
    app_ui,
    theme = rds_theme(),
    # The default floating logout button is disabled here; app_server()
    # renders a Logout link in the navbar instead (see account_menu).
    fab_position = "none",
    # golem_add_external_resources() sets the browser-tab favicon, but it
    # only runs inside app_ui(), which shinymanager doesn't call until
    # after login -- without this, the login page falls back to a
    # default/generic tab icon instead of matching the app.
    head_auth = tags$link(rel = "icon", type = "image/svg+xml", href = "www/favicon.svg"),
    tags_top = tags$div(
      style = "text-align:center;",
      tags$img(src = "www/favicon.svg", height = "48px", alt = "RDS"),
      tags$h4(
        style = "margin-top:8px;font-weight:600;color:#172033;",
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
