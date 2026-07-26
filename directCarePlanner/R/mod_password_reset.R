#' Password-reset page -- full-page UI, chosen by run_app.R's `?reset=1` branch
#'
#' Same shape as signup_ui() -- a plain Shiny UI, not wrapped by
#' shinymanager::secure_app(). Whether this shows the "request a link" form
#' or the "set a new password" form is decided entirely server-side (see
#' mod_password_reset_server()), not here -- the token in the reset link's
#' query string can't be read reliably from `request$QUERY_STRING` (that
#' reflects the websocket upgrade request, not the original page URL, same
#' issue already documented for demo-mode detection in app_server.R), only
#' from `session$clientData$url_search` once the client connects.
#' @noRd
password_reset_ui <- function(request) {
  tagList(
    tags$head(
      tags$link(rel = "icon", type = "image/svg+xml", href = "www/favicon.svg"),
      tags$title("Reset Password | Direct Care Practice Launch Planner")
    ),
    page_fluid(
      theme = rds_theme(),
      tags$div(
        class = "d-flex align-items-center justify-content-center",
        style = "min-height: 100vh;",
        tags$div(
          style = "max-width: 440px; width: 100%; padding: 24px;",
          mod_password_reset_ui("reset")
        )
      )
    )
  )
}

#' Password-reset module -- UI
#' @noRd
mod_password_reset_ui <- function(id) {
  ns <- NS(id)
  tagList(
    .auth_page_chrome("Direct Care Practice Launch Planner", dark_mode_id = ns("dark_mode")),
    tags$div(style = "height:24px;"),
    uiOutput(ns("reset_panel")),
    tags$div(
      style = "text-align:center;margin-top:16px;",
      tags$a(href = "/", class = "small", "Back to login")
    )
  )
}

#' Extract a reset token from a URL search/query string
#'
#' Factored out of the reactive below as a pure function so it's directly
#' unit-testable -- shiny's `MockShinySession`'s `clientData$url_search`
#' is hardcoded to a fixed test value ("?mocksearch=1") with no way to
#' override it in `testServer()`, so the token-extraction *logic* needs
#' to live somewhere testable independent of a real session.
#'
#' @param url_search A URL search string (e.g. `"?reset=1&token=abc123"`).
#' @return The token as a character string, or `NULL` if absent/blank.
#' @noRd
.extract_reset_token <- function(url_search) {
  query <- parseQueryString(url_search %||% "")
  token <- query$token
  if (is.character(token) && length(token) == 1 && nzchar(token)) token else NULL
}

#' Password-reset module -- server
#'
#' Two sub-states driven by whether `?token=...` is present in the URL
#' (read reactively from `session$clientData$url_search`, matching
#' app_server.R's existing demo-mode-detection pattern):
#'
#' - No token: the "forgot your password?" request form. On submit, logs a
#'   `password_reset_requested` auth_events row and checks the rate limit
#'   *unconditionally* -- regardless of whether the email maps to a real
#'   account -- then always shows the identical generic "if an account
#'   exists..." message either way. This is the account-enumeration guard:
#'   the UI must never branch on whether `password_reset_request()` found
#'   a real account.
#' - Token present: the "set a new password" form, consumed via
#'   `directCareAuth::password_reset_consume()`.
#'
#' `signup_panel`/`reset_panel`-style renderUI swap between form and
#' success state, matching mod_signup.R's own convention.
#' @noRd
mod_password_reset_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$dark_mode, {
      is_dark <- identical(input$dark_mode, "dark")
      session$setCurrentTheme(if (is_dark) rds_theme_dark() else rds_theme())
    })

    token_r <- reactive(.extract_reset_token(session$clientData$url_search))

    reset_msg <- reactiveVal(NULL)
    reset_done <- reactiveVal(FALSE)

    output$reset_msg <- renderUI({
      req(reset_msg())
      reset_msg()
    })

    # -- No token: request a reset link -------------------------------------
    observeEvent(input$request_submit, {
      email <- trimws(input$email %||% "")
      if (!nzchar(email)) {
        reset_msg(tags$p(class = "text-danger small mb-0", "Enter your email address"))
        return()
      }

      con <- directCareAuth::db_connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      # Logged unconditionally -- both for the audit trail and so the rate
      # limit itself can't be used to probe which emails have accounts.
      directCareAuth::auth_event_log(con, event_type = "password_reset_requested", email = email)

      if (!directCareAuth::password_reset_is_rate_limited(con, email)) {
        result <- directCareAuth::password_reset_request(con, email)
        if (isTRUE(result$ok)) {
          reset_url <- paste0(Sys.getenv("APP_BASE_URL"), "/?reset=1&token=", result$token)
          tryCatch(
            directCareAuth::send_password_reset_email(email, result$practice_name, reset_url),
            # A send failure shouldn't change what the user sees (see
            # below) or leak whether the account exists -- there's no
            # different actionable next step for them regardless of cause.
            error = function(e) NULL
          )
        }
      }

      reset_msg(tags$p(
        class = "text-success small mb-0",
        bs_icon("check-circle"),
        " If an account exists for that email, we've sent a password reset link."
      ))
    })

    # -- Token present: set a new password ----------------------------------
    observeEvent(input$reset_submit, {
      new_password <- input$new_password %||% ""
      confirm_password <- input$confirm_password %||% ""

      if (new_password != confirm_password) {
        reset_msg(tags$p(class = "text-danger small mb-0", "Passwords do not match"))
        return()
      }

      con <- directCareAuth::db_connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      result <- directCareAuth::password_reset_consume(con, token_r(), new_password)

      if (isTRUE(result$ok)) {
        directCareAuth::auth_event_log(
          con, event_type = "password_reset_completed", practice_id = result$practice_id
        )
        reset_done(TRUE)
      } else {
        msg <- switch(result$reason,
          invalid_or_expired_token = "This reset link is invalid or has expired. Request a new one.",
          weak_password = "New password must be at least 10 characters",
          "Something went wrong -- please try again"
        )
        reset_msg(tags$p(class = "text-danger small mb-0", msg))
      }
    })

    output$reset_panel <- renderUI({
      if (isTRUE(reset_done())) {
        return(tagList(
          tags$p(
            class = "text-success",
            bs_icon("check-circle"), " Password updated! You can now log in."
          ),
          tags$a(href = "/", class = "btn btn-primary w-100", "Go to Login")
        ))
      }

      if (!is.null(token_r())) {
        return(tagList(
          tags$h6(class = "fw-bold", "Set a new password"),
          passwordInput(session$ns("new_password"), "New Password"),
          tags$p(class = "text-muted small mt-n2", "At least 10 characters."),
          passwordInput(session$ns("confirm_password"), "Confirm New Password"),
          uiOutput(session$ns("reset_msg")),
          actionButton(session$ns("reset_submit"), "Set New Password", class = "btn-primary w-100 mt-2")
        ))
      }

      tagList(
        tags$h6(class = "fw-bold", "Forgot your password?"),
        tags$p(class = "text-muted small", "Enter your email and we'll send you a link to reset it."),
        textInput(session$ns("email"), "Email"),
        uiOutput(session$ns("reset_msg")),
        actionButton(session$ns("request_submit"), "Send Reset Link", class = "btn-primary w-100 mt-2")
      )
    })
  })
}
