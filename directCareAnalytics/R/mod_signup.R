#' Signup page -- full-page UI, chosen by run_app.R's `?signup=1` branch
#'
#' A plain Shiny UI (not wrapped by shinymanager::secure_app(), unlike the
#' login page), so it uses the normal session$setCurrentTheme() dark-mode
#' mechanism directly rather than the login page's static-CSS-injection
#' workaround (see .dark_mode_css_rules()'s own roxygen comment for why that
#' workaround exists there and not here). golem::add_resource_path("www", ...)
#' is already registered by the time any request reaches here -- run_app()
#' calls it once, unconditionally, before building any of the UI branches.
#' @noRd
signup_ui <- function(request) {
  tagList(
    tags$head(
      tags$link(rel = "icon", type = "image/svg+xml", href = "www/favicon.svg"),
      tags$title("Sign Up | Direct Care Analytics")
    ),
    page_fluid(
      theme = rds_theme(),
      tags$div(
        class = "d-flex align-items-center justify-content-center",
        style = "min-height: 100vh;",
        tags$div(
          style = "max-width: 440px; width: 100%; padding: 24px;",
          mod_signup_ui("signup")
        )
      )
    )
  )
}

#' Signup form module -- UI
#' @noRd
mod_signup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    .auth_page_chrome("Direct Care Analytics", dark_mode_id = ns("dark_mode")),
    tags$div(style = "height:24px;"),
    uiOutput(ns("signup_panel")),
    tags$div(
      style = "text-align:center;margin-top:16px;",
      tags$a(href = "/", class = "small", "Back to login")
    )
  )
}

#' Signup form module -- server
#'
#' Fully open for the beta: no invite code, approval step, or email
#' verification -- matches the app's current trial-only, no-billing-check
#' posture. `directCareAuth::practice_create()` handles password hashing
#' and strength validation; `signup_panel` swaps from the form to a success
#' state via renderUI(), the same conditional-content pattern already used
#' by mod_upload.R's `main_content` output.
#' @noRd
mod_signup_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$dark_mode, {
      is_dark <- identical(input$dark_mode, "dark")
      session$setCurrentTheme(if (is_dark) rds_theme_dark() else rds_theme())
    })

    signup_msg <- reactiveVal(NULL)
    signup_done <- reactiveVal(FALSE)

    output$signup_msg <- renderUI({
      req(signup_msg())
      signup_msg()
    })

    observeEvent(input$submit, {
      practice_name <- trimws(input$practice_name %||% "")
      email <- trimws(input$email %||% "")
      password <- input$password %||% ""
      confirm_password <- input$confirm_password %||% ""

      if (!nzchar(practice_name) || !nzchar(email)) {
        signup_msg(tags$p(
          class = "text-danger small mb-0",
          bs_icon("exclamation-circle"), " Practice Name and Email are required"
        ))
        return()
      }
      if (password != confirm_password) {
        signup_msg(tags$p(class = "text-danger small mb-0", "Passwords do not match"))
        return()
      }

      con <- directCareAuth::db_connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      result <- directCareAuth::practice_create(con, practice_name, email, password)

      if (isTRUE(result$ok)) {
        directCareAuth::auth_event_log(
          con, event_type = "signup", practice_id = result$id, email = email
        )
        signup_done(TRUE)
      } else {
        msg <- switch(result$reason,
          email_taken = "An account with that email already exists",
          weak_password = "Password must be at least 10 characters",
          "Something went wrong -- please try again"
        )
        signup_msg(tags$p(class = "text-danger small mb-0", msg))
      }
    })

    output$signup_panel <- renderUI({
      if (isTRUE(signup_done())) {
        return(tagList(
          tags$p(
            class = "text-success",
            bs_icon("check-circle"), " Account created! You can now log in."
          ),
          tags$a(href = "/", class = "btn btn-primary w-100", "Go to Login")
        ))
      }

      tagList(
        textInput(session$ns("practice_name"), "Practice Name"),
        textInput(session$ns("email"), "Email"),
        passwordInput(session$ns("password"), "Password"),
        tags$p(class = "text-muted small mt-n2", "At least 10 characters."),
        passwordInput(session$ns("confirm_password"), "Confirm Password"),
        uiOutput(session$ns("signup_msg")),
        actionButton(session$ns("submit"), "Create Account", class = "btn-primary w-100 mt-2")
      )
    })
  })
}
