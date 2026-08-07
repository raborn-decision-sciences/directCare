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
      tags$title("Sign Up | Direct Care Practice Launch Planner"),
      # Not wrapped by app_ui.R (this page renders before login), so it
      # needs its own copy of the Checkout/Portal redirect handler -- see
      # .redirect_script()'s own comment (utils_globals.R). The post-signup
      # plan picker below (mod_signup_server's "choose_plan" stage) is the
      # thing that actually sends this message.
      .redirect_script()
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
    .auth_page_chrome("Direct Care Practice Launch Planner", dark_mode_id = ns("dark_mode")),
    tags$div(style = "height:24px;"),
    uiOutput(ns("signup_panel")),
    tags$div(
      style = "text-align:center;margin-top:16px;",
      tags$a(href = "/", class = "small", "Back to login")
    )
  )
}

#' Compact plan-option row for the post-signup plan picker
#'
#' See directCareAnalytics's identical helper for the full rationale.
#' @noRd
.signup_plan_option <- function(ns, btn_id, title, price_label, description, button_label, button_class) {
  tags$div(
    class = "border rounded p-3 mb-2",
    tags$div(
      class = "d-flex justify-content-between align-items-center mb-1",
      tags$strong(title),
      tags$span(class = "text-muted small", price_label)
    ),
    tags$p(class = "text-muted small mb-2", description),
    actionButton(ns(btn_id), button_label, class = paste("w-100 btn-sm", button_class))
  )
}

#' Collapsed-by-default "tell us more" accordion for the signup form
#'
#' See directCareAnalytics's identical helper for the full rationale.
#' @noRd
.signup_more_info_accordion <- function(ns) {
  accordion(
    id = ns("more_info"),
    open = FALSE,
    class = "mb-2",
    accordion_panel(
      title = "Tell us more about your practice (optional)",
      icon = bs_icon("info-circle"),
      textInput(ns("first_name"), "First Name"),
      textInput(ns("last_name"), "Last Name"),
      selectInput(
        ns("practice_type"), "Practice Type",
        choices = c(
          "Select one" = "",
          "Physician", "Nurse Practitioner", "Mental Health Therapist", "Other"
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Other'", ns("practice_type")),
        textInput(ns("practice_type_other"), "Please specify")
      ),
      selectInput(
        ns("practice_status"), "Practice Status",
        choices = c(
          "Select one" = "",
          "Just exploring",
          "Planning to launch a direct care practice",
          "Direct care practice launched within the last year",
          "Direct care practice launched more than a year ago"
        )
      ),
      textInput(
        ns("practice_specialty"), "Practice Specialty",
        placeholder = "Primary Care, Pediatrics, Gynecology, etc."
      ),
      textInput(ns("referral_source"), "How did you hear about us?")
    )
  )
}

#' Signup form module -- server
#'
#' Fully open for the beta: no invite code, approval step, or email
#' verification -- matches the app's current trial-only, no-billing-check
#' posture. `directCareAuth::practice_create()` handles password hashing
#' and strength validation; `signup_panel` swaps between the form, a
#' post-signup plan picker, and a final success state via renderUI() --
#' see directCareAnalytics's identical module for the full rationale on why
#' Checkout starts from the plan picker, not the form itself.
#' @noRd
mod_signup_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$dark_mode, {
      is_dark <- identical(input$dark_mode, "dark")
      session$setCurrentTheme(if (is_dark) rds_theme_dark() else rds_theme())
    })

    signup_msg <- reactiveVal(NULL)
    signup_stage <- reactiveVal("form")
    new_practice_id <- reactiveVal(NULL)
    new_practice_email <- reactiveVal(NULL)

    output$signup_msg <- renderUI({
      req(signup_msg())
      signup_msg()
    })

    observeEvent(input$plan_free, {
      signup_stage("done")
    })
    observeEvent(input$plan_starter, {
      .start_stripe_checkout(session, new_practice_id(), new_practice_email(), STRIPE_LOOKUP_KEY_STARTER)
    })
    observeEvent(input$plan_pro, {
      .start_stripe_checkout(session, new_practice_id(), new_practice_email(), STRIPE_LOOKUP_KEY_PRO)
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
      ip <- directCareAuth::extract_client_ip(session)

      # Logged unconditionally, before the rate-limit check, so repeated
      # attempts against the same email count toward the limit regardless
      # of whether they ultimately succeed (matches mod_password_reset.R's
      # signup_attempt/password_reset_requested logging convention).
      directCareAuth::auth_event_log(con, event_type = "signup_attempt", email = email, ip_address = ip)

      # Gated on either the email or the IP being over its own threshold --
      # same "either" logic as login lockout's auth_is_locked_out()/
      # auth_is_locked_out_by_ip() pair, so signup spam cycling through
      # different emails from one source can't dodge the email-only check.
      if (
        directCareAuth::signup_is_rate_limited(con, email) ||
          directCareAuth::signup_is_rate_limited_by_ip(con, ip)
      ) {
        signup_msg(tags$p(
          class = "text-danger small mb-0",
          "Too many signup attempts. Please try again later."
        ))
        return()
      }

      # All optional -- see .signup_more_info_accordion()'s own comment for
      # why none of these gate account creation the way practice_name/email
      # do above.
      result <- directCareAuth::practice_create(
        con, practice_name, email, password,
        first_name = trimws(input$first_name %||% ""),
        last_name = trimws(input$last_name %||% ""),
        practice_type = input$practice_type %||% "",
        practice_type_other = trimws(input$practice_type_other %||% ""),
        practice_status = input$practice_status %||% "",
        practice_specialty = trimws(input$practice_specialty %||% ""),
        referral_source = trimws(input$referral_source %||% "")
      )

      if (isTRUE(result$ok)) {
        directCareAuth::auth_event_log(
          con, event_type = "signup", practice_id = result$id, email = email
        )
        new_practice_id(result$id)
        new_practice_email(email)
        signup_stage("choose_plan")
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
      switch(signup_stage(),
        "choose_plan" = tagList(
          tags$p(
            class = "text-success text-center mb-1",
            bs_icon("check-circle"), " Account created!"
          ),
          tags$p(
            class = "text-muted small text-center mb-3",
            "Choose a plan to get started -- you can change this anytime."
          ),
          .signup_plan_option(
            session$ns, "plan_starter", "Starter", "$39/mo",
            "Bookkeeping uploads, saved practice profile, historical trends, downloadable reports.",
            "Choose Starter", "btn-primary"
          ),
          .signup_plan_option(
            session$ns, "plan_pro", "Pro", "$79/mo",
            "Everything in Starter, plus guided decision tools and AI-generated financial interpretation.",
            "Choose Pro", "btn-outline-primary"
          ),
          tags$div(
            style = "text-align:center;margin-top:8px;",
            actionButton(
              session$ns("plan_free"), "Continue with Free",
              class = "btn-link btn-sm text-muted"
            )
          )
        ),
        "done" = tagList(
          tags$p(
            class = "text-success",
            bs_icon("check-circle"), " You're all set! You can now log in."
          ),
          tags$a(href = "/", class = "btn btn-primary w-100", "Go to Login")
        ),
        tagList(
          textInput(session$ns("practice_name"), "Practice Name"),
          textInput(session$ns("email"), "Email"),
          passwordInput(session$ns("password"), "Password"),
          tags$p(class = "text-muted small mt-n2", "At least 10 characters."),
          passwordInput(session$ns("confirm_password"), "Confirm Password"),
          .signup_more_info_accordion(session$ns),
          uiOutput(session$ns("signup_msg")),
          actionButton(session$ns("submit"), "Create Account", class = "btn-primary w-100 mt-2")
        )
      )
    })
  })
}
