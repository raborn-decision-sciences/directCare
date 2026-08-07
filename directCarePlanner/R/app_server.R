#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @param res_auth The `reactiveValues` returned by
#'   `shinymanager::secure_server()`, holding the logged-in practice's
#'   `user_info` (practice_id, practice_name, email). `NULL` outside of
#'   `run_app()` (e.g. in tests), in which case the account menu renders
#'   without an email.
#' @import shiny
#' @noRd
app_server <- function(input, output, session, res_auth = NULL) {
  # -- Account menu: login email + logout, top-right of the navbar ----------
  # Shows the login email rather than practice_name -- just a display
  # preference (email is the unambiguous login identifier); practice_name
  # itself is fully live now (sourced from res_auth below, and kept in sync
  # by the Account Settings modal's write-back), not stale in the way it
  # would have been back when it was a separately-typed in-app field.
  #
  # id = ".shinymanager_logout" is not a namespacing choice -- it's the
  # exact input id shinymanager::secure_server() listens for internally
  # (see fab_button()'s usage inside shinymanager::secure_app()). Reusing it
  # here lets this link trigger the same logout logic as the default
  # floating button, which run_app() disables via fab_position = "none".
  output$account_menu <- renderUI({
    email <- res_auth$email
    tags$span(
      class = "d-flex align-items-center gap-2",
      if (!is.null(email) && nzchar(email)) {
        tags$span(class = "text-light small", email)
      },
      tags$a(
        href = "#",
        class = "nav-link action-button",
        title = "Account Settings",
        onclick = "Shiny.setInputValue('account_settings_click', Math.random(), {priority: 'event'}); return false;",
        bs_icon("gear", title = "Account Settings")
      ),
      tags$a(
        id = ".shinymanager_logout",
        href = "#",
        class = "nav-link action-button",
        title = "Logout",
        bs_icon("box-arrow-right", title = "Logout")
      )
    )
  })
  # Shiny suspends renderUI evaluation for outputs it judges "hidden" by
  # default (suspendWhenHidden = TRUE) -- the navbar's <li class="... nav-item
  # form-inline"> wrapper reads as zero-size to that heuristic, so the
  # reactive never even runs otherwise (confirmed empirically in DCA: no
  # value and no error ever recorded for the output client-side).
  outputOptions(output, "account_menu", suspendWhenHidden = FALSE)

  # -- Account Settings modal: profile edit + password change -----------------
  # res_auth is a plain reactiveValues() (shinymanager's secure_server()
  # returns one directly, not a read-only snapshot -- confirmed by reading
  # its source), so writing res_auth$practice_name/address back after a
  # successful profile save is safe and immediately visible to every other
  # reader of res_auth in this session, unlike the optional in-app Practice
  # Name field on Plan Inputs, which has no write-back at all.
  account_profile_msg <- reactiveVal(NULL)
  account_password_msg <- reactiveVal(NULL)

  output$account_profile_msg <- renderUI({
    req(account_profile_msg())
    account_profile_msg()
  })
  output$account_password_msg <- renderUI({
    req(account_password_msg())
    account_password_msg()
  })

  observeEvent(input$account_settings_click, {
    req(res_auth)
    account_profile_msg(NULL)
    account_password_msg(NULL)
    showModal(modalDialog(
      title = tagList(bs_icon("gear"), " Account Settings"),
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tags$h6(class = "fw-bold", "Profile"),
      htmltools::tagQuery(
        textInput("account_practice_name", "Practice Name", value = res_auth$practice_name)
      )$find("input")$addAttrs(autocomplete = "organization")$allTags(),
      htmltools::tagQuery(
        textInput("account_address", "Address", value = res_auth$address %||% "")
      )$find("input")$addAttrs(autocomplete = "street-address")$allTags(),
      # See directCareAnalytics's identical block for the full rationale --
      # same optional fields collected at signup, now editable afterward,
      # prefilled from res_auth (practice_find_by_email() already surfaces
      # them, see auth.R's user_info).
      accordion(
        id = "account_more_info",
        open = FALSE,
        class = "mb-2",
        accordion_panel(
          title = "More about your practice (optional)",
          icon = bs_icon("info-circle"),
          textInput("account_first_name", "First Name", value = res_auth$first_name %||% ""),
          textInput("account_last_name", "Last Name", value = res_auth$last_name %||% ""),
          selectInput(
            "account_practice_type", "Practice Type",
            choices = c(
              "Select one" = "",
              "Physician", "Nurse Practitioner", "Mental Health Therapist", "Other"
            ),
            selected = res_auth$practice_type %||% ""
          ),
          conditionalPanel(
            condition = "input.account_practice_type == 'Other'",
            textInput("account_practice_type_other", "Please specify", value = res_auth$practice_type_other %||% "")
          ),
          selectInput(
            "account_practice_status", "Practice Status",
            choices = c(
              "Select one" = "",
              "Just exploring",
              "Planning to launch a direct care practice",
              "Direct care practice launched within the last year",
              "Direct care practice launched more than a year ago"
            ),
            selected = res_auth$practice_status %||% ""
          ),
          textInput(
            "account_practice_specialty", "Practice Specialty",
            value = res_auth$practice_specialty %||% "",
            placeholder = "Primary Care, Pediatrics, Gynecology, etc."
          ),
          textInput(
            "account_referral_source", "How did you hear about us?",
            value = res_auth$referral_source %||% ""
          )
        )
      ),
      uiOutput("account_profile_msg"),
      actionButton(
        "account_save_profile", "Save Profile",
        class = "btn-primary btn-sm mt-2 mb-3"
      ),
      tags$hr(),
      tags$h6(class = "fw-bold", "Change Password"),
      # Hidden username field -- see directCareAnalytics's identical block
      # for the full WHATWG/MDN rationale (a password-change form with no
      # visible username field makes browsers guess wrong about which
      # nearby text input to pair with the password fields; confirmed
      # live, Address was getting autofilled with the login email).
      tags$input(
        type = "text", value = res_auth$email %||% "", autocomplete = "username",
        style = "display:none", `aria-hidden` = "true"
      ),
      htmltools::tagQuery(
        passwordInput("account_current_password", "Current Password")
      )$find("input")$addAttrs(autocomplete = "current-password")$allTags(),
      htmltools::tagQuery(
        passwordInput("account_new_password", "New Password")
      )$find("input")$addAttrs(autocomplete = "new-password")$allTags(),
      tags$p(class = "text-muted small mt-n2", "At least 10 characters."),
      htmltools::tagQuery(
        passwordInput("account_confirm_password", "Confirm New Password")
      )$find("input")$addAttrs(autocomplete = "new-password")$allTags(),
      uiOutput("account_password_msg"),
      actionButton(
        "account_change_password", "Change Password",
        class = "btn-primary btn-sm mt-2"
      ),
      tags$hr(),
      tags$h6(class = "fw-bold", "Billing"),
      if (!is.null(r$stripe_customer_id) && nzchar(r$stripe_customer_id)) {
        tagList(
          tags$p(
            class = "text-muted small mb-2",
            "Current plan: ", tags$strong(tools::toTitleCase(r$plan_tier %||% "free"))
          ),
          actionButton(
            "account_manage_billing", "Manage Billing",
            icon = bs_icon("credit-card"),
            class = "btn-outline-primary btn-sm"
          )
        )
      } else {
        tagList(
          tags$p(class = "text-muted small mb-2", "You're on the Free plan."),
          actionButton(
            "account_see_plans", "See plans",
            icon = bs_icon("arrow-up-right-circle"),
            class = "btn-outline-primary btn-sm"
          )
        )
      }
    ))
  })

  # -- Billing: Checkout/Portal entry points -----------------------------------
  # Plain top-level ids, not module-namespaced -- see .show_plans_modal()'s
  # own comment (utils_billing.R) for why that's correct regardless of
  # which module triggered the modal these buttons live in.
  observeEvent(input$btn_checkout_starter, {
    .start_stripe_checkout(session, r$practice_id, r$email, STRIPE_LOOKUP_KEY_STARTER)
  })
  observeEvent(input$btn_checkout_pro, {
    .start_stripe_checkout(session, r$practice_id, r$email, STRIPE_LOOKUP_KEY_PRO)
  })
  observeEvent(input$account_see_plans, {
    .show_plans_modal()
  })
  observeEvent(input$account_manage_billing, {
    .start_stripe_portal(
      session, r$stripe_customer_id,
      return_url = paste0(Sys.getenv("APP_BASE_URL", unset = ""), "/")
    )
  })

  observeEvent(input$account_save_profile, {
    practice_name <- trimws(input$account_practice_name %||% "")
    address <- trimws(input$account_address %||% "")

    if (!nzchar(practice_name)) {
      account_profile_msg(tags$p(
        class = "text-danger small mb-0",
        bs_icon("exclamation-circle"), " Practice Name is required"
      ))
      return()
    }

    first_name <- trimws(input$account_first_name %||% "")
    last_name <- trimws(input$account_last_name %||% "")
    practice_type <- input$account_practice_type %||% ""
    practice_type_other <- trimws(input$account_practice_type_other %||% "")
    practice_status <- input$account_practice_status %||% ""
    practice_specialty <- trimws(input$account_practice_specialty %||% "")
    referral_source <- trimws(input$account_referral_source %||% "")

    con <- directCareAuth::db_connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    directCareAuth::practice_update_profile(
      con, res_auth$practice_id, practice_name, address,
      first_name = first_name, last_name = last_name,
      practice_type = practice_type, practice_type_other = practice_type_other,
      practice_status = practice_status, practice_specialty = practice_specialty,
      referral_source = referral_source
    )
    directCareAuth::auth_event_log(
      con, event_type = "profile_update",
      practice_id = res_auth$practice_id, email = res_auth$email
    )

    res_auth$practice_name <- practice_name
    res_auth$address <- address
    res_auth$first_name <- first_name
    res_auth$last_name <- last_name
    res_auth$practice_type <- practice_type
    res_auth$practice_type_other <- practice_type_other
    res_auth$practice_status <- practice_status
    res_auth$practice_specialty <- practice_specialty
    res_auth$referral_source <- referral_source
    r$practice_name <- practice_name
    account_profile_msg(tags$p(
      class = "text-success small mb-0",
      bs_icon("check-circle"), " Profile updated"
    ))
  })

  observeEvent(input$account_change_password, {
    new_password <- input$account_new_password %||% ""
    confirm_password <- input$account_confirm_password %||% ""

    if (new_password != confirm_password) {
      account_password_msg(tags$p(class = "text-danger small mb-0", "New passwords do not match"))
      return()
    }

    con <- directCareAuth::db_connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    result <- directCareAuth::practice_update_password(
      con, res_auth$practice_id, input$account_current_password %||% "", new_password
    )

    if (isTRUE(result$ok)) {
      directCareAuth::auth_event_log(
        con, event_type = "password_change",
        practice_id = res_auth$practice_id, email = res_auth$email
      )
      updateTextInput(session, "account_current_password", value = "")
      updateTextInput(session, "account_new_password", value = "")
      updateTextInput(session, "account_confirm_password", value = "")
      account_password_msg(tags$p(
        class = "text-success small mb-0",
        bs_icon("check-circle"), " Password updated"
      ))
    } else {
      msg <- switch(result$reason,
        wrong_current_password = "Current password is incorrect",
        weak_password = "New password must be at least 10 characters",
        "Something went wrong -- please try again"
      )
      account_password_msg(tags$p(class = "text-danger small mb-0", msg))
    }
  })

  thematic::thematic_shiny(
    bg = "#F8FAFC",
    fg = "#172033",
    accent = "#14B8A6"
  )

  # -- Dark/light mode toggle -------------------------------------------------
  # bslib::input_dark_mode() flips the client-side data-bs-theme attribute
  # (driving custom.css's [data-bs-theme="dark"] overrides) and emits
  # "light"/"dark" strings via input$dark_mode -- not TRUE/FALSE, so this
  # checks with identical() rather than isTRUE().
  #
  # Deliberately NOT calling session$setCurrentTheme() here (it was here
  # originally, alongside a comment about a since-worked-around bslib
  # class-check failure) -- confirmed via a live MutationObserver capture
  # (in DCA, identical mechanism) that calling it makes bslib tear down and
  # re-fetch all six theme stylesheets (bootstrap.min.css, both Google
  # Fonts, bslib-component-css, selectize.css, shiny-sass.css) with a fresh
  # cache-busting `?restyle=` query string on *every single toggle* -- a
  # real network round-trip for the entire CSS bundle, not just the changed
  # variables, and the reported cause of the light/dark toggle's visible
  # flash. It's also redundant: the exact `[data-bs-theme="dark"]` variable
  # overrides rds_theme_dark() would push are already present in every page
  # load as a static `!important` <style> block (run_app.R's `head_auth`,
  # sourced from `.dark_mode_css_rules()`) -- the client's own instant
  # data-bs-theme attribute flip is enough to apply them with zero network
  # calls. Signup and password-reset pages (mod_signup.R/
  # mod_password_reset.R) are NOT wrapped by shinymanager and so don't get
  # head_auth's static injection -- they still need session$setCurrentTheme()
  # and keep it.
  observeEvent(input$dark_mode, {
    is_dark <- identical(input$dark_mode, "dark")
    # Plots still need re-theming: thematic_shiny() pins explicit colors
    # rather than "auto" (see comment on the initial call above), so a CSS
    # variable swap alone wouldn't reach them.
    thematic::thematic_shiny(
      bg = if (is_dark) "#0F172A" else "#F8FAFC",
      fg = if (is_dark) "#E2E8F0" else "#172033",
      accent = "#14B8A6"
    )
    r$dark_mode <- input$dark_mode
  })

  # Shared state, populated by mod_plan_inputs on a successful "Build My
  # Plan" submission and read by mod_results. No getter/setter
  # abstraction -- modules read/write fields directly, matching
  # directCareAnalytics's convention.
  r <- reactiveValues(
    practice_name = NULL, # sourced from res_auth below, not by mod_plan_inputs
    # Added for saved-scenario slots (plan_scenarios, keyed by practice_id
    # like every other table in this schema) -- previously this app had no
    # such field at all, since it never tags its own market_context/
    # revenue/projections rows with one the way DCA's data-ingest pipeline
    # does. Sourced from res_auth below, same as practice_name.
    practice_id = NULL,
    email = NULL, # sourced from res_auth below; prefills Stripe Checkout's email field
    plan_tier = "free", # "free" | "starter" | "pro" — written only by directCareBilling's webhook; sourced from res_auth below, same as practice_name
    stripe_customer_id = NULL, # written only by directCareBilling's webhook; NULL until the practice's first completed Checkout. Manage Billing (Account Settings) is hidden until this is set.
    horizon_months = NULL,
    market_context = NULL, # dcPlanR_market_context, from build_market_context()
    revenue = NULL, # dcPlanR_revenue, from calc_mixed_revenue()
    projections = NULL, # dcPlanR_scenario_projection tibble, from project_scenarios()
    capital = NULL, # list(startup_costs = , personal_runway = )
    interpretations = NULL, # list(revenue = , projection = , capital = ), plain text

    # "light" or "dark" -- referenced (unused) inside renderPlot() purely
    # to register a reactive dependency; see directCareAnalytics's
    # app_server.R for the full explanation of why this is needed.
    dark_mode = "light"
  )

  # -- Source practice identity from the logged-in account ---------------------
  # Single source of truth, replacing the old optional in-app Practice Name
  # field (mod_plan_inputs.R). An observe() (not a one-time assignment) is
  # required here, not optional: res_auth's fields don't exist yet at the
  # moment app_server() starts running -- shinymanager populates them
  # asynchronously, once its own internal token check resolves after login
  # -- so this needs a reactive dependency on res_auth$practice_name to
  # correctly re-fire once it actually appears. Matches
  # directCareAnalytics's identical pattern.
  observe({
    req(res_auth)
    r$practice_name <- res_auth$practice_name
    r$practice_id <- res_auth$practice_id
    # Falls back to "free" (matches practices.plan_tier's own DB default)
    # rather than NULL -- see directCareAnalytics's identical pattern for
    # the full rationale.
    r$plan_tier <- if (is.null(res_auth$plan_tier)) "free" else res_auth$plan_tier
    r$email <- res_auth$email
    r$stripe_customer_id <- res_auth$stripe_customer_id
  })

  plan_inputs_result <- mod_plan_inputs_server("plan_inputs", r, parent_session = session)
  results_result <- mod_results_server("results", r, parent_session = session)

  # Saved scenario slots (plan_scenarios) -- instantiated once here, at the
  # app level, rather than inside mod_plan_inputs.R, so the Save/Load
  # buttons and "viewing saved scenario" banner (rendered once, globally,
  # via app_ui.R's page_navbar(header = ...)) are available from both Plan
  # Inputs and Results, not just Plan Inputs. plan_inputs_result's
  # get_inputs_for_save/get_dirty_signal/on_load are plain closures returned
  # by mod_plan_inputs_server(); see that module's own comment for why they
  # still correctly read its own input$xxx/session objects from here.
  directCareScenarios::mod_scenario_slots_server(
    "scenario",
    table = "plan_scenarios",
    get_con = directCareAuth::db_connect,
    practice_id = function() r$practice_id,
    get_inputs_for_save = plan_inputs_result$get_inputs_for_save,
    get_dirty_signal = plan_inputs_result$get_dirty_signal,
    on_load = plan_inputs_result$on_load,
    has_access = function() .has_paid_plan(r$plan_tier),
    on_access_denied = .show_plans_modal
  )
  observeEvent(input$btn_see_plans_scenario, {
    .show_plans_modal()
  })

  # -- Consolidated sticky nav bar: one central output, not one per tab -----
  # Each tab module returns its Back/Next/Download buttons (already wrapped
  # in .tour_nav_footer(), including that tab's own Save/Load "extra") via a
  # `nav_footer` reactive instead of rendering them in its own content --
  # this switch just selects which one is currently active and is the only
  # place either ever actually gets inserted into the DOM (see app_ui.R's
  # header comment).
  output$main_nav_footer <- renderUI({
    switch(input$main_nav,
      "plan_inputs" = plan_inputs_result$nav_footer(),
      "results" = results_result$nav_footer(),
      NULL
    )
  })
  outputOptions(output, "main_nav_footer", suspendWhenHidden = FALSE)
}
