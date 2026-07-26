#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @param res_auth The `reactiveValues` returned by
#'   `shinymanager::secure_server()`, holding the logged-in practice's
#'   `user_info` (practice_id, practice_name, email). `NULL` outside of
#'   `run_app()` (e.g. in tests), in which case the account menu renders
#'   without an email.
#' @import shiny
#' @importFrom thematic thematic_shiny
#' @noRd
app_server <- function(input, output, session, res_auth = NULL) {
  # -- Demo mode ----------------------------------------------------------
  # The login page's "Try the demo" link (run_app.R) sends `?demo=1`, which
  # the ui-level branch in run_app.R already used to skip straight to
  # app_ui() -- but that decision isn't visible here, since ui and server
  # are separate per-session closures with no direct channel between them.
  # session$clientData$url_search is the reliable way to re-read the query
  # string server-side (confirmed empirically: session$request$QUERY_STRING
  # reflects the websocket upgrade request, not the original page URL, and
  # reads empty). It isn't populated on the very first synchronous line of
  # server code, but resolves within the same initial reactive flush, so a
  # plain observe() sees the correct value on its first (only) run.
  demo_mode <- reactiveVal(FALSE)
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (isTRUE(query$demo == "1")) {
      demo_mode(TRUE)
    }
  })

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
    if (demo_mode()) {
      return(tags$span(
        class = "d-flex align-items-center gap-2",
        tags$span(
          class = "badge text-bg-warning",
          bs_icon("stars"),
          " Demo Mode"
        ),
        tags$a(
          href = "#",
          class = "nav-link",
          title = "Exit Demo",
          # A plain navigation (not an action button) back to the same
          # pathname without the `?demo=1` query string -- no server-side
          # session/login state to unwind, unlike the real logout link.
          onclick = "window.location.search=''; return false;",
          "Exit Demo"
        )
      ))
    }
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
  # reactive never even ran (confirmed via Shiny.shinyapp.$values/$errors:
  # neither a value nor an error was ever recorded for this output).
  outputOptions(output, "account_menu", suspendWhenHidden = FALSE)

  # -- Account Settings modal: profile edit + password change -----------------
  # res_auth is a plain reactiveValues() (shinymanager's secure_server()
  # returns one directly, not a read-only snapshot -- confirmed by reading
  # its source), so writing res_auth$practice_name/address back after a
  # successful profile save is safe and immediately visible to every other
  # reader of res_auth in this session, unlike the old in-app Practice Name
  # field this replaces (see mod_upload.R), which had no write-back at all.
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
      textInput("account_practice_name", "Practice Name", value = res_auth$practice_name),
      textInput("account_address", "Address", value = res_auth$address %||% ""),
      uiOutput("account_profile_msg"),
      actionButton(
        "account_save_profile", "Save Profile",
        class = "btn-primary btn-sm mt-2 mb-3"
      ),
      tags$hr(),
      tags$h6(class = "fw-bold", "Change Password"),
      passwordInput("account_current_password", "Current Password"),
      passwordInput("account_new_password", "New Password"),
      tags$p(class = "text-muted small mt-n2", "At least 10 characters."),
      passwordInput("account_confirm_password", "Confirm New Password"),
      uiOutput("account_password_msg"),
      actionButton(
        "account_change_password", "Change Password",
        class = "btn-primary btn-sm mt-2"
      )
    ))
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

    con <- directCareAuth::db_connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    directCareAuth::practice_update_profile(con, res_auth$practice_id, practice_name, address)
    directCareAuth::auth_event_log(
      con, event_type = "profile_update",
      practice_id = res_auth$practice_id, email = res_auth$email
    )

    res_auth$practice_name <- practice_name
    res_auth$address <- address
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

  # Pinned rather than "auto": auto-detection calls back into
  # shiny::getCurrentOutputInfo() to resolve bg/fg/accent from the
  # surrounding CSS. That resolution errored ("attempt to apply
  # non-function") for bg/fg on plotOutput()s inside full_screen bslib
  # cards (as ovhd_plot/inc_plot in mod_summary.R are). Pinning colors
  # avoids using any "auto" value, but thematic::auto_resolve_theme() still
  # unconditionally probes shiny::getCurrentOutputInfo() on every plot
  # render to see whether it *could* auto-resolve something — and warns
  # when that output context lacks CSS-reporting info, even though the
  # result goes unused since nothing here is actually "auto". Dropping
  # "shiny" from the auto-config priority list skips that probe entirely.
  thematic::auto_config_set(
    thematic::auto_config(priority = c("config", "bslib", "rstudio"))
  )
  thematic::thematic_shiny(
    bg = "#F8FAFC",
    fg = "#172033",
    accent = "#14B8A6"
  )

  # -- Dark/light mode toggle -------------------------------------------------
  # bslib::input_dark_mode() flips the client-side data-bs-theme attribute
  # (driving custom.css's [data-bs-theme="dark"] overrides) and emits
  # "light"/"dark" strings via input$dark_mode -- not TRUE/FALSE, so this
  # checks with identical() rather than isTRUE(). rds_theme_dark() is
  # constructed fresh here (a plain bs_theme() call, not a piped
  # bs_add_rules() chain), which avoids a known session$setCurrentTheme()
  # class-check failure some bslib versions have with piped-together theme
  # objects. Plots need re-theming too, since thematic_shiny() above pins
  # explicit colors rather than "auto" (see comment on that call) -- a CSS
  # variable swap alone wouldn't reach them.
  observeEvent(input$dark_mode, {
    is_dark <- identical(input$dark_mode, "dark")
    session$setCurrentTheme(if (is_dark) rds_theme_dark() else rds_theme())
    thematic::thematic_shiny(
      bg = if (is_dark) "#0F172A" else "#F8FAFC",
      fg = if (is_dark) "#E2E8F0" else "#172033",
      accent = "#14B8A6"
    )
    r$dark_mode <- input$dark_mode
  })

  # -- Shared reactive state --------------------------------------------------
  # All three modules read and write through `r`. Tab 1 populates the data;
  # Tab 2 may modify categories or append manual rows; Tab 3 consumes the
  # final result for forecasting and report generation.
  r <- reactiveValues(
    # Practice metadata -- both sourced from res_auth below, not user input
    practice_id = NULL, # character(DB serial id) \u2014 internal row-tagging value only, never shown/edited
    practice_name = NULL, # character \u2014 display label for reports
    panel_size = NULL, # numeric \u2014 current DPC panel size (members)
    membership_fee = NULL, # numeric \u2014 weighted avg monthly membership fee per member ($)
    membership_tiers = NULL, # list of {label, members, fee} \u2014 detail behind panel_size/membership_fee
    category_labels = NULL, # named chr vector: overhead category slug \u2192 custom display label
    source_labels = NULL, # named chr vector: income account_name \u2192 custom display label

    # Data pipeline state
    transactions = NULL, # normalized tibble from ingest_gnucash_csv()
    overhead = NULL, # filter_gnucash_overhead() output
    income = NULL, # normalize_gnucash_income() output
    overhead_monthly = NULL, # summarize_overhead_monthly() output
    income_monthly = NULL, # summarize_income_monthly() output
    scenario_inputs = NULL, # list of Quick Estimator form values

    # Validation flags surfaced from validate_overhead() / validate_income()
    validation = list(),

    # Incremented on every confirmed Start Over; modules observe this to
    # clear their own local reactive state (selected path, tier counts, etc).
    reset_signal = 0L,

    # "light" or "dark" -- referenced (unused) inside each module's
    # renderPlot() blocks purely to register a reactive dependency, since
    # none of those plots otherwise depend on anything that changes when
    # the theme toggles. Without it, Shiny never re-runs renderPlot() on
    # toggle and the old, wrong-theme PNG just stays displayed (confirmed
    # empirically: the observeEvent() below re-configures thematic_shiny()
    # correctly, but that alone does nothing for a plot whose reactive
    # dependencies haven't changed).
    dark_mode = "light"
  )

  # -- Source practice identity from the logged-in account ---------------------
  # Single source of truth, replacing the old in-app Practice Name/ID
  # re-entry card (mod_upload.R) -- no persisted user-facing "ID" survives;
  # r$practice_id is now just the DB's own serial practices.id, used
  # internally to tag ingested rows (see mod_upload.R's ingest_*() calls),
  # never shown or editable. An observe() (not a one-time assignment) is
  # required here, not optional: res_auth's fields don't exist yet at the
  # moment app_server() starts running -- shinymanager populates them
  # asynchronously, once its own internal token check resolves after
  # login -- so this needs a reactive dependency on res_auth$practice_name/
  # practice_id to correctly re-fire once they actually appear. Demo
  # sessions never populate res_auth (no real login occurs), so this
  # observer harmlessly no-ops for them and .load_demo_data() below is the
  # only thing that ever sets r$practice_name/practice_id in that case.
  observe({
    req(res_auth)
    r$practice_name <- res_auth$practice_name
    r$practice_id <- if (is.null(res_auth$practice_id)) {
      NULL
    } else {
      as.character(res_auth$practice_id)
    }
  })

  # Fires once, only for demo sessions. Populates `r` the same way a real
  # GnuCash CSV upload would (.load_demo_data(), utils_demo.R), then lands
  # on the Edit tab, matching mod_upload.R's own post-upload destination --
  # so a demo session looks exactly like "I just uploaded a CSV," not a
  # special-cased shortcut.
  #
  # Deliberately not ignoreInit = TRUE: the observe() above (which sets
  # demo_mode(TRUE)) runs in the same initial flush as this observer's own
  # first evaluation, and does so first -- so for a demo session, this
  # observer's "initial" run already sees demo_mode() == TRUE.
  # ignoreInit's "skip the handler on this observer's first run" behavior
  # would silently discard exactly that run, and demo_mode() never changes
  # again afterward to trigger a second one (confirmed empirically: with
  # ignoreInit = TRUE this handler never fired at all, even though
  # demo_mode() was correctly TRUE by then). req() below does the "only
  # run when true" gating instead, safely, on every run regardless of
  # ordering.
  observeEvent(demo_mode(), {
    req(demo_mode())
    .load_demo_data(r)
    session$onFlushed(function() {
      updateNavbarPage(session, "main_nav", selected = "edit")
    }, once = TRUE)
  })

  # -- Brand-click: return to Upload tab (navigation only, no data reset) -------
  observeEvent(
    input$brand_click,
    {
      updateNavbarPage(session, inputId = "main_nav", selected = "upload")
    },
    ignoreInit = TRUE
  )

  # -- Global Start Over: available from every page via the navbar icon --------
  observeEvent(
    input$global_start_over_click,
    {
      showModal(modalDialog(
        title = tagList(bs_icon("exclamation-triangle-fill"), " Start Over?"),
        p(
          "This will clear all loaded data and return you to the workflow selection screen."
        ),
        p(
          tags$strong("Your practice name will be kept,"),
          " but all uploaded or generated financial data will be removed."
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            "global_confirm_start_over",
            "Yes, start over",
            class = "btn-warning"
          )
        ),
        easyClose = TRUE
      ))
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$global_confirm_start_over,
    {
      removeModal()
      r$panel_size <- NULL
      r$membership_fee <- NULL
      r$membership_tiers <- NULL
      r$category_labels <- NULL
      r$source_labels <- NULL
      r$transactions <- NULL
      r$overhead <- NULL
      r$income <- NULL
      r$overhead_monthly <- NULL
      r$income_monthly <- NULL
      r$scenario_inputs <- NULL
      r$validation <- list()
      r$reset_signal <- r$reset_signal + 1L
      updateNavbarPage(session, inputId = "main_nav", selected = "upload")
    },
    ignoreInit = TRUE
  )

  # -- Guided walkthroughs (cicerone) -------------------------------------------
  # Three tours (R/utils_tours.R), one per Upload-tab workflow card, launched
  # from links added to the help modal's workflow cards below. Each tour is
  # built from several small "chapter" guides rather than one long one --
  # see R/utils_tours.R's own top-of-file comment for why: cicerone/
  # driver.js resolves every step's element at `$init()` time and silently
  # *drops and compacts* any step whose element doesn't exist yet, so a
  # single long guide's hardcoded step numbers break the moment an earlier
  # step's element gets removed from the DOM (e.g. the Quick Estimator form
  # replaced by "Scenario Ready" after Generate is clicked). Scoping each
  # chapter to one DOM-stable stretch and always starting a fresh chapter at
  # its own step 1 avoids that entirely.
  #
  # `active_tour` tracks which tour (if any) is currently running, so these
  # observers -- which fire on ordinary app usage, not just during a tour --
  # only touch a guide when its tour is actually in progress, and route
  # shared chapters (Projections, reached by both the Historical Data and
  # Plan My Practice tours) to the right one.
  #
  # A `later::later()` delay (not `session$onFlushed()`) gates every chapter
  # switch: onFlushed only fires once, after the *current* reactive flush
  # (e.g. the tab switch itself), but a tab's content here is a uiOutput()
  # that Shiny suspends while hidden -- it only starts evaluating once the
  # client reports the tab as visible, which lands in a *second*, later
  # flush. onFlushed(once = TRUE) fired before that second flush completed,
  # so the next chapter's `$init()` saw a DOM that still didn't have its
  # target element (confirmed empirically: console logged "Element to
  # highlight #edit-est_rent not found" immediately after clicking "Start
  # Planning"). A 3-second delay clears that reliably; shorter delays (0.5s,
  # 1s, 2s) worked for most transitions but silently failed reaching Projections
  # specifically (its bslib::layout_sidebar() sidebar takes noticeably
  # longer to finish its own JS-driven column-width layout pass than Edit's
  # plain Quick Estimator form does) -- driver.js's own `canHighlight()`
  # check (bundled driver.min.js) skips highlighting a still-zero-size
  # element with *no* console warning at all, unlike the "not found"/"Invalid
  # element" cases, which is what made that specific failure mode hard to
  # tell apart from a logic bug. A fixed delay is blunter than tracking the
  # exact flush properly, but is standard practice for this class of Shiny
  # timing problem, and is imperceptible for a guided-tour transition.
  active_tour <- reactiveVal(NULL) # NULL | "historical" | "plan" | "calculator"
  # Tracks whichever chapter guide is currently on-screen, purely so it can
  # be $reset() right before the next chapter takes over -- each chapter is
  # a *separate* Cicerone `id` (a separate JS Driver instance under the
  # hood), and nothing ever tears down the previous instance's overlay/
  # popover DOM automatically just because a new one starts. Left alone,
  # the old overlay (sized and positioned for whatever page it was on) can
  # linger behind the new chapter's, which is exactly the kind of stale
  # full-page dimming that made the Projections hand-off look broken.
  current_chapter <- reactiveVal(NULL)

  guide_h1 <- .build_tour_h1()
  guide_h2 <- .build_tour_h2()
  guide_h3 <- .build_tour_h3()
  guide_h4 <- .build_tour_h4()
  guide_h5 <- .build_tour_h5()
  guide_p1 <- .build_tour_p1()
  guide_p2 <- .build_tour_p2()
  guide_p3 <- .build_tour_p3()
  guide_c1 <- .build_tour_c1()
  guide_c2 <- .build_tour_c2()
  guide_proj1 <- .build_tour_proj1() # shared: Projections, before Run
  guide_proj2 <- .build_tour_proj2() # shared: Projections, after Run

  # Reset whichever chapter was previously on-screen (if any), then init +
  # start the new one at its own step 1.
  .tour_switch <- function(guide) {
    # isolate() is required here: .tour_switch() is called from inside
    # later::later() callbacks (via .tour_advance() and the prefill
    # observers below), which run outside any reactive context -- reading
    # a reactiveVal there without isolate() throws "Operation not allowed
    # without an active reactive context" and crashes the whole R session
    # (confirmed the hard way: the app process died mid-tour with exactly
    # that error, which is what made the last leg of the Plan My Practice
    # tour look like a UI/timing bug rather than a crash).
    prev <- isolate(current_chapter())
    if (!is.null(prev)) {
      prev$reset(session)
    }
    guide$init(session)
    guide$start(step = 1, session = session)
    current_chapter(guide)
  }

  # Switch to the named tour's next chapter, but only if `tour_name` is the
  # one currently running, and only after a short delay (see comment above)
  # so the chapter's elements have actually rendered.
  .tour_advance <- function(tour_name, guide) {
    if (!identical(active_tour(), tour_name)) {
      return(invisible())
    }
    later::later(function() {
      .tour_switch(guide)
    }, delay = 3)
  }

  observeEvent(input$launch_tour_historical, {
    removeModal()
    active_tour("historical")
    .tour_switch(guide_h1)
  })
  observeEvent(input$launch_tour_plan, {
    removeModal()
    active_tour("plan")
    .tour_switch(guide_p1)
  })
  observeEvent(input$launch_tour_calculator, {
    removeModal()
    active_tour("calculator")
    .tour_switch(guide_c1)
  })

  # -- Historical Data: h1 (Upload cards) -> h2 (upload form) -> h3 (mapping
  # results) -> h4 (Review & Edit) -> h5 (Summary) -> shared Projections tail.
  observeEvent(
    input[["upload-btn_use_real"]],
    .tour_advance("historical", guide_h2),
    ignoreInit = TRUE
  )
  # Both of h2's steps (csv_file, btn_upload) coexist in the same static
  # upload form, so this is a plain in-chapter move_forward() -- no re-init
  # (and no compaction risk) needed between two steps that never stop
  # existing.
  observeEvent(
    input[["upload-csv_file"]],
    {
      if (identical(active_tour(), "historical")) {
        guide_h2$move_forward(session = session)
      }
    },
    ignoreInit = TRUE
  )
  observeEvent(
    input[["upload-btn_upload"]],
    .tour_advance("historical", guide_h3),
    ignoreInit = TRUE
  )
  observeEvent(
    input[["upload-btn_next_to_edit"]],
    .tour_advance("historical", guide_h4),
    ignoreInit = TRUE
  )
  observeEvent(
    input[["edit-btn_next_to_summary"]],
    .tour_advance("historical", guide_h5),
    ignoreInit = TRUE
  )
  observeEvent(
    input[["summary-btn_next_to_projections"]],
    .tour_advance("historical", guide_proj1),
    ignoreInit = TRUE
  )

  # -- Plan My Practice: p1 (Upload cards) -> p2 (Quick Estimator, pre-
  # filled with demo-derived example values -- see R/utils_tours.R) -> p3
  # ("Scenario Ready") -> shared Projections tail.
  #
  # The pre-fill is folded into the same later::later() delay .tour_advance()
  # uses elsewhere, so it happens once the form exists and right before the
  # tour switches to the chapter pointing at it.
  observeEvent(
    input[["upload-btn_use_plan"]],
    {
      if (identical(active_tour(), "plan")) {
        later::later(function() {
          updateNumericInput(session, "edit-est_rent", value = .tour_demo_overhead$rent)
          updateNumericInput(session, "edit-est_payroll", value = .tour_demo_overhead$payroll)
          updateNumericInput(session, "edit-est_ehr", value = .tour_demo_overhead$ehr)
          updateNumericInput(session, "edit-est_malpractice", value = .tour_demo_overhead$malpractice)
          updateNumericInput(session, "edit-est_supplies", value = .tour_demo_overhead$supplies)
          updateNumericInput(session, "edit-est_other_overhead", value = .tour_demo_overhead$other)
          updateTextInput(session, "edit-est_tier_label_1", value = .tour_demo_tier$label)
          updateNumericInput(session, "edit-est_tier_members_1", value = .tour_demo_tier$members)
          updateNumericInput(session, "edit-est_tier_fee_1", value = .tour_demo_tier$fee)
          updateNumericInput(session, "edit-est_tier_growth_1", value = .tour_demo_tier$growth)
          updateNumericInput(session, "edit-est_new_visit_fee", value = .tour_demo_ffs$new_visit_fee)
          updateNumericInput(session, "edit-est_new_patients_mo", value = .tour_demo_ffs$new_patients_mo)
          updateNumericInput(session, "edit-est_followup_fee", value = .tour_demo_ffs$followup_fee)
          updateNumericInput(session, "edit-est_followups_mo", value = .tour_demo_ffs$followups_mo)
          updateNumericInput(session, "edit-est_other_income", value = .tour_demo_ffs$other_income)
          .tour_switch(guide_p2)
        }, delay = 3)
      }
    },
    ignoreInit = TRUE
  )
  observeEvent(
    input[["edit-btn_generate"]],
    .tour_advance("plan", guide_p3),
    ignoreInit = TRUE
  )
  observeEvent(
    input[["edit-btn_go_projections"]],
    .tour_advance("plan", guide_proj1),
    ignoreInit = TRUE
  )

  # -- Quick Calculator: c1 (Upload cards) -> c2 (calculator form,
  # pre-filled). Same pre-fill pattern as Plan My Practice, one path-
  # selection away rather than a tab switch; c2 is the tour's only other
  # chapter since nothing gets removed from the DOM mid-flow here.
  observeEvent(
    input[["upload-btn_use_calculator"]],
    {
      if (identical(active_tour(), "calculator")) {
        later::later(function() {
          updateNumericInput(
            session,
            "upload-calculator-monthly_overhead",
            value = .tour_demo_calc_overhead
          )
          updateTextInput(
            session,
            "upload-calculator-tier_label_1",
            value = .tour_demo_tier$label
          )
          updateNumericInput(
            session,
            "upload-calculator-tier_members_1",
            value = .tour_demo_tier$members
          )
          updateNumericInput(
            session,
            "upload-calculator-tier_fee_1",
            value = .tour_demo_tier$fee
          )
          .tour_switch(guide_c2)
        }, delay = 3)
      }
    },
    ignoreInit = TRUE
  )

  # -- Shared tail: both Historical Data and Plan My Practice reach
  # Projections (guide_proj1, wired above) and finish the same way here.
  observeEvent(
    input[["projections-btn_run"]],
    {
      .tour_advance("historical", guide_proj2)
      .tour_advance("plan", guide_proj2)
    },
    ignoreInit = TRUE
  )

  # -- Help modal ---------------------------------------------------------------
  observeEvent(
    input$help_click,
    {
      showModal(modalDialog(
        title = tagList(
          bsicons::bs_icon("question-circle"),
          " Direct Care Analytics \u2014 Help"
        ),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),

        # Workflow overview cards
        tags$h6(class = "fw-bold mt-2", "Choose a workflow"),
        tags$p(
          class = "small text-muted mb-3",
          "Enter your practice name and ID at the top of the page, then pick",
          " the workflow that fits your situation."
        ),
        tags$div(
          class = "row g-3 mb-3",

          # -- Workflow 1: Historical data --------------------------------------
          tags$div(
            class = "col-md-4",
            tags$div(
              class = "card h-100 border-primary",
              tags$div(
                class = "card-body",
                tags$h6(
                  class = "card-title",
                  bsicons::bs_icon("file-earmark-bar-graph"),
                  " Historical Data"
                ),
                tags$p(
                  class = "card-text small mb-2",
                  "For practices with existing financial records. Load your data",
                  " one of two ways:"
                ),
                tags$ul(
                  class = "small ps-3 mb-0",
                  tags$li(
                    tags$strong("CSV upload \u2014"),
                    " export transactions from GnuCash",
                    " (File \u2192 Export \u2192 Export Transactions to CSV).",
                    " The app maps accounts to expense categories automatically."
                  ),
                  tags$li(
                    tags$strong("Manual entry \u2014"),
                    " type in aggregate overhead and income totals",
                    " period by period. Good for spreadsheet-based records."
                  )
                ),
                tags$p(
                  class = "card-text small mt-2 mb-0 text-muted",
                  bsicons::bs_icon("arrow-right-short"),
                  " Review & Edit \u2192 Summary \u2192 Projections"
                ),
                actionButton(
                  "launch_tour_historical",
                  tagList(bsicons::bs_icon("compass"), " Take the guided tour"),
                  class = "btn-outline-primary btn-sm w-100 mt-2"
                )
              )
            )
          ),

          # -- Workflow 2: Plan My Practice ------------------------------------
          tags$div(
            class = "col-md-4",
            tags$div(
              class = "card h-100 border-info",
              tags$div(
                class = "card-body",
                tags$h6(
                  class = "card-title",
                  bsicons::bs_icon("sliders"),
                  " Plan My Practice"
                ),
                tags$p(
                  class = "card-text small",
                  "For practices in the planning stage or without accounting",
                  " software. Enter estimated monthly overhead, membership tiers",
                  " (with optional per-tier growth rates), and optional",
                  " fee-for-service income. The app builds a synthetic financial",
                  " history and runs forecasts from the end of that period."
                ),
                tags$p(
                  class = "card-text small mt-2 mb-0 text-muted",
                  bsicons::bs_icon("arrow-right-short"),
                  " Review Scenario \u2192 Projections"
                ),
                actionButton(
                  "launch_tour_plan",
                  tagList(bsicons::bs_icon("compass"), " Take the guided tour"),
                  class = "btn-outline-info btn-sm w-100 mt-2"
                )
              )
            )
          ),

          # -- Workflow 3: Quick Calculator ------------------------------------
          tags$div(
            class = "col-md-4",
            tags$div(
              class = "card h-100 border-secondary",
              tags$div(
                class = "card-body",
                tags$h6(
                  class = "card-title",
                  bsicons::bs_icon("calculator"),
                  " Quick Calculator"
                ),
                tags$p(
                  class = "card-text small",
                  "Instant break-even and income-target math from your current",
                  " overhead and membership panel. Supports multiple membership",
                  " tiers. No historical data or forecast model \u2014 results",
                  " update as you type."
                ),
                tags$p(
                  class = "card-text small mt-2 mb-0 text-muted",
                  bsicons::bs_icon("arrow-right-short"),
                  " Results shown immediately, no navigation required"
                ),
                actionButton(
                  "launch_tour_calculator",
                  tagList(bsicons::bs_icon("compass"), " Take the guided tour"),
                  class = "btn-outline-secondary btn-sm w-100 mt-2"
                )
              )
            )
          )
        ),

        tags$hr(),

        # Workflow walkthroughs
        tags$h6(class = "fw-bold", "Step-by-step walkthroughs"),

        # Historical data walkthrough
        tags$p(
          class = "small fw-semibold mb-1",
          bsicons::bs_icon("file-earmark-bar-graph"),
          " Historical Data"
        ),
        tags$ol(
          class = "small mb-3",
          tags$li(
            tags$strong("Upload tab \u2014"),
            " enter your practice name and ID, then either upload a GnuCash CSV",
            " or click \u201cEnter Data Manually\u201d to type in period totals."
          ),
          tags$li(
            tags$strong("Review & Edit \u2014"),
            " for CSV uploads, verify that transactions are mapped to the correct",
            " expense categories and remove any erroneous rows. For manual entry,",
            " this step is skipped."
          ),
          tags$li(
            tags$strong("Summary \u2014"),
            " review overhead and revenue trends by period, with a category",
            " and income-source breakdown for CSV uploads."
          ),
          tags$li(
            tags$strong("Projections \u2014"),
            " choose a forecast method (Linear, ETS, or ARIMA), set the horizon",
            " and confidence level, and optionally add planned overhead events.",
            " Enter your membership tiers to unlock member-count metrics.",
            " Download the PDF report when ready."
          )
        ),

        # Plan My Practice walkthrough
        tags$p(
          class = "small fw-semibold mb-1",
          bsicons::bs_icon("sliders"),
          " Plan My Practice"
        ),
        tags$ol(
          class = "small mb-3",
          tags$li(
            tags$strong("Upload tab \u2014"),
            " enter your practice name and ID, then click \u201cPlan My Practice\u201d."
          ),
          tags$li(
            tags$strong("Quick Estimator \u2014"),
            " fill in monthly overhead by category, define one or more membership",
            " tiers (label, starting members, fee, and growth per month), and",
            " optionally add fee-for-service income.",
            " Set the synthetic history length and click \u201cGenerate Practice Scenario\u201d."
          ),
          tags$li(
            tags$strong("Review Scenario \u2014"),
            " inspect the generated summary, then click \u201cGo to Projections\u201d.",
            " Use \u201cRevise Estimates\u201d to regenerate with different inputs."
          ),
          tags$li(
            tags$strong("Projections \u2014"),
            " same as the Historical Data workflow. Membership tiers are",
            " pre-filled from the end of your synthetic period."
          )
        ),

        # Quick Calculator walkthrough
        tags$p(
          class = "small fw-semibold mb-1",
          bsicons::bs_icon("calculator"),
          " Quick Calculator"
        ),
        tags$ol(
          class = "small mb-3",
          tags$li(
            tags$strong("Upload tab \u2014"),
            " enter your practice name and ID, then click \u201cQuick Calculator\u201d."
          ),
          tags$li(
            tags$strong("Calculator \u2014"),
            " enter total monthly overhead, add membership tiers, and set an",
            " income target. Results (break-even and target scenarios) update",
            " instantly as you type."
          )
        ),

        tags$hr(),

        tags$p(
          class = "small text-muted mb-0",
          bsicons::bs_icon("envelope"),
          " Questions or feedback? Contact ",
          tags$a(
            href = "mailto:anthony@raborndecisionsciences.com",
            "anthony@raborndecisionsciences.com."
          )
        )
      ))
    },
    ignoreInit = TRUE
  )

  # -- Module wiring ----------------------------------------------------------
  mod_upload_server("upload", r, parent_session = session)
  mod_edit_server("edit", r, parent_session = session)
  mod_summary_server("summary", r, parent_session = session)
  mod_projections_server("projections", r, parent_session = session)
}
