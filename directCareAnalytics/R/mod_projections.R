#' Projections Module UI
#'
#' Tab 3 -- Sub-tabs for Break-even, Revenue Forecast, and Income Target.
#' Shared options sidebar controls method, horizon, and confidence level.
#' Each sub-tab shows value boxes, a forecast plot, interpretation text,
#' and a download button for the branded report.
#'
#' @param id Module namespace ID.
#' @noRd
mod_projections_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("content"))
}


#' Projections Module Server
#'
#' @param id Module namespace ID.
#' @param r Shared `reactiveValues` object from `app_server`.
#' @noRd
mod_projections_server <- function(id, r, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Triggered from the break-even/income-target goal-seek and scenario-
    # comparison upsell notes' "See Pro plan" links (utils_billing.R's
    # .pro_upsell_note()) -- same modal every other "See plans" trigger in
    # the app opens.
    observeEvent(input$btn_see_plans_pro_goal_seek, {
      .show_plans_modal()
    })
    observeEvent(input$btn_see_plans_pro_target_goal_seek, {
      .show_plans_modal()
    })
    observeEvent(input$btn_see_plans_pro_scenario_compare, {
      .show_plans_modal()
    })
    observeEvent(input$btn_see_plans_pro_stress_test, {
      .show_plans_modal()
    })

    # -- Saved scenario slots (dca_forecast_scenarios) -------------------------
    # Raw per-row field lists (not overhead_events_df()/fee_events_df()
    # below, which drop tier_idx/new_fee once reduced to a computed
    # revenue_delta) -- these are what let a loaded scenario's Planned
    # Overhead Events / Planned Fee Changes UI actually re-render with the
    # same rows, not just feed the already-baked-in adjusted results.
    overhead_events_raw <- reactive({
      n <- n_overhead_events()
      if (n == 0L) {
        return(list())
      }
      lapply(seq_len(n), function(i) {
        list(
          label = input[[paste0("ov_event_label_", i)]],
          start_date = input[[paste0("ov_event_start_", i)]],
          cost = input[[paste0("ov_event_cost_", i)]]
        )
      })
    })

    fee_events_raw <- reactive({
      n <- n_fee_events()
      if (n == 0L) {
        return(list())
      }
      lapply(seq_len(n), function(i) {
        list(
          tier_idx = input[[paste0("fee_event_tier_", i)]],
          start_date = input[[paste0("fee_event_start_", i)]],
          new_fee = input[[paste0("fee_event_new_fee_", i)]]
        )
      })
    })

    # Deliberately excludes overhead_custom/target_income (both
    # conditionally rendered -- only exist once overhead_model == "custom"
    # / forecast_type == "target") from the dirty-check signal: comparing a
    # currently-NULL conditional field against a loaded snapshot's real
    # value would make the "viewing saved scenario" banner disappear
    # immediately after loading, before the user has touched anything, any
    # time the loaded scenario's mode differs from whatever's currently
    # selected. Both fields are still saved and restored in full via
    # get_bundle_for_save()/on_load() below -- only the auto-clear-on-edit
    # comparison skips them.
    scenario_dirty_signal <- function() {
      list(
        method = input$method,
        horizon = input$horizon,
        confidence = input$confidence,
        income_growth = input$income_growth,
        overhead_model = input$overhead_model,
        overhead_growth = input$overhead_growth,
        membership_tiers = proj_tiers_list(),
        overhead_events = overhead_events_raw(),
        fee_events = fee_events_raw()
      )
    }

    # -- Saved scenario slots: callbacks only ----------------------------------
    # mod_scenario_slots_server() itself is now instantiated once at the
    # app level (app_server.R), not per-tab, so Save/Load/the "viewing
    # saved scenario" banner are a single shared instance available from
    # every DCA tab's footer instead of a Projections-only widget -- see
    # app_server.R's own comment for the full rationale. This module just
    # exposes the same callback bodies as before, as plain closures, for
    # app_server.R to pass into that single top-level call. Returning
    # closures rather than the reactives they read directly is deliberate:
    # each closure still correctly reads *this* module's own input$xxx/
    # session/reactiveVal objects wherever it's ultimately invoked from,
    # since R closures capture their defining environment, not their call
    # site -- standard Shiny module-return-value behavior, no different
    # from any other module that returns a reactive for a caller elsewhere
    # to consume.
    get_bundle_for_save <- function() {
      list(
        transactions = r$transactions,
        overhead_monthly = r$overhead_monthly,
        income_monthly = r$income_monthly,
        inputs = list(
          method = input$method,
          horizon = input$horizon,
          confidence = input$confidence,
          income_growth = input$income_growth,
          overhead_model = input$overhead_model,
          overhead_growth = input$overhead_growth,
          overhead_custom = input$overhead_custom,
          target_income = input$target_income,
          membership_tiers = proj_tiers_list(),
          overhead_events = overhead_events_raw(),
          fee_events = fee_events_raw()
        ),
        results = list(
          breakeven = .safe_result(breakeven_result()),
          revenue = .safe_result(revenue_result()),
          target = .safe_result(target_result()),
          adj_breakeven = .safe_result(adj_breakeven()),
          adj_revenue = .safe_result(adj_revenue()),
          adj_target = .safe_result(adj_target())
        )
      )
    }

    scenario_extract_dirty_signal <- function(bundle) {
      inputs <- bundle$inputs
      list(
        method = inputs$method, horizon = inputs$horizon,
        confidence = inputs$confidence, income_growth = inputs$income_growth,
        overhead_model = inputs$overhead_model, overhead_growth = inputs$overhead_growth,
        membership_tiers = inputs$membership_tiers,
        overhead_events = inputs$overhead_events, fee_events = inputs$fee_events
      )
    }

    scenario_on_load <- function(bundle) {
      # Restores the shared upload/edit/summary data too, not just this
      # tab's own forecast settings below -- correct (if previously
      # unnoticed) gap in the original Projections-only design: it never
      # needed this, since by the time a user reached Projections in the
      # same session, r$transactions/overhead_monthly/income_monthly were
      # already populated from their own upload earlier in that same
      # session, and Load only ever needed to restore *settings* on top of
      # data that was already live. Now that Load is reachable from a
      # fresh session on any tab (Upload/Edit/Summary), without this the
      # banner would claim a scenario was loaded while every other tab
      # still showed "Upload a CSV first" -- confirmed live.
      r$transactions <- bundle$transactions
      r$overhead_monthly <- bundle$overhead_monthly
      r$income_monthly <- bundle$income_monthly

      inputs <- bundle$inputs
      updateSelectInput(session, "method", selected = inputs$method)
      updateSliderInput(session, "horizon", value = inputs$horizon)
      updateSliderInput(session, "confidence", value = inputs$confidence)
      updateSliderInput(session, "income_growth", value = inputs$income_growth)
      updateSelectInput(session, "overhead_model", selected = inputs$overhead_model)
      updateSliderInput(session, "overhead_growth", value = inputs$overhead_growth)

      proj_n_tiers(max(1L, length(inputs$membership_tiers)))
      n_overhead_events(length(inputs$overhead_events))
      n_fee_events(length(inputs$fee_events))

      # Two-phase: the tier/event field counts just set above only take
      # effect once their renderUI()s re-render with that many rows --
      # applying per-row values has to wait for that, or updateXInput()
      # would target ids that don't exist in the DOM yet. Unlike the
      # tour's own cross-*tab* transitions, this is a same-tab renderUI()
      # refresh, which reliably completes within one flush, so a plain
      # onFlushed(once = TRUE) (no polling/delay) is sufficient here.
      session$onFlushed(function() {
        if (!is.null(inputs$overhead_custom)) {
          updateNumericInput(session, "overhead_custom", value = inputs$overhead_custom)
        }
        if (!is.null(inputs$target_income)) {
          updateNumericInput(session, "target_income", value = inputs$target_income)
        }
        tiers <- inputs$membership_tiers
        for (i in seq_along(tiers)) {
          updateTextInput(session, paste0("proj_tier_label_", i), value = tiers[[i]]$label)
          updateNumericInput(session, paste0("proj_tier_members_", i), value = tiers[[i]]$members)
          updateNumericInput(session, paste0("proj_tier_fee_", i), value = tiers[[i]]$fee)
        }
        ov_events <- inputs$overhead_events
        for (i in seq_along(ov_events)) {
          updateTextInput(session, paste0("ov_event_label_", i), value = ov_events[[i]]$label)
          updateSelectInput(session, paste0("ov_event_start_", i), selected = ov_events[[i]]$start_date)
          updateNumericInput(session, paste0("ov_event_cost_", i), value = ov_events[[i]]$cost)
        }
        fee_events <- inputs$fee_events
        for (i in seq_along(fee_events)) {
          updateSelectInput(session, paste0("fee_event_tier_", i), selected = as.character(fee_events[[i]]$tier_idx))
          updateSelectInput(session, paste0("fee_event_start_", i), selected = fee_events[[i]]$start_date)
          updateNumericInput(session, paste0("fee_event_new_fee_", i), value = fee_events[[i]]$new_fee)
        }
      }, once = TRUE)
    }

    # -- Gate: require uploaded data ------------------------------------------
    output$content <- renderUI({
      has_data <- !is.null(r$overhead_monthly) && nrow(r$overhead_monthly) > 0
      if (!has_data) {
        return(
          card(
            card_body(
              class = "text-center text-muted py-5",
              bs_icon("arrow-left-circle", size = "2em"),
              p(
                "Upload a CSV or add period summaries in the Review & Edit tab to generate projections."
              )
            )
          )
        )
      }

      page_sidebar(
        sidebar = sidebar(
          title = "Forecast Options",
          # -- Shared controls --------------------------------------------
          tags$div(
            class = "d-flex align-items-center gap-1 mb-1",
            tags$span(class = "fw-semibold small", "Forecast Method"),
            tooltip(
              bs_icon("circle-fill", title = "Color legend", size = "0.7em"),
              HTML(paste0(
                "<strong>Data sufficiency colors:</strong><br>",
                "<span style='color:#16A34A;'>&#9679;</span> Green &mdash; sufficient data for accurate use<br>",
                "<span style='color:#F59E0B;'>&#9679;</span> Yellow &mdash; usable but accuracy may be lower<br>",
                "<span style='color:#DC2626;'>&#9679;</span> Red &mdash; insufficient data; consider another method"
              ))
            ),
            tooltip(
              bs_icon("info-circle", title = "Model descriptions"),
              HTML(paste0(
                "<strong>Linear regression</strong> &mdash; fits a straight-line trend. ",
                "Works with any amount of data. Best for stable, predictable growth.<br><br>",
                "<strong>ETS (exponential smoothing)</strong> &mdash; weights recent periods more ",
                "heavily so the model adapts as your practice evolves. Good with 12+ periods.<br><br>",
                "<strong>ARIMA</strong> &mdash; captures patterns in how periods relate to prior ones. ",
                "Needs 30+ periods for reliable results. Can over-fit with sparse data."
              ))
            )
          ),
          uiOutput(ns("method_selector")),
          uiOutput(ns("method_hint")),
          sliderInput(
            ns("horizon"),
            if (is_weekly()) {
              "Forecast horizon (weeks)"
            } else {
              "Forecast horizon (months)"
            },
            min = 4,
            max = if (is_weekly()) 84 else 36,
            value = if (is_weekly()) 52 else 12,
            step = 4
          ),
          sliderInput(
            ns("confidence"),
            "Confidence level",
            min = 0.80,
            max = 0.99,
            value = 0.95,
            step = 0.01
          ),
          hr(),
          # -- Growth-rate / overhead-model assumptions ------------------
          tags$p(
            class = "small fw-semibold mb-0",
            bs_icon("graph-up-arrow"),
            " Assumptions"
          ),
          tags$p(
            class = "small text-muted",
            "Applied on top of the fitted model. Defaults leave the historical trend unchanged."
          ),
          # Wrapped in an explicit id for the same reason overhead_model_wrap
          # is below: sliderInput()'s own inputId lands on a native <input>
          # that ionRangeSlider (the JS widget Shiny's slider uses) hides
          # and replaces with a sibling .irs structure -- the same
          # hidden-input gotcha as selectize, just for sliders instead of
          # selects. The tour step (utils_tours.R, tour_proj1) previously
          # targeted ns("income_growth") directly, which highlighted a
          # zero-size element up at the page's origin instead of the
          # visible slider -- confirmed live: it rendered in the upper
          # left, well above the actual "Income growth" control.
          tags$div(
            id = ns("income_growth_wrap"),
            sliderInput(
              ns("income_growth"),
              "Income growth (%/yr)",
              min = -60,
              max = 60,
              value = 0,
              step = 0.5
            )
          ),
          # Wrapped in an explicit id: selectInput()'s own inputId lands on
          # the native <select>, which selectize.js hides (display: none)
          # and replaces with a sibling .selectize-control div -- the same
          # hidden-select gotcha already hit (and fixed) for
          # edit-remap_account. Tour steps target this wrapper, not
          # ns("overhead_model") itself.
          tags$div(
            id = ns("overhead_model_wrap"),
            selectInput(
              ns("overhead_model"),
              "Overhead projection",
              choices = list(
                "Statistical" = c(
                  "Fitted trend (+ growth %)" = "trend"
                ),
                "Flat -- data-driven" = c(
                  "Historical average" = "avg_full",
                  "Recent average (last 6 mo)" = "avg_recent",
                  "Historical maximum (stress test)" = "max_full",
                  "Recent maximum (last 6 mo)" = "max_recent"
                ),
                "Flat -- manual" = c(
                  "Custom monthly value" = "custom"
                )
              ),
              selected = "trend"
            )
          ),
          uiOutput(ns("overhead_model_controls")),
          hr(),
          # -- Practice profile (optional) ------------------------------
          tags$p(
            class = "small fw-semibold mb-0",
            bs_icon("people"),
            " Membership Profile"
          ),
          tags$p(
            class = "small text-muted",
            "Enter your membership tiers to unlock member-count projections.",
            " For scenario workflows these are pre-filled from end-of-period counts.",
            " Leave blank to skip."
          ),
          uiOutput(ns("proj_tier_ui")),
          actionButton(
            ns("proj_btn_add_tier"),
            tagList(bs_icon("plus-circle"), " Add Tier"),
            class = "btn-outline-secondary btn-sm w-100 mb-1"
          ),
          hr(),
          # -- Planned future overhead events ---------------------------
          tags$p(
            class = "small fw-semibold mb-0",
            bs_icon("calendar-plus"),
            " Planned Overhead Events",
            tooltip(
              bs_icon("info-circle", title = "About overhead events"),
              paste0(
                "Add a planned cost increase (e.g. hiring staff, new office) ",
                "that starts at a specific point in the forecast horizon. ",
                "Each event adds a permanent monthly cost from that period forward."
              )
            )
          ),
          uiOutput(ns("overhead_events_ui")),
          actionButton(
            ns("btn_add_overhead_event"),
            tagList(bs_icon("plus-circle"), " Add Event"),
            class = "btn-outline-secondary btn-sm w-100 mb-2"
          ),
          hr(),
          # -- Planned future fee changes ----------------------------------
          tags$p(
            class = "small fw-semibold mb-0",
            bs_icon("calendar-plus"),
            " Planned Fee Changes",
            tooltip(
              bs_icon("info-circle", title = "About fee change events"),
              paste0(
                "Add a planned membership fee change for one of your tiers ",
                "that starts at a specific point in the forecast horizon. ",
                "The revenue forecast is stepped up (or down) by the ",
                "resulting change in that tier's monthly revenue from that ",
                "period forward."
              )
            )
          ),
          uiOutput(ns("fee_events_ui")),
          actionButton(
            ns("btn_add_fee_event"),
            tagList(bs_icon("plus-circle"), " Add Fee Change"),
            class = "btn-outline-secondary btn-sm w-100 mb-2"
          ),
          hr(),
          # -- Target-specific control -----------------------------------
          uiOutput(ns("target_input_ui")),
          # -- Run button ------------------------------------------------
          input_task_button(
            ns("btn_run"),
            "Run Forecast",
            icon = bsicons::bs_icon("play-fill"),
            class = "btn-primary w-100"
          ),
          uiOutput(ns("download_ui"))
          # Back navigation moves to the shared sticky footer below (see
          # .tour_nav_footer()) -- kept out of the sidebar to avoid showing
          # two Back controls at once.
        ),

        # -- Main area: forecast sub-tabs -----------------------------------
        # No mod_scenario_banner_ui()/mod_scenario_slots_ui() call here --
        # both are rendered exactly once, globally, in app_ui.R. bslib keeps
        # every nav_panel's content mounted in the DOM simultaneously (this
        # app forces several tabs' outputs to suspendWhenHidden = FALSE),
        # so rendering the same bare "scenario"-id module UI once per tab
        # produced *duplicate* DOM ids -- confirmed live: 4 elements all
        # sharing id="scenario-banner", of which only the first ever
        # received Shiny's renderUI updates, leaving the banner silently
        # empty on every tab except one. A single global instance (see
        # app_ui.R) is the correct fix, not a per-tab one.
        navset_card_underline(
          id = ns("forecast_type"),
          nav_panel(
            "Break-even",
            value = "breakeven",
            uiOutput(ns("breakeven_ui"))
          ),
          nav_panel(
            "Revenue Forecast",
            value = "revenue",
            uiOutput(ns("revenue_ui"))
          ),
          nav_panel(
            "Income Target",
            value = "target",
            uiOutput(ns("target_ui"))
          )
        )
      )
    })
    outputOptions(output, "content", suspendWhenHidden = FALSE)

    # Returned for the central output$main_nav_footer (app_server.R) to
    # render when Projections is the active tab -- see app_ui.R's header
    # comment. Last step in the sequence, so no forward button.
    nav_footer <- reactive({
      .tour_nav_footer(
        current_step = 4L,
        back = actionButton(
          ns("btn_back_to_summary"),
          tagList(bs_icon("arrow-left"), " Back to Summary"),
          class = "btn-outline-secondary"
        ),
        extra = .scenario_slots_ui("scenario", r$plan_tier)
      )
    })

    # -- Color-coded method selector -------------------------------------------
    # Renders a radioButtons where each label has a colored dot indicating
    # data sufficiency, using the same thresholds as the method_hint below.
    output$method_selector <- renderUI({
      n <- if (!is.null(r$overhead_monthly)) nrow(r$overhead_monthly) else 0L

      dot <- function(color) {
        tags$span(
          style = paste0(
            "display:inline-block;width:10px;height:10px;border-radius:50%;",
            "background:",
            color,
            ";margin-right:5px;vertical-align:middle;"
          )
        )
      }

      linear_dot <- dot("#16A34A")
      ets_dot <- if (n >= 12L) {
        dot("#16A34A")
      } else if (n >= 6L) {
        dot("#F59E0B")
      } else {
        dot("#DC2626")
      }
      arima_dot <- if (n >= 30L) {
        dot("#16A34A")
      } else if (n >= 12L) {
        dot("#F59E0B")
      } else {
        dot("#DC2626")
      }

      radioButtons(
        ns("method"),
        label = NULL,
        choiceNames = list(
          tagList(linear_dot, "Linear regression"),
          tagList(ets_dot, "ETS (exponential smoothing)"),
          tagList(arima_dot, "ARIMA")
        ),
        choiceValues = list("linear", "ets", "arima"),
        selected = isolate(input$method) %||% "linear"
      )
    })
    outputOptions(output, "method_selector", suspendWhenHidden = FALSE)

    # -- Method hint (plain-language guidance shown below the method selector) ---
    output$method_hint <- renderUI({
      n_periods <- if (!is.null(r$overhead_monthly)) {
        nrow(r$overhead_monthly)
      } else {
        0L
      }
      hint <- switch(
        input$method %||% "linear",
        linear = list(
          icon = "check2-circle",
          theme = "text-body",
          head = paste0(
            "Linear regression \u2014 ",
            n_periods,
            " observed period",
            if (n_periods != 1L) "s"
          ),
          body = paste0(
            "Fits a straight-line trend to your data. Best when you have ",
            "fewer than 20 periods or expect steady, predictable growth. ",
            "The simplest option and easiest to explain to others."
          )
        ),
        ets = list(
          icon = "arrow-repeat",
          theme = "text-body",
          head = paste0(
            "Exponential smoothing \u2014 ",
            n_periods,
            " observed period",
            if (n_periods != 1L) "s"
          ),
          body = paste0(
            "Weights recent periods more heavily than older ones, so the model ",
            "adapts as your practice evolves. A good middle ground for practices ",
            "with 20\u201340 periods of history where recent months are more ",
            "representative than the full record."
          )
        ),
        arima = list(
          icon = "activity",
          theme = "text-body",
          head = paste0(
            "ARIMA \u2014 ",
            n_periods,
            " observed period",
            if (n_periods != 1L) "s",
            if (n_periods < 30L) " (limited data)" else ""
          ),
          body = paste0(
            "Identifies patterns in how each period relates to prior ones. ",
            "Works best with 30 or more periods of history. ",
            if (n_periods < 30L) {
              "With your current history, linear or ETS will likely be more stable. "
            } else {
              ""
            },
            "Can capture subtle cycles but may over-fit with sparse data."
          )
        )
      )
      tags$div(
        class = paste("small mb-2", hint$theme),
        tags$span(
          class = "fw-semibold",
          bs_icon(hint$icon, size = "0.9em"),
          " ",
          hint$head
        ),
        tags$br(),
        hint$body
      )
    })

    # -- Show target income input only on the Target sub-tab -----------------
    output$target_input_ui <- renderUI({
      req(input$forecast_type)
      if (input$forecast_type == "target") {
        numericInput(
          ns("target_income"),
          if (is_weekly()) {
            "Weekly net income target ($)"
          } else {
            "Monthly net income target ($)"
          },
          value = 5000,
          min = 0,
          step = 500
        )
      }
    })

    # -- Frequency helper -----------------------------------------------------
    is_weekly <- reactive({
      !is.null(r$overhead_monthly) &&
        "week_start" %in% names(r$overhead_monthly)
    })

    # -- Membership tier UI (Practice Profile sidebar section) ----------------
    proj_n_tiers <- reactiveVal(1L)

    # -- Global Start Over: clear this module's local UI state -------------
    observeEvent(
      r$reset_signal,
      {
        proj_n_tiers(1L)
        shown_warnings(character(0))
        n_overhead_events(0L)
        n_fee_events(0L)
      },
      ignoreInit = TRUE
    )

    # When the scenario populates r$membership_tiers, sync the tier count so
    # the sidebar pre-fills with the end-of-period values.
    observeEvent(
      r$membership_tiers,
      {
        n <- length(r$membership_tiers)
        if (n > 0L && n != proj_n_tiers()) {
          proj_n_tiers(n)
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = FALSE
    )

    observeEvent(input$proj_btn_add_tier, {
      proj_n_tiers(proj_n_tiers() + 1L)
    })

    .nn_proj <- function(x) {
      v <- x %||% 0
      if (length(v) != 1L || !is.finite(v)) 0 else v
    }

    # Tracks the last r$membership_tiers value this renderUI actually baked
    # into its inputs, so it can tell "a new scenario just landed" (seed
    # should win) apart from "re-rendering for some other reason, e.g. Add
    # Tier" (the user's own typing, via isolate(input[[...]]), should win).
    # Without this, isolate(input[[...]]) %||% seed always prefers the
    # input side once any value exists there -- and a numericInput reports
    # its own default (0) the first time it's ever bound, which is already
    # non-NULL. That silently discarded every later scenario: generate,
    # visit Projections once (0 gets latched in), Start Over, generate a
    # *different* scenario -- the stale 0 always won over the fresh seed,
    # which is exactly the "demo/old scenario's numbers stick around
    # despite Start Over" bug report.
    last_applied_tiers <- reactiveVal(NULL)

    output$proj_tier_ui <- renderUI({
      n <- proj_n_tiers()
      tiers_seed <- r$membership_tiers
      is_new_scenario <- !identical(tiers_seed, isolate(last_applied_tiers()))
      if (is_new_scenario) {
        isolate(last_applied_tiers(tiers_seed))
      }
      lapply(seq_len(n), function(i) {
        seed <- if (!is.null(tiers_seed) && i <= length(tiers_seed)) {
          tiers_seed[[i]]
        } else {
          NULL
        }
        if (is_new_scenario) {
          saved_label <- seed$label %||% ""
          saved_members <- seed$members %||% 0L
          saved_fee <- seed$fee %||% 0
        } else {
          saved_label <- isolate(input[[paste0("proj_tier_label_", i)]]) %||%
            (seed$label %||% "")
          saved_members <- isolate(input[[paste0("proj_tier_members_", i)]]) %||%
            (seed$members %||% 0L)
          saved_fee <- isolate(input[[paste0("proj_tier_fee_", i)]]) %||%
            (seed$fee %||% 0)
        }
        div(
          class = "border rounded p-2 mb-2",
          if (n > 1L) {
            tags$div(
              class = "d-flex justify-content-between align-items-center mb-1",
              tags$span(
                class = "small fw-semibold text-muted",
                paste("Tier", i)
              ),
              if (i > 1L) {
                actionButton(
                  ns(paste0("proj_rm_tier_", i)),
                  bs_icon("x", title = paste("Remove tier", i)),
                  class = "btn-outline-danger btn-sm py-0 px-1"
                )
              }
            )
          },
          # Stacked, not a 3-column layout_columns() row: this sits in the
          # narrow Practice Profile sidebar, which isn't wide enough for
          # Label/Members/Fee side by side without smooshing/overflowing --
          # matches how the Planned Overhead Events and Planned Fee Changes
          # sections just below (also user-addable, bordered-card rows)
          # already stack their fields vertically instead of using columns.
          textInput(
            ns(paste0("proj_tier_label_", i)),
            if (i == 1L && n == 1L) "Label (optional)" else "Label",
            value = saved_label,
            placeholder = "e.g. Adult"
          ),
          numericInput(
            ns(paste0("proj_tier_members_", i)),
            "Members",
            value = saved_members,
            min = 0L,
            step = 1L
          ),
          numericInput(
            ns(paste0("proj_tier_fee_", i)),
            "Fee ($/mo)",
            value = saved_fee,
            min = 0,
            step = 5
          )
        )
      })
    })

    observe({
      n <- proj_n_tiers()
      lapply(seq_len(n), function(i) {
        if (i > 1L) {
          observeEvent(
            input[[paste0("proj_rm_tier_", i)]],
            {
              proj_n_tiers(max(1L, proj_n_tiers() - 1L))
            },
            ignoreInit = TRUE,
            once = TRUE
          )
        }
      })
    })

    # Raw (non-debounced) snapshot of every tier's current field values --
    # re-fires on every keystroke in any Label/Members/Fee box, same as
    # every downstream reactive reading input[[...]] directly used to.
    # Debounced immediately below; nothing else should read this reactive.
    .proj_tier_raw <- reactive({
      n <- proj_n_tiers()
      lapply(seq_len(n), function(i) {
        list(
          label = input[[paste0("proj_tier_label_", i)]] %||% "",
          members = .nn_proj(input[[paste0("proj_tier_members_", i)]]),
          fee = .nn_proj(input[[paste0("proj_tier_fee_", i)]])
        )
      })
    })

    # Debounced: proj_total_members/proj_total_revenue/proj_avg_fee/
    # profile_ok/fee_per_period, and the observe() below that syncs
    # r$panel_size/r$membership_fee/r$membership_tiers, all previously read
    # the tier inputs directly and so recomputed on every single keystroke
    # -- confirmed live this made the whole Membership Profile section feel
    # laggy while typing any Label, Members, or Fee value. Reading from this
    # debounced reactive instead means that whole downstream chain waits
    # for a short pause in typing before recomputing, matching how the
    # user's cursor moving to a different field (not muscle-memory typing
    # speed) is the real "done editing" signal.
    proj_tier_debounced <- shiny::debounce(.proj_tier_raw, millis = 500)

    proj_total_members <- reactive({
      sum(vapply(proj_tier_debounced(), \(t) t$members, numeric(1)))
    })

    proj_total_revenue <- reactive({
      sum(vapply(
        proj_tier_debounced(),
        \(t) t$members * t$fee,
        numeric(1)
      ))
    })

    proj_avg_fee <- reactive({
      m <- proj_total_members()
      if (m > 0) proj_total_revenue() / m else 0
    })

    proj_tiers_list <- reactive({
      proj_tier_debounced()
    })

    # -- Practice profile helpers ---------------------------------------------
    # profile_ok() is TRUE only when total members and avg fee are positive.
    profile_ok <- reactive({
      isTRUE(proj_total_members() > 0 && proj_avg_fee() > 0)
    })

    # membership_fee is always entered as $/member/month.  When the data are
    # weekly the forecast quantities are in $/week, so divide by 4.33.
    fee_per_period <- reactive({
      mf <- proj_avg_fee()
      if (is.null(mf) || is.na(mf) || mf <= 0) {
        return(NA_real_)
      }
      if (is_weekly()) mf / 4.33 else mf
    })

    # Sync tier aggregates into shared reactive state so interpret/report can
    # read them without re-wiring.
    observe({
      m <- proj_total_members()
      f <- proj_avg_fee()
      r$panel_size <- if (m > 0) m else NULL
      r$membership_fee <- if (f > 0) f else NULL
      tiers <- proj_tiers_list()
      r$membership_tiers <- if (
        any(vapply(tiers, \(t) t$members > 0, logical(1)))
      ) {
        tiers
      } else {
        NULL
      }
    })

    # -- Value text auto-shrink -----------------------------------------------
    # bslib value_box renders the value inside a paragraph styled with Bootstrap
    # fs-2 (~1.5 rem). Long strings wrap inside the fixed 90 px box height.
    # Wrapping the value in a span with an inline font-size overrides fs-2 via
    # CSS specificity, keeping everything on one or two lines.
    #
    # Calibrated thresholds (characters):
    #   <= 9  : default  (fs-2, ~1.5 rem) -- "Achieved", "$1,234", "12"
    #   10-14 : medium   (1.05 rem)       -- "Not in horizon", "Jan 2026"
    #   >= 15 : small    (0.88 rem)       -- "Achieved (at risk)"
    shrink_value <- function(x) {
      s <- as.character(x)
      n <- nchar(s)
      if (n >= 15) {
        tags$span(style = "font-size: 0.88rem; line-height: 1.25;", s)
      } else if (n >= 10) {
        tags$span(style = "font-size: 1.05rem; line-height: 1.3;", s)
      } else {
        s
      }
    }

    # -- Reactive forecast results --------------------------------------------
    # Break-even/Revenue/Target all run in a single background process via
    # shiny::ExtendedTask + mirai, instead of blocking the main R process on
    # eventReactive() (as this used to). forecast_breakeven()/forecast_revenue()/
    # forecast_target() call `forecast` (base R's stats::lm/HoltWinters or the
    # forecast package's ets()/auto.arima()), which for ARIMA on a longer
    # history is slow enough to freeze the *entire app* -- every other
    # session/practice too, since Shiny is single-threaded per R process --
    # for the duration of one click. Running it in a mirai worker keeps the
    # main process free to keep serving everyone else while this one
    # practice's forecast computes. input_task_button("btn_run", ...) (in the
    # sidebar above) is bound to this task below, so it shows its built-in
    # busy/spinner state for exactly as long as the task is running.
    #
    # All three forecasts share one task (not three separate ExtendedTasks):
    # they share income_summary()/overhead_monthly as inputs, so running them
    # together in the same worker call means their warnings land in one place
    # to de-duplicate against shown_warnings, matching the original
    # run_forecast() helper's behavior before this refactor.

    # Tracks warning messages already shown in the current btn_run event.
    # Reset each time Run Forecast is clicked so deduplication is per-run, not
    # per-session.
    shown_warnings <- reactiveVal(character(0))

    # The function passed to ExtendedTask$new() runs in *this* R process (it
    # receives the plain values invoke() is called with below) and must
    # return a promise -- a bare mirai::mirai() call satisfies that directly,
    # since mirai objects implement the promise interface themselves. The
    # `{...}` expression, by contrast, runs in a background mirai worker
    # process: it has no access to reactive values, `input`, or anything else
    # in this environment, only the objects explicitly passed in as named
    # arguments below -- hence directCareForecastR:: staying fully namespaced
    # (already true before this refactor) and income_summary/overhead_monthly
    # etc. being passed as plain data frames, not reactives.
    forecast_task <- ExtendedTask$new(function(
      income_summary,
      overhead_monthly,
      method,
      horizon,
      confidence,
      target_income,
      run_target
    ) {
      mirai::mirai(
        {
          # Runs one forecast call, capturing its data-volume warnings
          # (message + class only -- condition objects themselves don't need
          # to survive the trip back across the process boundary) instead of
          # showNotification()-ing them directly, since there is no Shiny
          # session in this worker process to notify. Errors are likewise
          # captured rather than allowed to fail the whole mirai call, so a
          # problem in one of the three forecasts (e.g. Target's
          # target_income) doesn't discard the other two.
          run_one <- function(expr) {
            warns <- list()
            result <- tryCatch(
              withCallingHandlers(
                expr,
                warning = function(w) {
                  warns[[length(warns) + 1L]] <<- list(
                    message = conditionMessage(w),
                    classes = class(w)
                  )
                  invokeRestart("muffleWarning")
                }
              ),
              error = function(e) {
                structure(
                  list(message = conditionMessage(e)),
                  class = "forecast_task_error"
                )
              }
            )
            list(result = result, warnings = warns)
          }

          list(
            breakeven = run_one(directCareForecastR::forecast_breakeven(
              income_summary = income_summary,
              overhead_summary = overhead_monthly,
              method = method,
              horizon = horizon,
              confidence_level = confidence
            )),
            revenue = run_one(directCareForecastR::forecast_revenue(
              income_summary = income_summary,
              method = method,
              horizon = horizon,
              confidence_level = confidence
            )),
            target = if (run_target) {
              run_one(directCareForecastR::forecast_target(
                income_summary = income_summary,
                overhead_summary = overhead_monthly,
                target_income = target_income,
                method = method,
                horizon = horizon,
                confidence_level = confidence
              ))
            } else {
              NULL
            }
          )
        },
        income_summary = income_summary,
        overhead_monthly = overhead_monthly,
        method = method,
        horizon = horizon,
        confidence = confidence,
        target_income = target_income,
        run_target = run_target
      )
    })
    bslib::bind_task_button(forecast_task, "btn_run")

    # Income summary: use uploaded/manually entered income if available,
    # otherwise fall back to a proportional proxy so projections still run.
    income_summary <- reactive({
      req(r$overhead_monthly)
      if (!is.null(r$income_monthly) && nrow(r$income_monthly) > 0) {
        r$income_monthly
      } else {
        showNotification(
          paste0(
            "No income data found. Projections are based on a synthetic revenue proxy ",
            "(80% of overhead) and should be treated as rough estimates only. ",
            "Upload income data or use manual entry for accurate forecasts."
          ),
          type = "warning",
          duration = 10
        )
        tibble::tibble(
          practice_id = r$overhead_monthly$practice_id,
          year = r$overhead_monthly$year,
          month = r$overhead_monthly$month,
          total_revenue = r$overhead_monthly$total_overhead * 0.8
        )
      }
    })

    observeEvent(input$btn_run, {
      req(r$overhead_monthly)
      shown_warnings(character(0))
      forecast_task$invoke(
        income_summary = income_summary(),
        overhead_monthly = r$overhead_monthly,
        method = input$method,
        horizon = input$horizon,
        confidence = input$confidence,
        target_income = input$target_income,
        # Preserves the original eventReactive-based behavior: Target only
        # computes if the user was on the Income Target sub-tab at the
        # moment Run Forecast was clicked (see the partial-report regression
        # tests in test-mod-projections.R).
        run_target = isTRUE(input$forecast_type == "target") &&
          !is.null(input$target_income)
      )
    })

    # Surfaces each forecast's captured warnings/errors as the same
    # showNotification() calls run_forecast() used to make directly -- moved
    # here, out of the reactive graph, since the worker process that computes
    # them has no session to notify from. Fires once per completed task
    # invocation (forecast_task$result() only changes when the task's status
    # actually transitions), not once per downstream reactive read.
    observeEvent(forecast_task$result(), {
      out <- forecast_task$result()
      notify_section <- function(section) {
        if (is.null(section)) {
          return(invisible())
        }
        for (w in section$warnings) {
          if (w$message %in% shown_warnings()) {
            next
          }
          shown_warnings(c(shown_warnings(), w$message))
          if ("dcForecastR_insufficient_data" %in% w$classes) {
            showNotification(w$message, type = "error", duration = 15)
          } else if ("dcForecastR_method_fallback" %in% w$classes) {
            showNotification(w$message, type = "warning", duration = 12)
          } else if ("dcForecastR_low_data_advisory" %in% w$classes) {
            showNotification(w$message, type = "message", duration = 10)
          }
        }
        if (inherits(section$result, "forecast_task_error")) {
          showNotification(section$result$message, type = "error", duration = 8)
        }
      }
      notify_section(out$breakeven)
      notify_section(out$revenue)
      notify_section(out$target)
    })

    # Resolves one forecast section down to the plain result adj_breakeven()/
    # adj_revenue()/adj_target() below already expect (NULL if that forecast
    # was never run this task, or errored -- the error itself was already
    # surfaced via notify_section() above).
    .extract_forecast <- function(section) {
      if (is.null(section)) {
        return(NULL)
      }
      if (inherits(section$result, "forecast_task_error")) {
        return(NULL)
      }
      section$result
    }

    breakeven_result <- reactive(.extract_forecast(forecast_task$result()$breakeven))
    revenue_result <- reactive(.extract_forecast(forecast_task$result()$revenue))
    target_result <- reactive(.extract_forecast(forecast_task$result()$target))

    # -- Conditional overhead-model controls ---------------------------------
    output$overhead_model_controls <- renderUI({
      model <- input$overhead_model %||% "trend"
      if (model == "trend") {
        sliderInput(
          ns("overhead_growth"),
          "Overhead growth (%/yr)",
          min = -60,
          max = 60,
          value = 0,
          step = 0.5
        )
      } else if (model == "custom") {
        default_val <- if (
          !is.null(r$overhead_monthly) && nrow(r$overhead_monthly) > 0
        ) {
          round(mean(r$overhead_monthly$total_overhead, na.rm = TRUE))
        } else {
          0L
        }
        numericInput(
          ns("overhead_custom"),
          "Monthly overhead ($)",
          value = default_val,
          min = 0,
          step = 100
        )
      }
      # avg_full / avg_recent: computed from data, no extra input needed
    })

    # -- Flat-overhead helper -------------------------------------------------
    # Returns a single numeric (monthly $) when a flat model is selected, or
    # NULL for "trend" (the growth multiplier path in apply_growth_assumptions).
    compute_flat_overhead <- reactive({
      model <- input$overhead_model %||% "trend"
      ovhd <- r$overhead_monthly
      has_data <- !is.null(ovhd) && nrow(ovhd) > 0
      recent_n <- if (has_data) min(6L, nrow(ovhd)) else 0L

      if (model == "avg_full" && has_data) {
        mean(ovhd$total_overhead, na.rm = TRUE)
      } else if (model == "avg_recent" && has_data) {
        mean(tail(ovhd, recent_n)$total_overhead, na.rm = TRUE)
      } else if (model == "max_full" && has_data) {
        max(ovhd$total_overhead, na.rm = TRUE)
      } else if (model == "max_recent" && has_data) {
        max(tail(ovhd, recent_n)$total_overhead, na.rm = TRUE)
      } else if (model == "custom") {
        as.numeric(input$overhead_custom %||% 0)
      } else {
        NULL
      } # "trend": use growth multiplier
    })

    # -- Growth / overhead-adjusted reactives ---------------------------------
    # Re-fire whenever sliders or model selector changes, without re-running
    # the expensive forecast model.

    # Shared overhead parameters used by both adj_breakeven and adj_target.
    # Kept separate from adj_revenue so that revenue tab does not invalidate
    # when the overhead model or growth slider changes.
    overhead_assumptions <- reactive({
      model <- input$overhead_model %||% "trend"
      list(
        growth_pct = if (model == "trend") input$overhead_growth %||% 0 else 0,
        flat = compute_flat_overhead()
      )
    })

    # -- Planned overhead events UI and state ----------------------------------
    n_overhead_events <- reactiveVal(0L)

    observeEvent(input$btn_add_overhead_event, {
      n_overhead_events(n_overhead_events() + 1L)
    })

    output$overhead_events_ui <- renderUI({
      n <- n_overhead_events()
      if (n == 0L) {
        return(tags$p(
          class = "small text-muted mb-1",
          "No events added."
        ))
      }

      # Build date choices starting from the first forecast period,
      # which is the month after the last observed period in the data.
      horizon <- input$horizon %||% 12L
      last_obs <- if (
        !is.null(r$overhead_monthly) && nrow(r$overhead_monthly) > 0
      ) {
        om <- r$overhead_monthly
        if ("period_start" %in% names(om)) {
          max(om$period_start, na.rm = TRUE)
        } else if (all(c("year", "month") %in% names(om))) {
          idx <- which.max(om$year * 12L + om$month)
          lubridate::make_date(om$year[idx], om$month[idx], 1L)
        } else {
          Sys.Date()
        }
      } else {
        Sys.Date()
      }
      first_mo <- as.Date(format(last_obs + 31L, "%Y-%m-01"))
      mo_seq <- seq(first_mo, by = "month", length.out = as.integer(horizon))
      mo_choices <- stats::setNames(
        as.character(mo_seq),
        format(mo_seq, "%b %Y")
      )

      lapply(seq_len(n), function(i) {
        div(
          class = "border rounded p-2 mb-2",
          tags$div(
            class = "d-flex justify-content-between align-items-center mb-1",
            tags$span(
              class = "small fw-semibold text-muted",
              paste("Event", i)
            ),
            actionButton(
              ns(paste0("btn_rm_ov_event_", i)),
              bs_icon("x", title = paste("Remove event", i)),
              class = "btn-outline-danger btn-sm py-0 px-1"
            )
          ),
          # isolate() on every input[[...]] read below is deliberate: these
          # exist purely to seed value=/selected= with whatever the user
          # already typed, so this renderUI survives its own re-renders
          # without clearing the fields -- reading them non-isolated (as
          # this block used to) makes renderUI depend on its own inputs, so
          # every keystroke's debounced value update invalidates and
          # re-runs it, tearing down and recreating these DOM nodes and
          # kicking focus out of whatever field the user was typing in.
          textInput(
            ns(paste0("ov_event_label_", i)),
            NULL,
            value = isolate(input[[paste0("ov_event_label_", i)]]) %||% "",
            placeholder = "e.g. Hire MA"
          ),
          selectInput(
            ns(paste0("ov_event_start_", i)),
            NULL,
            choices = mo_choices,
            selected = isolate(input[[paste0("ov_event_start_", i)]]) %||%
              as.character(mo_seq[1L])
          ),
          numericInput(
            ns(paste0("ov_event_cost_", i)),
            "Monthly cost increase ($)",
            value = isolate(input[[paste0("ov_event_cost_", i)]]) %||% 0,
            min = 0,
            step = 100
          )
        )
      })
    })

    observe({
      n <- n_overhead_events()
      lapply(seq_len(n), function(i) {
        btn_id <- paste0("btn_rm_ov_event_", i)
        observeEvent(
          input[[btn_id]],
          {
            n_overhead_events(max(0L, n_overhead_events() - 1L))
          },
          ignoreInit = TRUE,
          once = TRUE
        )
      })
    })

    overhead_events_df <- reactive({
      n <- n_overhead_events()
      if (n == 0L) {
        return(NULL)
      }
      rows <- lapply(seq_len(n), function(i) {
        cost <- as.numeric(input[[paste0("ov_event_cost_", i)]] %||% 0)
        start_str <- input[[paste0("ov_event_start_", i)]]
        if (is.null(start_str) || !nzchar(start_str)) {
          return(NULL)
        }
        data.frame(
          label = input[[paste0("ov_event_label_", i)]] %||% paste("Event", i),
          start_date = as.Date(start_str),
          monthly_cost = cost,
          stringsAsFactors = FALSE
        )
      })
      valid <- Filter(Negate(is.null), rows)
      if (length(valid) == 0L) {
        return(NULL)
      }
      do.call(rbind, valid)
    })

    # -- Planned fee-change events UI and state --------------------------------
    n_fee_events <- reactiveVal(0L)

    observeEvent(input$btn_add_fee_event, {
      n_fee_events(n_fee_events() + 1L)
    })

    # Month choices shared with the overhead-events UI (first forecast period
    # through the end of the horizon).
    .fee_event_month_choices <- reactive({
      horizon <- input$horizon %||% 12L
      last_obs <- if (
        !is.null(r$overhead_monthly) && nrow(r$overhead_monthly) > 0
      ) {
        om <- r$overhead_monthly
        if ("period_start" %in% names(om)) {
          max(om$period_start, na.rm = TRUE)
        } else if (all(c("year", "month") %in% names(om))) {
          idx <- which.max(om$year * 12L + om$month)
          lubridate::make_date(om$year[idx], om$month[idx], 1L)
        } else {
          Sys.Date()
        }
      } else {
        Sys.Date()
      }
      first_mo <- as.Date(format(last_obs + 31L, "%Y-%m-01"))
      mo_seq <- seq(first_mo, by = "month", length.out = as.integer(horizon))
      stats::setNames(as.character(mo_seq), format(mo_seq, "%b %Y"))
    })

    output$fee_events_ui <- renderUI({
      n <- n_fee_events()
      if (n == 0L) {
        return(tags$p(
          class = "small text-muted mb-1",
          "No fee changes added."
        ))
      }

      mo_choices <- .fee_event_month_choices()
      tiers <- proj_tiers_list()
      tier_choices <- stats::setNames(
        as.character(seq_along(tiers)),
        vapply(
          seq_along(tiers),
          function(i) {
            lbl <- if (nzchar(tiers[[i]]$label)) {
              tiers[[i]]$label
            } else {
              paste("Tier", i)
            }
            paste0(lbl, " (", fmt_dollar(tiers[[i]]$fee), "/mo)")
          },
          character(1)
        )
      )

      lapply(seq_len(n), function(i) {
        div(
          class = "border rounded p-2 mb-2",
          tags$div(
            class = "d-flex justify-content-between align-items-center mb-1",
            tags$span(
              class = "small fw-semibold text-muted",
              paste("Fee change", i)
            ),
            actionButton(
              ns(paste0("btn_rm_fee_event_", i)),
              bs_icon("x", title = paste("Remove fee change", i)),
              class = "btn-outline-danger btn-sm py-0 px-1"
            )
          ),
          # isolate() here for the same reason as overhead_events_ui above --
          # avoids this renderUI depending on its own inputs and
          # self-triggering a focus-losing re-render on every keystroke.
          selectInput(
            ns(paste0("fee_event_tier_", i)),
            "Tier",
            choices = tier_choices,
            selected = isolate(input[[paste0("fee_event_tier_", i)]]) %||%
              tier_choices[1L]
          ),
          selectInput(
            ns(paste0("fee_event_start_", i)),
            NULL,
            choices = mo_choices,
            selected = isolate(input[[paste0("fee_event_start_", i)]]) %||%
              mo_choices[1L]
          ),
          numericInput(
            ns(paste0("fee_event_new_fee_", i)),
            "New fee ($/mo)",
            value = isolate(input[[paste0("fee_event_new_fee_", i)]]) %||% 0,
            min = 0,
            step = 5
          )
        )
      })
    })

    observe({
      n <- n_fee_events()
      lapply(seq_len(n), function(i) {
        btn_id <- paste0("btn_rm_fee_event_", i)
        observeEvent(
          input[[btn_id]],
          {
            n_fee_events(max(0L, n_fee_events() - 1L))
          },
          ignoreInit = TRUE,
          once = TRUE
        )
      })
    })

    fee_events_df <- reactive({
      n <- n_fee_events()
      if (n == 0L) {
        return(NULL)
      }
      tiers <- proj_tiers_list()
      rows <- lapply(seq_len(n), function(i) {
        tier_idx <- suppressWarnings(as.integer(
          input[[paste0("fee_event_tier_", i)]]
        ))
        start_str <- input[[paste0("fee_event_start_", i)]]
        new_fee <- as.numeric(input[[paste0("fee_event_new_fee_", i)]] %||% 0)
        if (
          is.null(start_str) ||
            !nzchar(start_str) ||
            is.na(tier_idx) ||
            tier_idx < 1L ||
            tier_idx > length(tiers)
        ) {
          return(NULL)
        }
        tier <- tiers[[tier_idx]]
        delta <- (new_fee - tier$fee) * tier$members
        data.frame(
          start_date = as.Date(start_str),
          revenue_delta = delta,
          stringsAsFactors = FALSE
        )
      })
      valid <- Filter(Negate(is.null), rows)
      if (length(valid) == 0L) {
        return(NULL)
      }
      do.call(rbind, valid)
    })

    adj_breakeven <- reactive({
      req(breakeven_result())
      oa <- overhead_assumptions()
      res <- apply_growth_assumptions(
        breakeven_result(),
        income_growth_pct = input$income_growth %||% 0,
        overhead_growth_pct = oa$growth_pct,
        overhead_flat = oa$flat
      )
      res <- apply_overhead_events(res, overhead_events_df())
      apply_membership_fee_events(res, fee_events_df())
    })

    # Pro-exclusive: what income/overhead growth-rate change would reach
    # break-even within the forecast horizon, when adj_breakeven() doesn't.
    # Computed against the raw breakeven_result() (not adj_breakeven()),
    # since goal_seek_breakeven() re-derives the growth adjustment itself
    # for each candidate rate -- see its own comment for why. Only
    # computed in the case where it would say anything (interpret_breakeven()
    # ignores it otherwise too), so a Starter/Free practice, or a Pro one
    # that's already at break-even, never pays for the extra binary search.
    breakeven_goal_seek <- reactive({
      req(adj_breakeven())
      if (!.has_pro_plan(r$plan_tier) || !is.na(adj_breakeven()$breakeven_date)) {
        return(NULL)
      }
      oa <- overhead_assumptions()
      goal_seek_breakeven(
        breakeven_result(),
        current_income_growth_pct = input$income_growth %||% 0,
        current_overhead_growth_pct = oa$growth_pct,
        overhead_flat = oa$flat,
        overhead_events = overhead_events_df(),
        fee_events = fee_events_df()
      )
    })

    # Pro-exclusive, mirror image of breakeven_goal_seek: how many members'
    # worth of revenue an ALREADY-achieved break-even could lose and still
    # be sustained through the horizon. Computed against adj_breakeven()
    # directly (not breakeven_result(), unlike goal-seek) since
    # stress_test_breakeven() applies its own adjustment on top of an
    # already-fully-adjusted result rather than re-deriving the growth
    # adjustment itself. Only computed when it would say anything
    # (interpret_breakeven() ignores it otherwise too), so a Starter/Free
    # practice, or a Pro one not yet at break-even, never pays for the
    # extra binary search.
    breakeven_stress_test <- reactive({
      req(adj_breakeven())
      if (
        !.has_pro_plan(r$plan_tier) ||
          !identical(adj_breakeven()$periods_to_breakeven, 0L)
      ) {
        return(NULL)
      }
      stress_test_breakeven(
        adj_breakeven(),
        panel_size = r$panel_size,
        membership_fee = r$membership_fee
      )
    })

    # Pro-exclusive: compare saved forecast scenarios' break-even timing.
    # req(adj_breakeven()) isn't needed for correctness here (this reads
    # straight from the DB, not from the current session's forecast), but
    # it does double as this reactive's only invalidation trigger --
    # mod_scenario_slots_server() doesn't expose a "scenario list changed"
    # signal, so re-running whenever the user (re-)runs a forecast is a
    # cheap, good-enough proxy for "state may have changed" rather than
    # querying on every render. Returns `count` (so the free/starter
    # upsell teaser can tell "0 or 1 saved" apart from "2+ saved but not
    # Pro") and `narrative` (NULL unless Pro and 2+ scenarios have a
    # comparable adj_breakeven snapshot).
    scenario_compare_state <- reactive({
      req(adj_breakeven())
      req(r$practice_id)
      con <- directCareAuth::db_connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      listed <- directCareScenarios::scenario_list(con, "dca_forecast_scenarios", r$practice_id)
      if (nrow(listed) < 2L || !.has_pro_plan(r$plan_tier)) {
        return(list(count = nrow(listed), narrative = NULL))
      }
      loaded <- lapply(listed$id, function(id) directCareScenarios::scenario_load_forecast(con, id))
      scenarios <- lapply(loaded, function(x) {
        list(label = x$label, result = x$bundle$results$adj_breakeven)
      })
      # Scenarios saved before the practice ever ran a Break-even forecast
      # have no adj_breakeven snapshot to compare -- drop them rather than
      # erroring.
      scenarios <- Filter(function(s) !is.null(s$result), scenarios)
      list(
        count = nrow(listed),
        narrative = if (length(scenarios) >= 2L) compare_breakeven_scenarios(scenarios) else NULL
      )
    })

    adj_revenue <- reactive({
      req(revenue_result())
      res <- apply_growth_assumptions(
        revenue_result(),
        income_growth_pct = input$income_growth %||% 0
      )
      apply_membership_fee_events(res, fee_events_df())
    })

    adj_target <- reactive({
      req(target_result())
      oa <- overhead_assumptions()
      res <- apply_growth_assumptions(
        target_result(),
        income_growth_pct = input$income_growth %||% 0,
        overhead_growth_pct = oa$growth_pct,
        overhead_flat = oa$flat,
        # Always pass target_income so apply_growth_assumptions can keep
        # fd$required_revenue consistent with the (possibly adjusted)
        # overhead forecast -- whether that's a flat model or the fitted
        # trend with a non-zero overhead growth rate.
        target_income_override = input$target_income
      )
      # Store target_income on result so apply_overhead_events can re-derive
      # required_revenue = overhead_forecast + target_income after event steps.
      res$target_income <- as.numeric(input$target_income %||% 0)
      res <- apply_overhead_events(res, overhead_events_df())
      apply_membership_fee_events(res, fee_events_df())
    })

    # Pro-exclusive: same idea as breakeven_goal_seek, applied to the
    # income-target forecast -- see its own comment for the rationale.
    target_goal_seek <- reactive({
      req(adj_target())
      if (!.has_pro_plan(r$plan_tier) || !is.na(adj_target()$target_date)) {
        return(NULL)
      }
      oa <- overhead_assumptions()
      goal_seek_target(
        target_result(),
        current_income_growth_pct = input$income_growth %||% 0,
        current_overhead_growth_pct = oa$growth_pct,
        overhead_flat = oa$flat,
        target_income_override = input$target_income,
        overhead_events = overhead_events_df(),
        fee_events = fee_events_df()
      )
    })

    # -- Break-even UI --------------------------------------------------------
    output$breakeven_ui <- renderUI({
      res <- adj_breakeven()
      if (is.null(res)) {
        return(p(
          class = "text-muted",
          "Click 'Run Forecast' to generate results."
        ))
      }

      sustained <- breakeven_is_sustained(res)
      already <- identical(res$periods_to_breakeven, 0L)

      bkevn_value <- if (already && isTRUE(sustained)) {
        "Achieved"
      } else if (already && isFALSE(sustained)) {
        "Achieved (at risk)"
      } else if (already) {
        "Achieved"
      } else if (is.na(res$breakeven_date)) {
        "Not in horizon"
      } else {
        format(res$breakeven_date, "%b %Y")
      }

      bkevn_theme <- if (already && isFALSE(sustained)) {
        "warning"
      } else if (already) {
        "success"
      } else if (is.na(res$breakeven_date)) {
        "danger"
      } else {
        "primary"
      }

      # Build value-box list; add member-count box when profile is complete
      bkevn_boxes <- list(
        value_box(
          title = "Break-even",
          value = shrink_value(bkevn_value),
          theme = bkevn_theme,
          height = "90px"
        ),
        value_box(
          title = if (is_weekly()) "Weeks away" else "Months away",
          value = if (is.na(res$periods_to_breakeven)) {
            "--"
          } else {
            res$periods_to_breakeven
          },
          theme = "secondary",
          height = "90px"
        ),
        value_box(
          title = "Current surplus / deficit",
          value = fmt_dollar(res$current_surplus_deficit),
          theme = if (res$current_surplus_deficit >= 0) {
            "success"
          } else {
            "warning"
          },
          height = "90px"
        )
      )
      if (profile_ok()) {
        # current_overhead_avg is the rolling mean of the last few overhead
        # periods (in per-income-period units). It is more reliable than the
        # raw single-period current_overhead, which is often $0 for weekly
        # data when monthly overhead lands only on month-start rows.
        current_ovhd <- res$current_overhead_avg %||%
          res$current_overhead %||%
          (res$current_revenue - res$current_surplus_deficit)
        members_bkevn <- ceiling(current_ovhd / fee_per_period())
        bkevn_boxes <- c(
          bkevn_boxes,
          list(
            value_box(
              title = "Members to break even",
              value = members_bkevn,
              theme = "info",
              height = "90px"
            )
          )
        )
      }

      tagList(
        do.call(
          layout_column_wrap,
          c(list(width = "180px", fill = FALSE), bkevn_boxes)
        ),
        div(
          style = "max-height: 620px; overflow-y: auto; padding-right: 2px;",
          card(
            full_screen = TRUE,
            card_header("Break-even Forecast"),
            card_body(plotOutput(
              ns("breakeven_plot"),
              height = "560px",
              width = "100%"
            ))
          ),
          card(
            card_header("Interpretation"),
            card_body(uiOutput(ns("breakeven_interpretation")))
          ),
          uiOutput(ns("scenario_comparison_card"))
        ),
      )
    })

    output$breakeven_plot <- renderPlot(
      {
        r$dark_mode # dependency only -- forces a redraw when the theme toggles
        req(adj_breakeven())
        plot_forecast_breakeven(
          adj_breakeven(),
          income_monthly = r$income_monthly,
          overhead_monthly = r$overhead_monthly
        )
      },
      res = 96,
      height = 560,
      width = function() {
        w <- session$clientData[[paste0(
          "output_",
          ns("breakeven_plot"),
          "_width"
        )]]
        max(w %||% 700L, 300L)
      }
    )

    output$breakeven_interpretation <- renderUI({
      req(adj_breakeven())
      tagList(
        HTML(interpret_breakeven(
          adj_breakeven(),
          r$practice_name,
          sustained = breakeven_is_sustained(adj_breakeven()),
          panel_size = r$panel_size,
          membership_fee = r$membership_fee,
          membership_tiers = r$membership_tiers,
          confidence_level = input$confidence,
          goal_seek = breakeven_goal_seek(),
          stress_test = breakeven_stress_test()
        )),
        # Goal-seek and the stress test are mirror images (not-yet-
        # achieved vs. already-achieved), so exactly one of these two
        # upsell notes is ever relevant for a given forecast state -- the
        # scenario-comparison card below is a separate, always-available
        # third upsell for the achieved case.
        if (!.has_pro_plan(r$plan_tier) && is.na(adj_breakeven()$breakeven_date)) {
          .pro_upsell_note(
            ns,
            "Pro also tells you what growth-rate change -- in income, overhead, or both -- would reach break-even within your forecast horizon.",
            btn_id = "btn_see_plans_pro_goal_seek"
          )
        },
        if (
          !.has_pro_plan(r$plan_tier) &&
            identical(adj_breakeven()$periods_to_breakeven, 0L)
        ) {
          .pro_upsell_note(
            ns,
            "Pro also tells you how many members your practice could lose and still sustain break-even.",
            btn_id = "btn_see_plans_pro_stress_test"
          )
        }
      )
    })

    output$scenario_comparison_card <- renderUI({
      state <- scenario_compare_state()
      if (!is.null(state$narrative)) {
        card(
          card_header(bs_icon("bar-chart-line"), " Scenario Comparison"),
          card_body(HTML(state$narrative))
        )
      } else if (!.has_pro_plan(r$plan_tier) && state$count >= 2L) {
        card(
          card_body(
            .pro_upsell_note(
              ns,
              "Pro also compares your saved scenarios' break-even timing, naming which one gets there soonest.",
              btn_id = "btn_see_plans_pro_scenario_compare"
            )
          )
        )
      }
    })

    # -- Revenue UI -----------------------------------------------------------
    output$revenue_ui <- renderUI({
      res <- adj_revenue()
      if (is.null(res)) {
        return(p(
          class = "text-muted",
          "Click 'Run Forecast' to generate results."
        ))
      }

      rev_boxes <- list(
        value_box(
          title = if (is_weekly()) {
            "Current weekly revenue"
          } else {
            "Current monthly revenue"
          },
          value = fmt_dollar(res$current_revenue),
          theme = "primary",
          height = "90px"
        ),
        value_box(
          title = paste(
            "Projected in",
            input$horizon,
            if (is_weekly()) "weeks" else "months"
          ),
          value = fmt_dollar(tail(res$forecast_data$revenue_forecast, 1)),
          theme = "secondary",
          height = "90px"
        ),
        value_box(
          title = "Method",
          value = shrink_value(toupper(res$method)),
          theme = "info",
          height = "90px"
        )
      )
      if (profile_ok()) {
        pmpm_label <- if (is_weekly()) {
          "Revenue per member / week"
        } else {
          "Revenue per member / month"
        }
        pmpm_val <- res$current_revenue / proj_total_members()
        # Compare per-period PMPM against per-period fee (not monthly fee directly).
        pmpm_theme <- if (pmpm_val >= fee_per_period() * 0.9) {
          "success"
        } else {
          "warning"
        }
        rev_boxes <- c(
          rev_boxes,
          list(
            value_box(
              title = pmpm_label,
              value = fmt_dollar(pmpm_val),
              theme = pmpm_theme,
              height = "90px"
            )
          )
        )
      }

      tagList(
        do.call(
          layout_column_wrap,
          c(list(width = "180px", fill = FALSE), rev_boxes)
        ),
        div(
          style = "max-height: 620px; overflow-y: auto; padding-right: 2px;",
          card(
            full_screen = TRUE,
            card_header("Revenue Forecast"),
            card_body(plotOutput(
              ns("revenue_plot"),
              height = "560px",
              width = "100%"
            ))
          ),
          card(
            card_header("Interpretation"),
            card_body(uiOutput(ns("revenue_interpretation")))
          )
        ),
      )
    })

    output$revenue_plot <- renderPlot(
      {
        r$dark_mode # dependency only -- forces a redraw when the theme toggles
        req(adj_revenue())
        plot_forecast_revenue(adj_revenue(), income_monthly = r$income_monthly)
      },
      res = 96,
      height = 560,
      width = function() {
        w <- session$clientData[[paste0(
          "output_",
          ns("revenue_plot"),
          "_width"
        )]]
        max(w %||% 700L, 300L)
      }
    )

    output$revenue_interpretation <- renderUI({
      req(adj_revenue())
      HTML(interpret_revenue(
        adj_revenue(),
        r$practice_name,
        panel_size = r$panel_size,
        membership_fee = r$membership_fee,
        membership_tiers = r$membership_tiers,
        confidence_level = input$confidence
      ))
    })

    # -- Income Target UI -----------------------------------------------------
    output$target_ui <- renderUI({
      res <- adj_target()
      if (is.null(res)) {
        return(p(
          class = "text-muted",
          "Click 'Run Forecast' to generate results."
        ))
      }

      tgt_sustained <- target_is_sustained(res)
      tgt_already <- isTRUE(res$current_gap >= 0)

      tgt_value <- if (tgt_already && isFALSE(tgt_sustained)) {
        "Achieved (at risk)"
      } else if (tgt_already) {
        "Achieved"
      } else if (is.na(res$target_date)) {
        "Not in horizon"
      } else {
        format(res$target_date, "%b %Y")
      }

      tgt_theme <- if (tgt_already && isFALSE(tgt_sustained)) {
        "warning"
      } else if (tgt_already) {
        "success"
      } else if (is.na(res$target_date)) {
        "danger"
      } else {
        "success"
      }

      tgt_boxes <- list(
        value_box(
          title = "Target reached",
          value = shrink_value(tgt_value),
          theme = tgt_theme,
          height = "90px"
        ),
        value_box(
          title = "Revenue needed now",
          value = fmt_dollar(res$required_revenue_now),
          theme = "secondary",
          height = "90px"
        ),
        value_box(
          title = "Current gap",
          value = fmt_dollar(res$current_gap),
          theme = if (isTRUE(res$current_gap >= 0)) "success" else "warning",
          height = "90px"
        )
      )
      if (profile_ok()) {
        # Divide required revenue ($/period) by fee per period (not monthly fee)
        # to get a panel-size figure in consistent units.
        members_needed <- ceiling(res$required_revenue_now / fee_per_period())
        members_gap <- members_needed - proj_total_members()
        tgt_boxes <- c(
          tgt_boxes,
          list(
            value_box(
              title = "Members needed",
              value = members_needed,
              theme = if (members_gap <= 0) "success" else "info",
              height = "90px"
            )
          )
        )
      }

      tagList(
        do.call(
          layout_column_wrap,
          c(list(width = "200px", fill = FALSE), tgt_boxes)
        ),
        div(
          style = "max-height: 620px; overflow-y: auto; padding-right: 2px;",
          card(
            full_screen = TRUE,
            card_header("Income Target Forecast"),
            card_body(plotOutput(
              ns("target_plot"),
              height = "560px",
              width = "100%"
            ))
          ),
          card(
            card_header("Interpretation"),
            card_body(uiOutput(ns("target_interpretation")))
          )
        ),
      )
    })

    output$target_plot <- renderPlot(
      {
        r$dark_mode # dependency only -- forces a redraw when the theme toggles
        req(adj_target())
        plot_forecast_target(adj_target(), income_monthly = r$income_monthly)
      },
      res = 96,
      height = 560,
      width = function() {
        w <- session$clientData[[paste0(
          "output_",
          ns("target_plot"),
          "_width"
        )]]
        max(w %||% 700L, 300L)
      }
    )

    output$target_interpretation <- renderUI({
      req(adj_target())
      tagList(
        HTML(interpret_target(
          adj_target(),
          r$practice_name,
          input$target_income,
          sustained = target_is_sustained(adj_target()),
          panel_size = r$panel_size,
          membership_fee = r$membership_fee,
          membership_tiers = r$membership_tiers,
          goal_seek = target_goal_seek()
        )),
        if (!.has_pro_plan(r$plan_tier) && is.na(adj_target()$target_date)) {
          .pro_upsell_note(
            ns,
            "Pro also tells you what growth-rate change -- in income, overhead, or both -- would reach your income target within your forecast horizon.",
            btn_id = "btn_see_plans_pro_target_goal_seek"
          )
        }
      )
    })

    # Reactives built with eventReactive()/req() throw a silent
    # "shiny.silent.error" condition when called before their trigger has
    # fired (e.g. target_result() before the user has run the forecast from
    # the Income Target sub-tab) -- is.null() does NOT catch that, it just
    # propagates. Resolving each forecast through this helper turns "not run
    # yet" into a plain NULL so the rest of the report/download logic (which
    # already NULL-checks each section) works instead of crashing.
    .safe_result <- function(expr) {
      tryCatch(expr, shiny.silent.error = function(e) NULL)
    }

    # -- Sidebar download button (appears once any forecast has been run) ------
    output$download_ui <- renderUI({
      have <- c(
        "Break-even" = !is.null(.safe_result(adj_breakeven())),
        "Revenue Forecast" = !is.null(.safe_result(adj_revenue())),
        "Income Target" = !is.null(.safe_result(adj_target()))
      )
      if (!any(have)) {
        return(NULL)
      }

      missing_names <- names(have)[!have]
      tagList(
        downloadButton(
          ns("dl_report"),
          tagList(bs_icon("file-earmark-pdf"), " Download Report"),
          class = "btn-outline-secondary w-100 mt-2"
        ),
        if (length(missing_names) > 0L) {
          tags$p(
            class = "small text-muted mt-1 mb-0",
            bs_icon("info-circle"),
            " Report will not include: ",
            paste(missing_names, collapse = ", "),
            " (run that forecast first to include it)."
          )
        }
      )
    })

    # -- Comprehensive PDF report handler -------------------------------------
    output$dl_report <- downloadHandler(
      filename = function() {
        # Slugified practice_name, not practice_id -- practice_id is now
        # just the DB's raw serial id (see app_server.R), which would make
        # an ugly filename ("dpc-report-3-...") rather than a friendly one.
        safe_name <- .slugify(r$practice_name %||% "practice")
        paste0(
          "dpc-report-",
          safe_name,
          "-",
          format(Sys.Date(), "%Y%m%d"),
          ".pdf"
        )
      },
      content = function(file) {
        bkevn_res <- .safe_result(adj_breakeven())
        rev_res <- .safe_result(adj_revenue())
        tgt_res <- .safe_result(adj_target())

        # Collect interpretation text (HTML \u2192 plain text handled inside build_report_data)
        bkevn_text <- if (!is.null(bkevn_res)) {
          interpret_breakeven(
            bkevn_res,
            r$practice_name,
            sustained = breakeven_is_sustained(bkevn_res),
            panel_size = r$panel_size,
            membership_fee = r$membership_fee,
            membership_tiers = r$membership_tiers,
            confidence_level = input$confidence
          )
        } else {
          NULL
        }

        rev_text <- if (!is.null(rev_res)) {
          interpret_revenue(
            rev_res,
            r$practice_name,
            panel_size = r$panel_size,
            membership_fee = r$membership_fee,
            membership_tiers = r$membership_tiers,
            confidence_level = input$confidence
          )
        } else {
          NULL
        }

        tgt_text <- if (!is.null(tgt_res)) {
          interpret_target(
            tgt_res,
            r$practice_name,
            input$target_income,
            sustained = target_is_sustained(tgt_res),
            panel_size = r$panel_size,
            membership_fee = r$membership_fee,
            membership_tiers = r$membership_tiers
          )
        } else {
          NULL
        }

        data <- build_report_data(
          r = r,
          inputs = list(
            method = input$method,
            horizon = input$horizon,
            confidence = input$confidence,
            target_income = input$target_income
          ),
          breakeven_res = bkevn_res,
          revenue_res = rev_res,
          target_res = tgt_res,
          interpret_bkevn = bkevn_text,
          interpret_rev = rev_text,
          interpret_tgt = tgt_text
        )

        withProgress(message = "Generating PDF report\u2026", value = 0.5, {
          render_report_pdf(
            data,
            file,
            breakeven_res = bkevn_res,
            revenue_res = rev_res,
            target_res = tgt_res,
            income_monthly = r$income_monthly,
            overhead_monthly = r$overhead_monthly
          )
        })
      }
    )

    observeEvent(input$btn_back_to_summary, {
      updateNavbarPage(
        parent_session %||% session,
        "main_nav",
        selected = "summary"
      )
    })

    list(
      get_bundle_for_save = get_bundle_for_save,
      get_dirty_signal = scenario_dirty_signal,
      extract_dirty_signal = scenario_extract_dirty_signal,
      on_load = scenario_on_load,
      nav_footer = nav_footer
    )
  })
}
