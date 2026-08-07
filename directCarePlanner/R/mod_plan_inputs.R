#' plan_inputs UI Function
#'
#' Assumption-collection form: location, revenue model (membership and/or
#' fee-for-service), overhead, capital requirements, and projection
#' horizon. Submitting runs the full directCarePlanR pipeline and stores
#' the results in the shared reactive state for the Results tab.
#'
#' @param id Internal parameter for `{shiny}`.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plan_inputs_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      fillable = FALSE,
      card(
        card_header(bsicons::bs_icon("geo-alt"), " Location"),
        card_body(
          htmltools::tagQuery(
            textInput(ns("zip"), "ZIP code", value = "30309")
          )$find("input")$addAttrs(autocomplete = "off")$allTags()
        )
      ),
      card(
        card_header(bsicons::bs_icon("receipt"), " Overhead"),
        card_body(
          radioButtons(
            ns("overhead_mode"),
            NULL,
            choices = c("Itemized by category" = "itemized", "Single total" = "single"),
            selected = "single",
            inline = TRUE
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'single'", ns("overhead_mode")),
            numericInput(ns("overhead_monthly"), "Monthly overhead ($)", value = 12000, min = 0, step = 100)
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'itemized'", ns("overhead_mode")),
            numericInput(ns("overhead_rent"), "Rent & Utilities ($)", value = 0, min = 0, step = 50),
            numericInput(ns("overhead_staff"), "Staff & Payroll ($)", value = 0, min = 0, step = 100),
            numericInput(ns("overhead_ehr"), "EHR & Software ($)", value = 0, min = 0, step = 10),
            numericInput(ns("overhead_malpractice"), "Malpractice Insurance ($)", value = 0, min = 0, step = 10),
            numericInput(ns("overhead_supplies"), "Supplies & Labs ($)", value = 0, min = 0, step = 10),
            numericInput(ns("overhead_other"), "Other Overhead ($)", value = 0, min = 0, step = 10),
            uiOutput(ns("overhead_total_ui"))
          )
        )
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      fillable = FALSE,
      card(
        card_header(bsicons::bs_icon("cash-coin"), " Revenue Model"),
        card_body(
          checkboxInput(ns("include_membership"), "Include membership revenue", value = TRUE),
          conditionalPanel(
            condition = sprintf("input['%s']", ns("include_membership")),
            numericInput(ns("panel_size"), "Target panel size (members)", value = 300, min = 0, step = 10),
            numericInput(ns("fee"), "Monthly membership fee ($/member)", value = 100, min = 0, step = 5),
            numericInput(ns("ramp_months"), "Months to reach target panel size", value = 12, min = 1, step = 1),
            selectInput(
              ns("ramp_shape"),
              "Ramp shape",
              choices = c("Linear" = "linear", "S-curve" = "s_curve")
            )
          ),
          tags$hr(),
          checkboxInput(ns("include_fee"), "Include fee-for-service revenue", value = FALSE),
          conditionalPanel(
            condition = sprintf("input['%s']", ns("include_fee")),
            numericInput(ns("visit_volume"), "Target visits per month", value = 100, min = 0, step = 10),
            numericInput(ns("new_visit_fee"), "New-patient visit fee ($)", value = 200, min = 0, step = 5),
            numericInput(ns("follow_up_fee"), "Follow-up visit fee ($)", value = 100, min = 0, step = 5),
            numericInput(ns("new_visit_pct"), "Share of visits that are new-patient (%)", value = 20, min = 0, max = 100, step = 1),
            numericInput(ns("fee_ramp_months"), "Months to reach target visit volume", value = 12, min = 1, step = 1),
            selectInput(
              ns("fee_ramp_shape"),
              "Ramp shape",
              choices = c("Linear" = "linear", "S-curve" = "s_curve")
            )
          )
        )
      ),
      card(
        card_header(bsicons::bs_icon("piggy-bank"), " Capital Requirements"),
        card_body(
          tags$p(class = "text-muted small", "One-time startup costs"),
          radioButtons(
            ns("startup_mode"),
            NULL,
            choices = c("Itemized by category" = "itemized", "Single total" = "single"),
            selected = "itemized",
            inline = TRUE
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'single'", ns("startup_mode")),
            numericInput(ns("cost_total"), "Total startup costs ($)", value = 17500, min = 0, step = 100)
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'itemized'", ns("startup_mode")),
            layout_columns(
              col_widths = c(6, 6),
              fill = FALSE,
              fillable = FALSE,
              numericInput(ns("cost_ehr"), "EHR setup ($)", value = 8000, min = 0, step = 100),
              numericInput(ns("cost_equipment"), "Equipment ($)", value = 5000, min = 0, step = 100),
              numericInput(ns("cost_licensing"), "Licensing ($)", value = 1500, min = 0, step = 100),
              numericInput(ns("cost_marketing"), "Marketing ($)", value = 3000, min = 0, step = 100)
            ),
            numericInput(ns("cost_other"), "Other startup costs ($)", value = 0, min = 0, step = 100)
          ),
          tags$p(class = "text-muted small mt-3", "Personal runway"),
          radioButtons(
            ns("runway_mode"),
            NULL,
            choices = c("Itemized by category" = "itemized", "Single total" = "single"),
            selected = "single",
            inline = TRUE
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'single'", ns("runway_mode")),
            numericInput(ns("monthly_expenses"), "Monthly living expenses ($)", value = 5000, min = 0, step = 100)
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'itemized'", ns("runway_mode")),
            numericInput(ns("runway_housing"), "Housing ($)", value = 0, min = 0, step = 50),
            numericInput(ns("runway_utilities"), "Utilities ($)", value = 0, min = 0, step = 25),
            numericInput(ns("runway_food"), "Food & Groceries ($)", value = 0, min = 0, step = 25),
            numericInput(ns("runway_insurance"), "Insurance ($)", value = 0, min = 0, step = 25),
            numericInput(ns("runway_debt"), "Debt Payments ($)", value = 0, min = 0, step = 25),
            numericInput(ns("runway_other"), "Other ($)", value = 0, min = 0, step = 25),
            uiOutput(ns("runway_total_ui"))
          ),
          numericInput(ns("months_coverage"), "Months of coverage needed", value = 6, min = 1, step = 1),
          accordion(
            open = FALSE,
            class = "mt-3",
            accordion_panel(
              "Customize category labels",
              icon = bsicons::bs_icon("tags"),
              tags$p(
                class = "text-muted small",
                "Rename how startup cost items appear in the Results table",
                " and the downloaded report -- the underlying numbers are unchanged."
              ),
              uiOutput(ns("cost_label_editor_ui")),
              actionButton(
                ns("btn_save_cost_labels"),
                "Save Labels",
                icon = bsicons::bs_icon("check2"),
                class = "btn-secondary btn-sm w-100 mt-1"
              )
            )
          )
        )
      )
    ),
    accordion(
      open = FALSE,
      accordion_panel(
        "Advanced options",
        icon = bsicons::bs_icon("sliders"),
        numericInput(ns("horizon_months"), "Projection horizon (months)", value = 24, min = 1, step = 1),
        numericInput(ns("overhead_growth_rate"), "Monthly overhead growth rate (%)", value = 0, step = 0.1)
      )
    ),
    # No mod_scenario_banner_ui()/mod_scenario_slots_ui() call here -- both
    # are rendered exactly once, globally, via page_navbar(header = ...) in
    # app_ui.R, the same fix already applied to directCareAnalytics (see its
    # own app_ui.R for the full duplicate-DOM-id rationale this avoids). The
    # "Build My Plan" submit button lives in the same central nav bar now
    # too -- see this module's own `nav_footer` return value.
    .branded_footer()
  )
}

#' plan_inputs Server Functions
#'
#' @param id Internal parameter for `{shiny}`.
#' @param r Shared reactiveValues object.
#' @param parent_session The top-level session, used to switch the active
#'   navbar tab to Results after a successful submission.
#'
#' @noRd
mod_plan_inputs_server <- function(id, r, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # .nn0()/.overhead_total_from() now live in utils_globals.R (shared with
    # mod_results.R's .assumptions_from_saved_inputs(), which needs
    # identical overhead-total logic against a saved scenario's inputs) --
    # pulled out of this closure so .build_plan() can call them against a
    # plain list (a just-loaded scenario's saved inputs) instead of the
    # live `input` just as before, but from a shared location instead of a
    # module-local duplicate. .runway_total_from() has no use outside this
    # module, so it stays here.
    .runway_total_from <- function(vals) {
      if (identical(vals$runway_mode, "single")) {
        .nn0(vals$monthly_expenses)
      } else {
        sum(
          c(
            .nn0(vals$runway_housing),
            .nn0(vals$runway_utilities),
            .nn0(vals$runway_food),
            .nn0(vals$runway_insurance),
            .nn0(vals$runway_debt),
            .nn0(vals$runway_other)
          )
        )
      }
    }

    overhead_total_r <- reactive({
      .overhead_total_from(list(
        overhead_mode = input$overhead_mode,
        overhead_monthly = input$overhead_monthly,
        overhead_rent = input$overhead_rent,
        overhead_staff = input$overhead_staff,
        overhead_ehr = input$overhead_ehr,
        overhead_malpractice = input$overhead_malpractice,
        overhead_supplies = input$overhead_supplies,
        overhead_other = input$overhead_other
      ))
    })

    output$overhead_total_ui <- renderUI({
      tags$div(
        class = "border-top pt-2 mt-1 small fw-semibold",
        "Monthly overhead total: ", .fmt_dollar(overhead_total_r())
      )
    })

    runway_total_r <- reactive({
      .runway_total_from(list(
        runway_mode = input$runway_mode,
        monthly_expenses = input$monthly_expenses,
        runway_housing = input$runway_housing,
        runway_utilities = input$runway_utilities,
        runway_food = input$runway_food,
        runway_insurance = input$runway_insurance,
        runway_debt = input$runway_debt,
        runway_other = input$runway_other
      ))
    })

    output$runway_total_ui <- renderUI({
      tags$div(
        class = "border-top pt-2 mt-1 small fw-semibold",
        "Monthly living expenses total: ", .fmt_dollar(runway_total_r())
      )
    })

    # -- Customize startup-cost item display labels ----------------------
    # .cost_item_labels / .humanize_cost_items() live in mod_results.R
    # (same package namespace, no import needed) -- this is the one place
    # Planner has a slug -> display-label mapping that actually reaches a
    # downstream table/report, matching directCareAnalytics's
    # .cat_labels/.pretty_cat pattern for its own "Customize Category
    # Labels" editor. Overhead/personal-runway itemized inputs have no
    # equivalent: both collapse to a single total before .build_plan()
    # ever calls the directCarePlanR package, so there's no per-category
    # display anywhere downstream to rename.
    output$cost_label_editor_ui <- renderUI({
      slugs <- names(.cost_item_labels)
      cur <- r$cost_item_labels %||% .cost_item_labels
      tagList(
        lapply(slugs, function(slug) {
          default <- .cost_item_labels[[slug]]
          textInput(
            ns(paste0("cost_label_", slug)),
            default,
            value = isolate(input[[paste0("cost_label_", slug)]]) %||%
              (if (slug %in% names(cur)) cur[[slug]] else default),
            placeholder = default
          )
        })
      )
    })

    observeEvent(input$btn_save_cost_labels, {
      slugs <- names(.cost_item_labels)
      new_labels <- stats::setNames(
        vapply(
          slugs,
          function(slug) {
            v <- input[[paste0("cost_label_", slug)]] %||% ""
            if (nzchar(trimws(v))) v else .cost_item_labels[[slug]]
          },
          character(1)
        ),
        slugs
      )
      r$cost_item_labels <- new_labels
      showNotification("Category labels updated.", type = "message", duration = 2)
    })

    # Extracted from the submit button's own observer so a loaded scenario
    # (see the saved-scenario wiring below) can re-run the exact same
    # pipeline against its restored inputs, without duplicating this logic.
    # Planner scenarios save inputs only, not a results snapshot (unlike
    # DCA's forecast scenarios) -- see SAVED_SCENARIOS.md -- so loading one
    # always means "recompute from these inputs," exactly what this
    # function already does for a live submission.
    #
    # `values`, when supplied, is a plain named list (the exact shape
    # plan_inputs_for_save() produces) used instead of reading `input$x`.
    # This is what a scenario load passes in -- reading `input$x` right
    # after update*Input() calls would race the client: those calls only
    # take effect once the browser echoes the new value back over the
    # websocket, and session$onFlushed() fires on the *server's* own flush,
    # well before that round trip completes (confirmed live: without this,
    # a loaded scenario silently recomputed against the stale/default
    # input values instead of the ones just loaded). Passing the freshly
    # loaded list directly sidesteps the race instead of racing it.
    .build_plan <- function(switch_tab = TRUE, values = NULL) {
      iv <- function(name) if (!is.null(values)) values[[name]] else input[[name]]

      zip <- trimws(iv("zip"))
      if (!grepl("^[0-9]{5}$", zip)) {
        showNotification("Enter a 5-digit ZIP code.", type = "error", duration = 8)
        return(invisible(NULL))
      }

      # No `state` arg -- directCarePlanR::build_market_context()/
      # resolve_geography() only ever consult it to disambiguate an
      # ambiguous *county name*, which this ZIP-only field no longer
      # accepts; a 5-digit ZIP is always sufficient on its own. The
      # underlying package function keeps its more generic
      # location/state parameters, since it's a shared, non-user-facing
      # calculation layer -- only this Shiny-facing input narrowed.
      market_context <- run_plan(directCarePlanR::build_market_context(zip))
      if (is.null(market_context)) {
        return(invisible(NULL))
      }

      membership_args <- if (isTRUE(iv("include_membership"))) {
        list(
          panel_size = iv("panel_size"),
          fee = iv("fee"),
          ramp_months = iv("ramp_months"),
          ramp_shape = iv("ramp_shape")
        )
      }
      fee_args <- if (isTRUE(iv("include_fee"))) {
        list(
          visit_volume = iv("visit_volume"),
          new_visit_fee = iv("new_visit_fee"),
          follow_up_fee = iv("follow_up_fee"),
          new_visit_pct = iv("new_visit_pct") / 100,
          # %||% 1 (not the UI's own default of 12) specifically covers a
          # scenario saved before fee-for-service ramp existed: `values`
          # (a just-loaded scenario, see iv() above) won't have this key at
          # all, and ramp_months = 1 reproduces the old flat-from-month-1
          # behavior those scenarios were actually built with, rather than
          # silently reinterpreting them under a 12-month ramp they never
          # had.
          ramp_months = iv("fee_ramp_months") %||% 1L,
          ramp_shape = iv("fee_ramp_shape") %||% "linear"
        )
      }

      revenue <- run_plan(
        directCarePlanR::calc_mixed_revenue(membership_args, fee_args, horizon_months = iv("horizon_months"))
      )
      if (is.null(revenue)) {
        return(invisible(NULL))
      }

      overhead_monthly <- if (!is.null(values)) .overhead_total_from(values) else overhead_total_r()
      assumptions <- list(
        membership_args = membership_args,
        fee_args = fee_args,
        overhead_monthly = overhead_monthly,
        overhead_growth_rate = iv("overhead_growth_rate") / 100
      )
      projections <- run_plan(directCarePlanR::project_scenarios(assumptions, horizon_months = iv("horizon_months")))
      if (is.null(projections)) {
        return(invisible(NULL))
      }

      startup_costs <- if (identical(iv("startup_mode"), "single")) {
        run_plan(directCarePlanR::calc_startup_costs(c(total = iv("cost_total"))))
      } else {
        run_plan(directCarePlanR::calc_startup_costs(c(
          ehr_setup = iv("cost_ehr"),
          equipment = iv("cost_equipment"),
          licensing = iv("cost_licensing"),
          marketing = iv("cost_marketing"),
          other = iv("cost_other")
        )))
      }
      runway_monthly <- if (!is.null(values)) .runway_total_from(values) else runway_total_r()
      personal_runway <- run_plan(
        directCarePlanR::calc_personal_runway(runway_monthly, iv("months_coverage"))
      )
      if (is.null(startup_costs) || is.null(personal_runway)) {
        return(invisible(NULL))
      }

      # Pro-exclusive: which assumption (ramp speed vs. overhead) drives
      # more of the conservative-to-optimistic spread. Computed here (not
      # inside interpret_projection() itself) because it needs the raw
      # `assumptions` list, which callers of that function don't otherwise
      # have to supply. run_plan() swallows and notifies on any
      # dcPlanR_* error the same as the calls above; a failure here
      # degrades to the plain 3-paragraph interpretation, not a crash.
      sensitivity_decomposition <- if (.has_pro_plan(r$plan_tier)) {
        run_plan(directCarePlanR::decompose_projection_sensitivity(
          assumptions,
          horizon_months = iv("horizon_months")
        ))
      } else {
        NULL
      }

      # Pro-exclusive: what it would take (overhead cut and/or a faster
      # ramp) for the base scenario to recover within the horizon, when it
      # currently doesn't. Only computed in that case -- interpret_projection()
      # ignores a goal_seek argument when the base scenario already
      # recovers, so skipping the (cheap but pointless) computation here
      # just saves the wasted binary search.
      base_recovers <- {
        base_proj <- projections[projections$scenario == "base", ]
        any(base_proj$cumulative_net_income >= 0)
      }
      goal_seek <- if (.has_pro_plan(r$plan_tier) && !base_recovers) {
        run_plan(directCarePlanR::goal_seek_projection_recovery(
          assumptions,
          horizon_months = iv("horizon_months")
        ))
      } else {
        NULL
      }

      interpretations <- list(
        revenue = directCarePlanR::interpret_revenue(revenue),
        projection = directCarePlanR::interpret_projection(
          projections,
          sensitivity_decomposition,
          goal_seek
        ),
        capital = directCarePlanR::interpret_capital(startup_costs, personal_runway)
      )

      # r$practice_name is not set here -- it's sourced once from
      # res_auth in app_server.R (the logged-in account's practice name),
      # not re-derived per submission.
      r$horizon_months <- iv("horizon_months")
      r$market_context <- market_context
      r$revenue <- revenue
      r$projections <- projections
      r$capital <- list(startup_costs = startup_costs, personal_runway = personal_runway)
      r$interpretations <- interpretations

      if (switch_tab) {
        updateNavbarPage(parent_session %||% session, "main_nav", selected = "results")
      }
    }

    observeEvent(input$submit, {
      .build_plan()
    })

    # -- Saved scenario slots (plan_scenarios) ---------------------------------
    plan_inputs_for_save <- function() {
      list(
        zip = input$zip,
        overhead_mode = input$overhead_mode,
        overhead_monthly = input$overhead_monthly,
        overhead_rent = input$overhead_rent,
        overhead_staff = input$overhead_staff,
        overhead_ehr = input$overhead_ehr,
        overhead_malpractice = input$overhead_malpractice,
        overhead_supplies = input$overhead_supplies,
        overhead_other = input$overhead_other,
        include_membership = isTRUE(input$include_membership),
        panel_size = input$panel_size,
        fee = input$fee,
        ramp_months = input$ramp_months,
        ramp_shape = input$ramp_shape,
        include_fee = isTRUE(input$include_fee),
        visit_volume = input$visit_volume,
        new_visit_fee = input$new_visit_fee,
        follow_up_fee = input$follow_up_fee,
        new_visit_pct = input$new_visit_pct,
        fee_ramp_months = input$fee_ramp_months,
        fee_ramp_shape = input$fee_ramp_shape,
        startup_mode = input$startup_mode,
        cost_total = input$cost_total,
        cost_ehr = input$cost_ehr,
        cost_equipment = input$cost_equipment,
        cost_licensing = input$cost_licensing,
        cost_marketing = input$cost_marketing,
        cost_other = input$cost_other,
        runway_mode = input$runway_mode,
        monthly_expenses = input$monthly_expenses,
        runway_housing = input$runway_housing,
        runway_utilities = input$runway_utilities,
        runway_food = input$runway_food,
        runway_insurance = input$runway_insurance,
        runway_debt = input$runway_debt,
        runway_other = input$runway_other,
        months_coverage = input$months_coverage,
        horizon_months = input$horizon_months,
        overhead_growth_rate = input$overhead_growth_rate
      )
    }

    # mod_scenario_slots_server() itself is now instantiated once at the
    # app level (app_server.R), not per-tab, so Save/Load/the "viewing
    # saved scenario" banner are a single shared instance available from
    # both Plan Inputs and Results (previously Plan Inputs only) -- see
    # app_server.R's own comment, and directCareAnalytics's identical fix,
    # for the full rationale. This module just exposes the same callback
    # bodies as before, as plain closures, for app_server.R to pass into
    # that single top-level call -- each closure still correctly reads
    # *this* module's own input$xxx/session objects wherever it's
    # ultimately invoked from, since R closures capture their defining
    # environment, not their call site.
    scenario_on_load <- function(inputs) {
      updateTextInput(session, "zip", value = inputs$zip)
      updateRadioButtons(session, "overhead_mode", selected = inputs$overhead_mode)
      updateNumericInput(session, "overhead_monthly", value = inputs$overhead_monthly)
      updateNumericInput(session, "overhead_rent", value = inputs$overhead_rent)
      updateNumericInput(session, "overhead_staff", value = inputs$overhead_staff)
      updateNumericInput(session, "overhead_ehr", value = inputs$overhead_ehr)
      updateNumericInput(session, "overhead_malpractice", value = inputs$overhead_malpractice)
      updateNumericInput(session, "overhead_supplies", value = inputs$overhead_supplies)
      updateNumericInput(session, "overhead_other", value = inputs$overhead_other)
      updateCheckboxInput(session, "include_membership", value = isTRUE(inputs$include_membership))
      updateNumericInput(session, "panel_size", value = inputs$panel_size)
      updateNumericInput(session, "fee", value = inputs$fee)
      updateNumericInput(session, "ramp_months", value = inputs$ramp_months)
      updateSelectInput(session, "ramp_shape", selected = inputs$ramp_shape)
      updateCheckboxInput(session, "include_fee", value = isTRUE(inputs$include_fee))
      updateNumericInput(session, "visit_volume", value = inputs$visit_volume)
      updateNumericInput(session, "new_visit_fee", value = inputs$new_visit_fee)
      updateNumericInput(session, "follow_up_fee", value = inputs$follow_up_fee)
      updateNumericInput(session, "new_visit_pct", value = inputs$new_visit_pct)
      updateNumericInput(session, "fee_ramp_months", value = inputs$fee_ramp_months)
      updateSelectInput(session, "fee_ramp_shape", selected = inputs$fee_ramp_shape)
      updateRadioButtons(session, "startup_mode", selected = inputs$startup_mode)
      updateNumericInput(session, "cost_total", value = inputs$cost_total)
      updateNumericInput(session, "cost_ehr", value = inputs$cost_ehr)
      updateNumericInput(session, "cost_equipment", value = inputs$cost_equipment)
      updateNumericInput(session, "cost_licensing", value = inputs$cost_licensing)
      updateNumericInput(session, "cost_marketing", value = inputs$cost_marketing)
      updateNumericInput(session, "cost_other", value = inputs$cost_other)
      updateRadioButtons(session, "runway_mode", selected = inputs$runway_mode)
      updateNumericInput(session, "monthly_expenses", value = inputs$monthly_expenses)
      updateNumericInput(session, "runway_housing", value = inputs$runway_housing)
      updateNumericInput(session, "runway_utilities", value = inputs$runway_utilities)
      updateNumericInput(session, "runway_food", value = inputs$runway_food)
      updateNumericInput(session, "runway_insurance", value = inputs$runway_insurance)
      updateNumericInput(session, "runway_debt", value = inputs$runway_debt)
      updateNumericInput(session, "runway_other", value = inputs$runway_other)
      updateNumericInput(session, "months_coverage", value = inputs$months_coverage)
      updateNumericInput(session, "horizon_months", value = inputs$horizon_months)
      updateNumericInput(session, "overhead_growth_rate", value = inputs$overhead_growth_rate)

      # The update*Input() calls above are for visual sync only (so the
      # form reflects what was loaded) -- they do NOT feed the
      # recompute below. `input$x` won't reflect any of them until the
      # client echoes the new values back over the websocket, and
      # there's no server-side hook (session$onFlushed() included) that
      # waits for that round trip; it only fires once the server's own
      # reactive flush completes. Waiting on it previously caused a
      # loaded scenario to silently recompute against the stale/default
      # input values instead of the ones just loaded (confirmed live:
      # loading a saved "10001" ZIP still showed the default 30309's
      # market context). Passing `inputs` straight into .build_plan()
      # sidesteps the race entirely -- it's already the exact values
      # that were just saved, no DOM round trip required.
      .build_plan(switch_tab = FALSE, values = inputs)
    }

    # Returned for the central output$main_nav_footer (app_server.R) to
    # render when Plan Inputs is the active tab -- see app_ui.R's header
    # comment. First step in the sequence, so no back button.
    nav_footer <- reactive({
      .tour_nav_footer(
        current_step = 1L,
        forward = input_task_button(
          ns("submit"),
          "Build My Plan",
          icon = bsicons::bs_icon("arrow-right-circle")
        ),
        extra = .scenario_slots_ui("scenario", r$plan_tier)
      )
    })

    list(
      get_inputs_for_save = plan_inputs_for_save,
      get_dirty_signal = plan_inputs_for_save,
      on_load = scenario_on_load,
      nav_footer = nav_footer
    )
  })
}
