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
            numericInput(ns("visit_volume"), "Visits per month", value = 100, min = 0, step = 10),
            numericInput(ns("new_visit_fee"), "New-patient visit fee ($)", value = 200, min = 0, step = 5),
            numericInput(ns("follow_up_fee"), "Follow-up visit fee ($)", value = 100, min = 0, step = 5),
            numericInput(ns("new_visit_pct"), "Share of visits that are new-patient (%)", value = 20, min = 0, max = 100, step = 1)
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
          numericInput(ns("months_coverage"), "Months of coverage needed", value = 6, min = 1, step = 1)
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
    div(
      class = "d-flex justify-content-end mt-3 mb-4",
      input_task_button(ns("submit"), "Build My Plan", icon = bsicons::bs_icon("arrow-right-circle"))
    ),
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
    # .nn0(): coerce a potentially-NULL or NA numeric input to 0, so a
    # cleared itemized field contributes $0 to the running total instead of
    # propagating NA into it (a cleared numericInput sends NA, not NULL, so
    # %||% alone doesn't catch it).
    .nn0 <- function(x) {
      v <- x %||% 0
      if (length(v) != 1L || !is.finite(v)) 0 else v
    }

    overhead_total_r <- reactive({
      if (identical(input$overhead_mode, "single")) {
        .nn0(input$overhead_monthly)
      } else {
        sum(
          c(
            .nn0(input$overhead_rent),
            .nn0(input$overhead_staff),
            .nn0(input$overhead_ehr),
            .nn0(input$overhead_malpractice),
            .nn0(input$overhead_supplies),
            .nn0(input$overhead_other)
          )
        )
      }
    })

    output$overhead_total_ui <- renderUI({
      tags$div(
        class = "border-top pt-2 mt-1 small fw-semibold",
        "Monthly overhead total: ", .fmt_dollar(overhead_total_r())
      )
    })

    runway_total_r <- reactive({
      if (identical(input$runway_mode, "single")) {
        .nn0(input$monthly_expenses)
      } else {
        sum(
          c(
            .nn0(input$runway_housing),
            .nn0(input$runway_utilities),
            .nn0(input$runway_food),
            .nn0(input$runway_insurance),
            .nn0(input$runway_debt),
            .nn0(input$runway_other)
          )
        )
      }
    })

    output$runway_total_ui <- renderUI({
      tags$div(
        class = "border-top pt-2 mt-1 small fw-semibold",
        "Monthly living expenses total: ", .fmt_dollar(runway_total_r())
      )
    })

    observeEvent(input$submit, {
      zip <- trimws(input$zip)
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

      membership_args <- if (isTRUE(input$include_membership)) {
        list(
          panel_size = input$panel_size,
          fee = input$fee,
          ramp_months = input$ramp_months,
          ramp_shape = input$ramp_shape
        )
      }
      fee_args <- if (isTRUE(input$include_fee)) {
        list(
          visit_volume = input$visit_volume,
          new_visit_fee = input$new_visit_fee,
          follow_up_fee = input$follow_up_fee,
          new_visit_pct = input$new_visit_pct / 100
        )
      }

      revenue <- run_plan(
        directCarePlanR::calc_mixed_revenue(membership_args, fee_args, horizon_months = input$horizon_months)
      )
      if (is.null(revenue)) {
        return(invisible(NULL))
      }

      assumptions <- list(
        membership_args = membership_args,
        fee_args = fee_args,
        overhead_monthly = overhead_total_r(),
        overhead_growth_rate = input$overhead_growth_rate / 100
      )
      projections <- run_plan(directCarePlanR::project_scenarios(assumptions, horizon_months = input$horizon_months))
      if (is.null(projections)) {
        return(invisible(NULL))
      }

      startup_costs <- if (identical(input$startup_mode, "single")) {
        run_plan(directCarePlanR::calc_startup_costs(c(total = input$cost_total)))
      } else {
        run_plan(directCarePlanR::calc_startup_costs(c(
          ehr_setup = input$cost_ehr,
          equipment = input$cost_equipment,
          licensing = input$cost_licensing,
          marketing = input$cost_marketing,
          other = input$cost_other
        )))
      }
      personal_runway <- run_plan(
        directCarePlanR::calc_personal_runway(runway_total_r(), input$months_coverage)
      )
      if (is.null(startup_costs) || is.null(personal_runway)) {
        return(invisible(NULL))
      }

      interpretations <- list(
        revenue = directCarePlanR::interpret_revenue(revenue),
        projection = directCarePlanR::interpret_projection(projections),
        capital = directCarePlanR::interpret_capital(startup_costs, personal_runway)
      )

      # r$practice_name is not set here -- it's sourced once from
      # res_auth in app_server.R (the logged-in account's practice name),
      # not re-derived per submission.
      r$horizon_months <- input$horizon_months
      r$market_context <- market_context
      r$revenue <- revenue
      r$projections <- projections
      r$capital <- list(startup_costs = startup_costs, personal_runway = personal_runway)
      r$interpretations <- interpretations

      updateNavbarPage(parent_session %||% session, "main_nav", selected = "results")
    })
  })
}
