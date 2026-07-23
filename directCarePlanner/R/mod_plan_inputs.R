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

  state_choices <- c(
    "(auto-detect)" = "",
    stats::setNames(c(datasets::state.abb, "DC"), c(datasets::state.name, "District of Columbia"))
  )

  tagList(
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      fillable = FALSE,
      card(
        card_header(bsicons::bs_icon("geo-alt"), " Location"),
        card_body(
          textInput(ns("location"), "ZIP code or county name", value = "30309"),
          selectInput(ns("state"), "State (to disambiguate a county name)", choices = state_choices)
        )
      ),
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
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      fillable = FALSE,
      card(
        card_header(bsicons::bs_icon("receipt"), " Overhead"),
        card_body(
          numericInput(ns("overhead_monthly"), "Monthly overhead ($)", value = 12000, min = 0, step = 100)
        )
      ),
      card(
        card_header(bsicons::bs_icon("piggy-bank"), " Capital Requirements"),
        card_body(
          tags$p(class = "text-muted small", "One-time startup costs"),
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            fillable = FALSE,
            numericInput(ns("cost_ehr"), "EHR setup ($)", value = 8000, min = 0, step = 100),
            numericInput(ns("cost_equipment"), "Equipment ($)", value = 5000, min = 0, step = 100),
            numericInput(ns("cost_licensing"), "Licensing ($)", value = 1500, min = 0, step = 100),
            numericInput(ns("cost_marketing"), "Marketing ($)", value = 3000, min = 0, step = 100)
          ),
          numericInput(ns("cost_other"), "Other startup costs ($)", value = 0, min = 0, step = 100),
          tags$p(class = "text-muted small mt-3", "Personal runway"),
          numericInput(ns("monthly_expenses"), "Monthly living expenses ($)", value = 5000, min = 0, step = 100),
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
    )
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
    observeEvent(input$submit, {
      location <- trimws(input$location)
      if (!nzchar(location)) {
        showNotification("Enter a ZIP code or county name.", type = "error", duration = 8)
        return(invisible(NULL))
      }
      state <- if (nzchar(input$state)) input$state else NULL

      market_context <- run_plan(directCarePlanR::build_market_context(location, state = state))
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
        overhead_monthly = input$overhead_monthly,
        overhead_growth_rate = input$overhead_growth_rate / 100
      )
      projections <- run_plan(directCarePlanR::project_scenarios(assumptions, horizon_months = input$horizon_months))
      if (is.null(projections)) {
        return(invisible(NULL))
      }

      startup_costs <- run_plan(directCarePlanR::calc_startup_costs(c(
        ehr_setup = input$cost_ehr,
        equipment = input$cost_equipment,
        licensing = input$cost_licensing,
        marketing = input$cost_marketing,
        other = input$cost_other
      )))
      personal_runway <- run_plan(
        directCarePlanR::calc_personal_runway(input$monthly_expenses, input$months_coverage)
      )
      if (is.null(startup_costs) || is.null(personal_runway)) {
        return(invisible(NULL))
      }

      interpretations <- list(
        revenue = directCarePlanR::interpret_revenue(revenue),
        projection = directCarePlanR::interpret_projection(projections),
        capital = directCarePlanR::interpret_capital(startup_costs, personal_runway)
      )

      r$practice_name <- paste0(market_context$geography$county_name, ", ", market_context$geography$state_abb, " Practice")
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
