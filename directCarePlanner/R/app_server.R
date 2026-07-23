#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  thematic::thematic_shiny()

  # Shared state, populated by mod_plan_inputs on a successful "Build My
  # Plan" submission and read by mod_results. No getter/setter
  # abstraction -- modules read/write fields directly, matching
  # directCareAnalytics's convention.
  r <- reactiveValues(
    practice_name = NULL,
    horizon_months = NULL,
    market_context = NULL, # dcPlanR_market_context, from build_market_context()
    revenue = NULL, # dcPlanR_revenue, from calc_mixed_revenue()
    projections = NULL, # dcPlanR_scenario_projection tibble, from project_scenarios()
    capital = NULL, # list(startup_costs = , personal_runway = )
    interpretations = NULL # list(revenue = , projection = , capital = ), plain text
  )

  mod_plan_inputs_server("plan_inputs", r, parent_session = session)
  mod_results_server("results", r)
}
