#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @param res_auth The `reactiveValues` returned by
#'   `shinymanager::secure_server()`, holding the logged-in practice's
#'   `user_info` (practice_id, practice_name, email). `NULL` outside of
#'   `run_app()` (e.g. in tests), in which case the account menu renders
#'   without a practice name.
#' @import shiny
#' @noRd
app_server <- function(input, output, session, res_auth = NULL) {
  # -- Account menu: practice name + logout, top-right of the navbar --------
  # id = ".shinymanager_logout" is not a namespacing choice -- it's the
  # exact input id shinymanager::secure_server() listens for internally
  # (see fab_button()'s usage inside shinymanager::secure_app()). Reusing it
  # here lets this link trigger the same logout logic as the default
  # floating button, which run_app() disables via fab_position = "none".
  output$account_menu <- renderUI({
    practice_name <- res_auth$practice_name
    tags$span(
      class = "d-flex align-items-center gap-2",
      if (!is.null(practice_name) && nzchar(practice_name)) {
        tags$span(class = "text-light small", practice_name)
      },
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
  mod_results_server("results", r, parent_session = session)
}
