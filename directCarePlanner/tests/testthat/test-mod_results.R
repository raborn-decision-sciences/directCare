# Unit tests for mod_results_server (via testServer)

fixture_populated_r <- function() {
  revenue <- directCarePlanR::calc_mixed_revenue(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 3),
    horizon_months = 3
  )
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 3),
    overhead_monthly = 5000
  )
  projections <- directCarePlanR::project_scenarios(assumptions, horizon_months = 3)
  startup_costs <- directCarePlanR::calc_startup_costs(c(equipment = 5000, ehr_setup = 8000))
  personal_runway <- directCarePlanR::calc_personal_runway(monthly_expenses = 4000, months_coverage = 6)

  reactiveValues(
    practice_name = "Test Practice",
    # Represents a normal, fully-functional practice for tests that
    # aren't specifically exercising the paywall gate itself (see
    # "gates Market Context and Download Report for a free-tier
    # practice" below for that case).
    plan_tier = "pro",
    horizon_months = 3,
    market_context = list(
      geography = list(county_name = "Fulton County", state_abb = "GA", metro_fips = "12060"),
      population_income = list(population = 1000000L, median_household_income = 75000),
      uninsured = list(uninsured_rate = 0.1),
      physician_density = list(physician_density_per_10k = 40),
      landscape = data.frame(county_fips = character(0))
    ),
    revenue = revenue,
    projections = projections,
    capital = list(startup_costs = startup_costs, personal_runway = personal_runway),
    interpretations = list(
      revenue = directCarePlanR::interpret_revenue(revenue),
      projection = directCarePlanR::interpret_projection(projections),
      capital = directCarePlanR::interpret_capital(startup_costs, personal_runway)
    )
  )
}

test_that("shows an empty-state prompt when no plan has been built", {
  r <- reactiveValues(projections = NULL)

  testServer(mod_results_server, args = list(r = r), {
    html <- paste(as.character(output$content), collapse = "")
    expect_true(grepl("Build a plan", html))
  })
})

test_that("renders results content once a plan has been built", {
  r <- fixture_populated_r()

  testServer(mod_results_server, args = list(r = r), {
    html <- paste(as.character(output$content), collapse = "")
    expect_true(grepl("Market Context", html))
    expect_true(grepl("Fulton County", html))
    expect_true(grepl("Scenario Projections", html))
    expect_true(grepl("Capital Requirements", html))
    expect_true(grepl("Interpretation", html))
  })
})

test_that("gates Market Context and Download Report for a free-tier practice", {
  r <- fixture_populated_r()
  r$plan_tier <- "free"

  testServer(mod_results_server, args = list(r = r), {
    html <- paste(as.character(output$content), collapse = "")
    expect_false(grepl("Fulton County", html))
    expect_true(grepl("Starter or Pro plan required", html))

    footer_html <- paste(as.character(nav_footer()), collapse = "")
    # "Unlock Download Report" itself contains "Download Report", so check
    # the actual element rendered rather than that substring: the real
    # download link's id ("...-dl_report") should be absent, and the
    # gated trigger's should be present.
    expect_false(grepl("dl_report", footer_html, fixed = TRUE))
    expect_true(grepl("btn_see_plans_report", footer_html, fixed = TRUE))
    # Same swap for the Save/Load scenario-slots widget.
    expect_false(grepl("scenario-save_click", footer_html, fixed = TRUE))
    expect_true(grepl("btn_see_plans_scenario", footer_html, fixed = TRUE))
  })
})

test_that(".paragraphs_to_html() splits on blank lines and wraps each in <p>", {
  html <- paste(as.character(.paragraphs_to_html("First paragraph.\n\nSecond paragraph.")), collapse = "")
  expect_true(grepl("<p>First paragraph.</p>", html, fixed = TRUE))
  expect_true(grepl("<p>Second paragraph.</p>", html, fixed = TRUE))
})

test_that(".paragraphs_to_html() returns NULL for empty input", {
  expect_null(.paragraphs_to_html(NULL))
  expect_null(.paragraphs_to_html(""))
})

# ── .humanize_cost_items() (startup-cost category label overrides) ──────

test_that(".humanize_cost_items returns default labels with no overrides", {
  expect_equal(.humanize_cost_items("ehr_setup"), "EHR setup")
  expect_equal(.humanize_cost_items("equipment"), "Equipment")
})

test_that(".humanize_cost_items applies overrides and falls back for unmapped keys", {
  overrides <- c(ehr_setup = "Practice Software")
  expect_equal(.humanize_cost_items("ehr_setup", overrides), "Practice Software")
  expect_equal(.humanize_cost_items("equipment", overrides), "Equipment")
  expect_equal(.humanize_cost_items("unknown_slug", overrides), "unknown_slug")
})

