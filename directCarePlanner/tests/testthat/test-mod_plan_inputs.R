# Unit tests for mod_plan_inputs_server (via testServer)

test_that("submitting a valid plan populates shared state and navigates to Results", {
  r <- reactiveValues(
    practice_name = NULL, horizon_months = NULL, market_context = NULL,
    revenue = NULL, projections = NULL, capital = NULL, interpretations = NULL
  )

  testServer(mod_plan_inputs_server, args = list(r = r), {
    session$setInputs(
      zip = "30309",
      include_membership = TRUE,
      panel_size = 300,
      fee = 100,
      ramp_months = 12,
      ramp_shape = "linear",
      include_fee = TRUE,
      visit_volume = 100,
      new_visit_fee = 200,
      follow_up_fee = 100,
      new_visit_pct = 20,
      overhead_mode = "single",
      overhead_monthly = 12000,
      startup_mode = "itemized",
      cost_ehr = 8000,
      cost_equipment = 5000,
      cost_licensing = 1500,
      cost_marketing = 3000,
      cost_other = 0,
      runway_mode = "single",
      monthly_expenses = 5000,
      months_coverage = 6,
      horizon_months = 24,
      overhead_growth_rate = 0
    )
    session$setInputs(submit = 1)

    expect_equal(r$market_context$geography$county_fips, "13121")
    expect_s3_class(r$revenue, "dcPlanR_revenue")
    expect_s3_class(r$projections, "dcPlanR_scenario_projection")
    expect_equal(r$capital$startup_costs$total, 17500)
    expect_equal(r$capital$personal_runway$total, 30000)
    expect_type(r$interpretations$revenue, "character")
    expect_type(r$interpretations$projection, "character")
    expect_type(r$interpretations$capital, "character")
    expect_equal(r$horizon_months, 24)
  })
})

test_that("itemized overhead, single-total startup costs, and itemized runway all sum correctly", {
  # The inverse mode combination from the test above (single
  # overhead/itemized startup/single runway) -- exercises the other branch
  # of all three toggles in one pass.
  r <- reactiveValues(
    practice_name = NULL, horizon_months = NULL, market_context = NULL,
    revenue = NULL, projections = NULL, capital = NULL, interpretations = NULL
  )

  testServer(mod_plan_inputs_server, args = list(r = r), {
    session$setInputs(
      zip = "30309",
      include_membership = TRUE,
      panel_size = 300,
      fee = 100,
      ramp_months = 12,
      ramp_shape = "linear",
      include_fee = FALSE,
      overhead_mode = "itemized",
      overhead_rent = 3000,
      overhead_staff = 5000,
      overhead_ehr = 800,
      overhead_malpractice = 400,
      overhead_supplies = 300,
      overhead_other = 200,
      startup_mode = "single",
      cost_total = 20000,
      runway_mode = "itemized",
      runway_housing = 1500,
      runway_utilities = 300,
      runway_food = 600,
      runway_insurance = 400,
      runway_debt = 500,
      runway_other = 200,
      months_coverage = 4,
      horizon_months = 24,
      overhead_growth_rate = 0
    )

    expect_equal(overhead_total_r(), 9700)
    expect_equal(runway_total_r(), 3500)

    session$setInputs(submit = 1)

    expect_equal(r$capital$startup_costs$total, 20000)
    expect_equal(r$capital$startup_costs$line_items, c(total = 20000))
    expect_equal(r$capital$personal_runway$total, 14000)
    expect_equal(r$capital$personal_runway$monthly_expenses, 3500)
  })
})

test_that("a blank ZIP shows an error notification and leaves state untouched", {
  r <- reactiveValues(market_context = NULL)

  testServer(mod_plan_inputs_server, args = list(r = r), {
    session$setInputs(zip = "   ")
    session$setInputs(submit = 1)
    expect_null(r$market_context)
  })
})

test_that("a non-5-digit ZIP shows an error notification and leaves state untouched", {
  r <- reactiveValues(market_context = NULL)

  testServer(mod_plan_inputs_server, args = list(r = r), {
    session$setInputs(zip = "abc")
    session$setInputs(submit = 1)
    expect_null(r$market_context)
  })
})

test_that("an unresolvable ZIP leaves state untouched", {
  r <- reactiveValues(market_context = NULL)

  testServer(mod_plan_inputs_server, args = list(r = r), {
    session$setInputs(
      zip = "00000",
      include_membership = TRUE,
      panel_size = 300,
      fee = 100,
      ramp_months = 12,
      ramp_shape = "linear",
      include_fee = FALSE,
      overhead_mode = "single",
      overhead_monthly = 12000,
      startup_mode = "itemized",
      cost_ehr = 0, cost_equipment = 0, cost_licensing = 0, cost_marketing = 0, cost_other = 0,
      runway_mode = "single",
      monthly_expenses = 0, months_coverage = 1,
      horizon_months = 24,
      overhead_growth_rate = 0
    )
    session$setInputs(submit = 1)
    expect_null(r$market_context)
  })
})
