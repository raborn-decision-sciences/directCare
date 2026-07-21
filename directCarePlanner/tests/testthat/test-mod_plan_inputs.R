# Unit tests for mod_plan_inputs_server (via testServer)

test_that("submitting a valid plan populates shared state and navigates to Results", {
  r <- reactiveValues(
    practice_name = NULL, horizon_months = NULL, market_context = NULL,
    revenue = NULL, projections = NULL, capital = NULL, interpretations = NULL
  )

  testServer(mod_plan_inputs_server, args = list(r = r), {
    session$setInputs(
      location = "30309",
      state = "",
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
      overhead_monthly = 12000,
      cost_ehr = 8000,
      cost_equipment = 5000,
      cost_licensing = 1500,
      cost_marketing = 3000,
      cost_other = 0,
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

test_that("an empty location shows an error notification and leaves state untouched", {
  r <- reactiveValues(market_context = NULL)

  testServer(mod_plan_inputs_server, args = list(r = r), {
    session$setInputs(location = "   ")
    session$setInputs(submit = 1)
    expect_null(r$market_context)
  })
})

test_that("an unresolvable location leaves state untouched", {
  r <- reactiveValues(market_context = NULL)

  testServer(mod_plan_inputs_server, args = list(r = r), {
    session$setInputs(
      location = "00000",
      state = "",
      include_membership = TRUE,
      panel_size = 300,
      fee = 100,
      ramp_months = 12,
      ramp_shape = "linear",
      include_fee = FALSE,
      overhead_monthly = 12000,
      cost_ehr = 0, cost_equipment = 0, cost_licensing = 0, cost_marketing = 0, cost_other = 0,
      monthly_expenses = 0, months_coverage = 1,
      horizon_months = 24,
      overhead_growth_rate = 0
    )
    session$setInputs(submit = 1)
    expect_null(r$market_context)
  })
})
