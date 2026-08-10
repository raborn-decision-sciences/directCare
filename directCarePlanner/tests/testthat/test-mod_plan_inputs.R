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

test_that("fee-for-service ramp fields flow through to r$revenue", {
  r <- reactiveValues(
    practice_name = NULL, horizon_months = NULL, market_context = NULL,
    revenue = NULL, projections = NULL, capital = NULL, interpretations = NULL
  )

  testServer(mod_plan_inputs_server, args = list(r = r), {
    session$setInputs(
      zip = "30309",
      include_membership = FALSE,
      include_fee = TRUE,
      visit_volume = 100,
      new_visit_fee = 200,
      follow_up_fee = 100,
      new_visit_pct = 20,
      fee_ramp_months = 4,
      fee_ramp_shape = "linear",
      overhead_mode = "single",
      overhead_monthly = 0,
      startup_mode = "single",
      cost_total = 0,
      runway_mode = "single",
      monthly_expenses = 0,
      months_coverage = 1,
      horizon_months = 4,
      overhead_growth_rate = 0
    )
    session$setInputs(submit = 1)

    # Same ramp math as directCarePlanR's own test-revenue.R: visit volume
    # builds 25, 50, 75, 100 over 4 months (ramp_months = horizon_months = 4,
    # linear); revenue per visit = 0.2*200 + 0.8*100 = 120.
    expect_equal(r$revenue$fee_for_service$visit_volume, c(25, 50, 75, 100))
    expect_equal(r$revenue$fee_for_service$revenue, c(25, 50, 75, 100) * 120)
  })
})

test_that("omitting fee ramp inputs falls back to an unramped (ramp_months = 1) fee projection", {
  # Covers a scenario saved before fee-for-service ramp existed: `values`
  # (a just-loaded scenario) has no fee_ramp_months/fee_ramp_shape keys at
  # all, so .build_plan() must fall back to the pre-ramp flat behavior
  # those old scenarios were actually built with, not silently reinterpret
  # them under some default ramp.
  r <- reactiveValues(
    practice_name = NULL, horizon_months = NULL, market_context = NULL,
    revenue = NULL, projections = NULL, capital = NULL, interpretations = NULL
  )

  testServer(mod_plan_inputs_server, args = list(r = r), {
    session$setInputs(
      zip = "30309",
      include_membership = FALSE,
      include_fee = TRUE,
      visit_volume = 100,
      new_visit_fee = 200,
      follow_up_fee = 100,
      new_visit_pct = 20,
      overhead_mode = "single",
      overhead_monthly = 0,
      startup_mode = "single",
      cost_total = 0,
      runway_mode = "single",
      monthly_expenses = 0,
      months_coverage = 1,
      horizon_months = 4,
      overhead_growth_rate = 0
    )
    session$setInputs(submit = 1)

    expect_equal(r$revenue$fee_for_service$visit_volume, rep(100, 4))
    expect_equal(r$revenue$fee_for_service$revenue, rep(12000, 4))
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

test_that("cost_label_editor_ui renders a field per startup-cost slug", {
  r <- reactiveValues(cost_item_labels = NULL)

  testServer(mod_plan_inputs_server, args = list(r = r), {
    html <- paste(as.character(output$cost_label_editor_ui), collapse = "")
    expect_true(grepl("cost_label_ehr_setup", html, fixed = TRUE))
    expect_true(grepl("cost_label_equipment", html, fixed = TRUE))
    expect_true(grepl("cost_label_total", html, fixed = TRUE))
  })
})

test_that("saving cost labels writes r$cost_item_labels, falling back to defaults for blanks", {
  r <- reactiveValues(cost_item_labels = NULL)

  testServer(mod_plan_inputs_server, args = list(r = r), {
    session$setInputs(cost_label_ehr_setup = "Practice Software", cost_label_equipment = "")
    session$setInputs(btn_save_cost_labels = 1)

    expect_equal(r$cost_item_labels[["ehr_setup"]], "Practice Software")
    expect_equal(r$cost_item_labels[["equipment"]], "Equipment")
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

# -- Location comparison (Pro) -------------------------------------------

full_plan_inputs <- function(extra = list()) {
  utils::modifyList(
    list(
      zip = "30309",
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
    ),
    extra
  )
}

test_that("Pro tier with valid compare ZIPs populates market_context_compare", {
  r <- reactiveValues(market_context = NULL, plan_tier = "pro")

  testServer(mod_plan_inputs_server, args = list(r = r), {
    do.call(session$setInputs, full_plan_inputs(list(zip_2 = "94103", zip_3 = "10001")))
    session$setInputs(submit = 1)

    expect_equal(r$market_context$geography$county_fips, "13121")
    expect_equal(r$market_context_compare_requested, 2L)
    expect_length(r$market_context_compare, 2L)
    expect_equal(r$market_context_compare[[1]]$geography$county_fips, "06075")
    expect_equal(r$market_context_compare[[2]]$geography$county_fips, "36061")
  })
})

test_that("non-Pro tier with compare ZIPs entered tracks the request but doesn't compute contexts", {
  r <- reactiveValues(market_context = NULL, plan_tier = "starter")

  testServer(mod_plan_inputs_server, args = list(r = r), {
    do.call(session$setInputs, full_plan_inputs(list(zip_2 = "94103", zip_3 = "10001")))
    session$setInputs(submit = 1)

    expect_equal(r$market_context$geography$county_fips, "13121")
    expect_equal(r$market_context_compare_requested, 2L)
    expect_null(r$market_context_compare)
  })
})

test_that("blank compare ZIPs leave market_context_compare_requested at 0", {
  r <- reactiveValues(market_context = NULL, plan_tier = "pro")

  testServer(mod_plan_inputs_server, args = list(r = r), {
    do.call(session$setInputs, full_plan_inputs())
    session$setInputs(submit = 1)

    expect_equal(r$market_context_compare_requested, 0L)
    expect_null(r$market_context_compare)
  })
})

test_that("an unresolvable compare ZIP is skipped without blocking the plan or the other compare ZIP", {
  r <- reactiveValues(market_context = NULL, plan_tier = "pro")

  testServer(mod_plan_inputs_server, args = list(r = r), {
    do.call(session$setInputs, full_plan_inputs(list(zip_2 = "94103", zip_3 = "00000")))
    session$setInputs(submit = 1)

    expect_equal(r$market_context$geography$county_fips, "13121")
    # Both were entered, so "requested" counts both, even though only one resolved.
    expect_equal(r$market_context_compare_requested, 2L)
    expect_length(r$market_context_compare, 1L)
    expect_equal(r$market_context_compare[[1]]$geography$county_fips, "06075")
  })
})
