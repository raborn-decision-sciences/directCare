# -- interpret_revenue -----------------------------------------------------

test_that("interpret_revenue() reports composition and names the larger sensitivity lever", {
  revenue <- calc_mixed_revenue(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 3),
    fee_args = list(visit_volume = 100, new_visit_fee = 200, follow_up_fee = 100),
    horizon_months = 3
  )
  text <- interpret_revenue(revenue)

  expect_type(text, "character")
  expect_length(text, 1L)
  expect_true(grepl("\\$42,000/month", text))
  expect_true(grepl("\\$30,000 \\(71%\\)", text))
  expect_true(grepl("\\$12,000 \\(29%\\)", text))
  # Membership shift ($3,000) > fee shift ($1,200), so membership fee
  # should be named the more sensitive lever.
  expect_true(grepl("membership fee is the most sensitive lever", text))
  expect_true(grepl("\\$3,000", text))
  expect_true(grepl("\\$1,200", text))
})

test_that("interpret_revenue() handles a membership-only plan", {
  revenue <- calc_mixed_revenue(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 3),
    horizon_months = 3
  )
  text <- interpret_revenue(revenue)

  expect_true(grepl("\\$30,000/month in membership revenue", text))
  expect_true(grepl("no fee-for-service component", text))
  expect_false(grepl("most sensitive lever", text))
})

test_that("interpret_revenue() handles a fee-for-service-only plan", {
  revenue <- calc_mixed_revenue(
    fee_args = list(visit_volume = 100, new_visit_fee = 200, follow_up_fee = 100),
    horizon_months = 3
  )
  text <- interpret_revenue(revenue)

  expect_true(grepl("\\$12,000/month in fee-for-service revenue", text))
  expect_true(grepl("no membership component", text))
})

# -- interpret_projection ---------------------------------------------------

test_that("interpret_projection() reports the base recovery month and scenario spread", {
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 12),
    overhead_monthly = 5000
  )
  projection <- project_scenarios(assumptions, horizon_months = 24)
  text <- interpret_projection(projection)

  expect_true(grepl("base assumptions.*recovers.*month 3", text))
  expect_true(grepl("shifts to month 6", text))
  expect_true(grepl("reached by month 2", text))
  expect_true(grepl("4-month spread", text))
  expect_true(grepl("relatively robust", text))
})

test_that("interpret_projection() flags a wide scenario spread as highly sensitive", {
  assumptions <- list(
    membership_args = list(panel_size = 200, fee = 80, ramp_months = 12),
    overhead_monthly = 8000
  )
  projection <- project_scenarios(assumptions, horizon_months = 24)
  text <- interpret_projection(projection)

  expect_true(grepl("11-month spread", text))
  expect_true(grepl("highly sensitive", text))
})

test_that("interpret_projection() handles scenarios that never recover within the horizon", {
  assumptions <- list(
    membership_args = list(panel_size = 50, fee = 50, ramp_months = 12),
    overhead_monthly = 10000
  )
  projection <- project_scenarios(assumptions, horizon_months = 12)
  text <- interpret_projection(projection)

  expect_true(grepl("does not recover its ramp-period costs", text))
  expect_true(grepl("cannot be compared directly", text))
})

# -- interpret_capital -------------------------------------------------------

test_that("interpret_capital() reports totals, top line items, and a combined figure", {
  startup_costs <- calc_startup_costs(c(equipment = 5000, ehr_setup = 8000, licensing = 1000))
  personal_runway <- calc_personal_runway(monthly_expenses = 4000, months_coverage = 6)
  text <- interpret_capital(startup_costs, personal_runway)

  expect_true(grepl("\\$14,000", text))
  expect_true(grepl("EHR Setup \\(\\$8,000\\)", text))
  expect_true(grepl("Equipment \\(\\$5,000\\)", text))
  expect_false(grepl("[Ll]icensing", text)) # smallest item, not in the top 2
  expect_true(grepl("6 months at \\$4,000/month", text))
  expect_true(grepl("\\$24,000", text))
  expect_true(grepl("secure at least \\$38,000", text))
})
