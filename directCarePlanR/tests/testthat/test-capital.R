# -- calc_startup_costs --------------------------------------------------

test_that("calc_startup_costs() sums line items and preserves them", {
  result <- calc_startup_costs(c(equipment = 5000, ehr_setup = 2000, licensing = 1000))
  expect_s3_class(result, "dcPlanR_startup_costs")
  expect_equal(result$total, 8000)
  expect_equal(result$line_items, c(equipment = 5000, ehr_setup = 2000, licensing = 1000))
})

test_that("calc_startup_costs() works with a named list input", {
  result <- calc_startup_costs(list(equipment = 5000, marketing = 1500))
  expect_equal(result$total, 6500)
})

test_that("calc_startup_costs() errors on invalid input", {
  expect_error(calc_startup_costs(numeric(0)), class = "dcPlanR_invalid_argument")
  expect_error(calc_startup_costs(c(5000, 2000)), class = "dcPlanR_invalid_argument")
  expect_error(calc_startup_costs(c(equipment = 5000, equipment = 2000)), class = "dcPlanR_invalid_argument")
  expect_error(calc_startup_costs(c(equipment = -100)), class = "dcPlanR_invalid_argument")
  expect_error(calc_startup_costs(c(equipment = NA_real_)), class = "dcPlanR_invalid_argument")
})

# -- calc_personal_runway --------------------------------------------------

test_that("calc_personal_runway() multiplies expenses by months of coverage", {
  result <- calc_personal_runway(monthly_expenses = 4000, months_coverage = 6)
  expect_s3_class(result, "dcPlanR_personal_runway")
  expect_equal(result$total, 24000)
  expect_equal(result$monthly_expenses, 4000)
  expect_equal(result$months_coverage, 6L)
})

test_that("calc_personal_runway() errors on invalid arguments", {
  expect_error(calc_personal_runway(-100, 6), class = "dcPlanR_invalid_argument")
  expect_error(calc_personal_runway(4000, 0), class = "dcPlanR_invalid_argument")
  expect_error(calc_personal_runway(4000, 6.5), class = "dcPlanR_invalid_argument")
})

# -- calc_loan_amortization ------------------------------------------------

test_that("calc_loan_amortization() produces a correct interest-bearing schedule", {
  result <- calc_loan_amortization(principal = 1200, annual_rate = 0.12, term_months = 12)

  expect_s3_class(result, "dcPlanR_loan_amortization")
  expect_equal(nrow(result), 12L)
  expect_equal(result$month, 1:12)

  # Payment is level across all months.
  expect_equal(length(unique(round(result$payment, 6))), 1L)
  expect_equal(result$payment[1], 106.6185464, tolerance = 1e-6)

  # Month 1: interest = 1200 * (0.12/12) = 12 exactly.
  expect_equal(result$interest_paid[1], 12)
  expect_equal(result$principal_paid[1], result$payment[1] - 12)
  expect_equal(result$remaining_balance[1], 1200 - result$principal_paid[1])

  # Loan fully amortizes: total principal paid equals the original
  # principal, and the final balance is exactly 0 (no floating-point
  # residue).
  expect_equal(sum(result$principal_paid), 1200)
  expect_equal(result$remaining_balance[12], 0)
})

test_that("calc_loan_amortization() handles an interest-free loan", {
  result <- calc_loan_amortization(principal = 1200, annual_rate = 0, term_months = 12)

  expect_equal(unique(result$payment), 100)
  expect_true(all(result$interest_paid == 0))
  expect_true(all(result$principal_paid == 100))
  expect_equal(result$remaining_balance, seq(1100, 0, by = -100))
})

test_that("calc_loan_amortization() errors on invalid arguments", {
  expect_error(calc_loan_amortization(-100, 0.08, 60), class = "dcPlanR_invalid_argument")
  expect_error(calc_loan_amortization(50000, -0.01, 60), class = "dcPlanR_invalid_argument")
  expect_error(calc_loan_amortization(50000, 0.08, 0), class = "dcPlanR_invalid_argument")
})
