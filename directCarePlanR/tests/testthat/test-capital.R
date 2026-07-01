test_that("capital planning functions are stubbed pending implementation", {
  expect_error(
    calc_startup_costs(c(equipment = 5000, ehr_setup = 2000)),
    class = "dcPlanR_not_implemented"
  )
  expect_error(
    calc_personal_runway(monthly_expenses = 4000, months_coverage = 6),
    class = "dcPlanR_not_implemented"
  )
  expect_error(
    calc_loan_amortization(principal = 50000, annual_rate = 0.08, term_months = 60),
    class = "dcPlanR_not_implemented"
  )
})
