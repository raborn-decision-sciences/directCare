test_that("market context functions are stubbed pending implementation", {
  expect_error(resolve_geography("30301"), class = "dcPlanR_not_implemented")
  expect_error(get_population_income("13121"), class = "dcPlanR_not_implemented")
  expect_error(get_uninsured_estimate("13121"), class = "dcPlanR_not_implemented")
  expect_error(get_physician_density("13121"), class = "dcPlanR_not_implemented")
  expect_error(get_direct_care_landscape("13121"), class = "dcPlanR_not_implemented")
  expect_error(build_market_context("30301"), class = "dcPlanR_not_implemented")
})
