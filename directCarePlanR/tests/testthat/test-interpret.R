test_that("interpretation functions are stubbed pending implementation", {
  expect_error(interpret_revenue(list()), class = "dcPlanR_not_implemented")
  expect_error(interpret_projection(list()), class = "dcPlanR_not_implemented")
  expect_error(interpret_capital(list(), list()), class = "dcPlanR_not_implemented")
})
