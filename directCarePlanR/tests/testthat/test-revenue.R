test_that("revenue functions are stubbed pending implementation", {
  expect_error(
    calc_membership_revenue(panel_size = 300, fee = 100),
    class = "dcPlanR_not_implemented"
  )
  expect_error(
    calc_fee_revenue(visit_volume = 100, new_visit_fee = 200, follow_up_fee = 100),
    class = "dcPlanR_not_implemented"
  )
  expect_error(
    calc_mixed_revenue(
      membership_args = list(panel_size = 300, fee = 100),
      fee_args = list(visit_volume = 20, new_visit_fee = 200, follow_up_fee = 100)
    ),
    class = "dcPlanR_not_implemented"
  )
})
