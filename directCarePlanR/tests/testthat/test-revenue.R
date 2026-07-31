# -- calc_membership_revenue --------------------------------------------------

test_that("calc_membership_revenue() ramps linearly", {
  result <- calc_membership_revenue(
    panel_size = 300,
    fee = 100,
    horizon_months = 3,
    ramp_months = 3,
    ramp_shape = "linear"
  )
  expect_s3_class(result, "dcPlanR_membership_revenue")
  expect_equal(result$month, 1:3)
  expect_equal(result$panel_size, c(100, 200, 300))
  expect_equal(result$revenue, c(10000, 20000, 30000))
})

test_that("calc_membership_revenue() ramps via smoothstep s_curve", {
  result <- calc_membership_revenue(
    panel_size = 300,
    fee = 100,
    horizon_months = 3,
    ramp_months = 3,
    ramp_shape = "s_curve"
  )
  expect_equal(result$panel_size, c(78, 222, 300))
  expect_equal(result$revenue, c(7800, 22200, 30000))
})

test_that("calc_membership_revenue() handles a ramp longer than the horizon", {
  result <- calc_membership_revenue(
    panel_size = 300,
    fee = 100,
    horizon_months = 3,
    ramp_months = 24,
    ramp_shape = "linear"
  )
  expect_equal(result$panel_size, c(12, 25, 38))
  expect_true(all(result$panel_size < 300))
})

test_that("calc_membership_revenue() errors on invalid arguments", {
  expect_error(
    calc_membership_revenue(panel_size = -1, fee = 100),
    class = "dcPlanR_invalid_argument"
  )
  expect_error(
    calc_membership_revenue(panel_size = 300, fee = -5),
    class = "dcPlanR_invalid_argument"
  )
  expect_error(
    calc_membership_revenue(panel_size = 300, fee = 100, horizon_months = 0),
    class = "dcPlanR_invalid_argument"
  )
  expect_error(
    calc_membership_revenue(panel_size = 300, fee = 100, ramp_months = -1),
    class = "dcPlanR_invalid_argument"
  )
  expect_error(
    calc_membership_revenue(panel_size = 300, fee = 100, ramp_shape = "quadratic")
  )
})

# -- calc_fee_revenue ----------------------------------------------------------

test_that("calc_fee_revenue() computes flat revenue once ramp_months = 1 (fully ramped from month 1)", {
  result <- calc_fee_revenue(
    visit_volume = 100,
    new_visit_fee = 200,
    follow_up_fee = 100,
    new_visit_pct = 0.2,
    horizon_months = 4,
    ramp_months = 1
  )
  expect_s3_class(result, "dcPlanR_fee_revenue")
  expect_equal(result$month, 1:4)
  expect_equal(result$visit_volume, rep(100, 4))
  # 20 new visits * 200 + 80 follow-ups * 100 = 4000 + 8000 = 12000
  expect_equal(result$revenue, rep(12000, 4))
})

test_that("calc_fee_revenue() ramps linearly by default", {
  result <- calc_fee_revenue(
    visit_volume = 100,
    new_visit_fee = 200,
    follow_up_fee = 100,
    new_visit_pct = 0.2,
    horizon_months = 4,
    ramp_months = 4,
    ramp_shape = "linear"
  )
  expect_equal(result$visit_volume, c(25, 50, 75, 100))
  # revenue per visit = 0.2*200 + 0.8*100 = 120
  expect_equal(result$revenue, c(25, 50, 75, 100) * 120)
})

test_that("calc_fee_revenue() ramps via smoothstep s_curve", {
  result <- calc_fee_revenue(
    visit_volume = 100,
    new_visit_fee = 200,
    follow_up_fee = 100,
    horizon_months = 3,
    ramp_months = 3,
    ramp_shape = "s_curve"
  )
  # Same smoothstep fractions as calc_membership_revenue()'s s_curve test
  # (78/300, 222/300, 300/300), applied to visit_volume = 100 instead of
  # panel_size = 300.
  expect_equal(result$visit_volume, c(26, 74, 100))
})

test_that("calc_fee_revenue() handles a ramp longer than the horizon", {
  result <- calc_fee_revenue(
    visit_volume = 100,
    new_visit_fee = 200,
    follow_up_fee = 100,
    horizon_months = 3,
    ramp_months = 24,
    ramp_shape = "linear"
  )
  expect_true(all(result$visit_volume < 100))
})

test_that("calc_fee_revenue() errors on invalid arguments", {
  expect_error(
    calc_fee_revenue(visit_volume = -1, new_visit_fee = 200, follow_up_fee = 100),
    class = "dcPlanR_invalid_argument"
  )
  expect_error(
    calc_fee_revenue(visit_volume = 100, new_visit_fee = 200, follow_up_fee = 100, new_visit_pct = 1.5),
    class = "dcPlanR_invalid_argument"
  )
  expect_error(
    calc_fee_revenue(visit_volume = 100, new_visit_fee = 200, follow_up_fee = 100, horizon_months = -1),
    class = "dcPlanR_invalid_argument"
  )
  expect_error(
    calc_fee_revenue(visit_volume = 100, new_visit_fee = 200, follow_up_fee = 100, ramp_months = -1),
    class = "dcPlanR_invalid_argument"
  )
  expect_error(
    calc_fee_revenue(visit_volume = 100, new_visit_fee = 200, follow_up_fee = 100, ramp_shape = "quadratic")
  )
})

# -- calc_mixed_revenue ---------------------------------------------------------

test_that("calc_mixed_revenue() combines both components by month", {
  result <- calc_mixed_revenue(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 3),
    fee_args = list(visit_volume = 100, new_visit_fee = 200, follow_up_fee = 100, ramp_months = 1),
    horizon_months = 3
  )
  expect_s3_class(result, "dcPlanR_revenue")
  expect_equal(result$membership$revenue, c(10000, 20000, 30000))
  expect_equal(result$fee_for_service$revenue, rep(12000, 3))
  expect_equal(result$total$total_revenue, result$membership$revenue + result$fee_for_service$revenue)
})

test_that("calc_mixed_revenue() zero-fills the omitted component", {
  membership_only <- calc_mixed_revenue(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 3),
    horizon_months = 3
  )
  expect_null(membership_only$fee_for_service)
  expect_equal(membership_only$total$fee_revenue, c(0, 0, 0))
  expect_equal(membership_only$total$total_revenue, membership_only$membership$revenue)

  fee_only <- calc_mixed_revenue(
    fee_args = list(visit_volume = 100, new_visit_fee = 200, follow_up_fee = 100, ramp_months = 1),
    horizon_months = 3
  )
  expect_null(fee_only$membership)
  expect_equal(fee_only$total$membership_revenue, c(0, 0, 0))
  expect_equal(fee_only$total$total_revenue, fee_only$fee_for_service$revenue)
})

test_that("calc_mixed_revenue() overrides horizon_months in the sub-args", {
  result <- calc_mixed_revenue(
    membership_args = list(panel_size = 300, fee = 100, horizon_months = 99),
    horizon_months = 3
  )
  expect_equal(nrow(result$membership), 3L)
  expect_equal(nrow(result$total), 3L)
})

test_that("calc_mixed_revenue() errors when both components are omitted", {
  expect_error(
    calc_mixed_revenue(),
    class = "dcPlanR_missing_revenue_component"
  )
})
