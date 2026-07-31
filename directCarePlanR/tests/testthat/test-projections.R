# -- project_practice -----------------------------------------------------

test_that("project_practice() nets membership revenue against flat overhead", {
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 3),
    overhead_monthly = 5000
  )
  result <- project_practice(assumptions, horizon_months = 3)

  expect_s3_class(result, "dcPlanR_projection")
  expect_equal(result$month, 1:3)
  expect_equal(result$membership_revenue, c(10000, 20000, 30000))
  expect_equal(result$fee_revenue, c(0, 0, 0))
  expect_equal(result$total_revenue, c(10000, 20000, 30000))
  expect_equal(result$overhead, c(5000, 5000, 5000))
  expect_equal(result$net_income, c(5000, 15000, 25000))
  expect_equal(result$cumulative_net_income, c(5000, 20000, 45000))
})

test_that("project_practice() compounds overhead growth month over month", {
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 3),
    overhead_monthly = 1000,
    overhead_growth_rate = 0.1
  )
  result <- project_practice(assumptions, horizon_months = 3)

  expect_equal(result$overhead, c(1000, 1100, 1210))
  expect_equal(result$net_income, c(9000, 18900, 28790))
  expect_equal(result$cumulative_net_income, c(9000, 27900, 56690))
})

test_that("project_practice() combines membership and fee-for-service revenue", {
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 3),
    fee_args = list(visit_volume = 100, new_visit_fee = 200, follow_up_fee = 100, ramp_months = 1),
    overhead_monthly = 5000
  )
  result <- project_practice(assumptions, horizon_months = 3)

  # Cross-check against calc_mixed_revenue() directly rather than
  # re-deriving the revenue math here.
  revenue <- calc_mixed_revenue(
    membership_args = assumptions$membership_args,
    fee_args = assumptions$fee_args,
    horizon_months = 3
  )
  expect_equal(result$membership_revenue, revenue$total$membership_revenue)
  expect_equal(result$fee_revenue, revenue$total$fee_revenue)
  expect_equal(result$total_revenue, revenue$total$total_revenue)
  expect_equal(result$net_income, c(17000, 27000, 37000))
  expect_equal(result$cumulative_net_income, c(17000, 44000, 81000))
})

test_that("project_practice() errors when no revenue component is supplied", {
  expect_error(
    project_practice(list(overhead_monthly = 5000), horizon_months = 3),
    class = "dcPlanR_missing_revenue_component"
  )
})

test_that("project_practice() errors on missing or invalid overhead_monthly", {
  assumptions <- list(membership_args = list(panel_size = 300, fee = 100))
  expect_error(
    project_practice(assumptions, horizon_months = 3),
    class = "dcPlanR_invalid_argument"
  )
  expect_error(
    project_practice(c(assumptions, list(overhead_monthly = -100)), horizon_months = 3),
    class = "dcPlanR_invalid_argument"
  )
})

# -- project_scenarios ------------------------------------------------------

test_that("project_scenarios() base scenario matches a direct project_practice() call", {
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 12),
    overhead_monthly = 5000
  )
  result <- project_scenarios(assumptions, horizon_months = 12)
  base_rows <- result[result$scenario == "base", ]
  direct <- project_practice(assumptions, horizon_months = 12)

  expect_equal(base_rows$month, direct$month)
  expect_equal(base_rows$net_income, direct$net_income)
  expect_equal(base_rows$overhead, direct$overhead)
})

test_that("project_scenarios() defaults: conservative is slower/pricier, optimistic faster/cheaper", {
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 12),
    overhead_monthly = 5000
  )
  result <- project_scenarios(assumptions, horizon_months = 12)
  month6 <- result[result$month == 6, ]
  month6 <- month6[match(c("conservative", "base", "optimistic"), month6$scenario), ]

  expect_equal(month6$membership_revenue, c(10000, 15000, 20000))
  expect_equal(month6$overhead, c(5500, 5000, 4500))
})

test_that("project_scenarios() lets scenario_params override individual fields", {
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 12),
    overhead_monthly = 5000
  )
  result <- project_scenarios(
    assumptions,
    horizon_months = 12,
    scenario_params = list(conservative = list(overhead_multiplier = 1.2))
  )
  conservative <- result[result$scenario == "conservative", ]

  # overhead_multiplier overridden to 1.2 -> 5000 * 1.2 = 6000
  expect_equal(unique(conservative$overhead), 6000)
  # ramp_months_multiplier keeps its 1.5 default -> ramp_months = 18,
  # so month 6 is not yet fully ramped (same as the default-params case).
  expect_equal(conservative$membership_revenue[conservative$month == 6], 10000)
})

test_that("project_scenarios() errors on unrecognized scenario_params names", {
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 12),
    overhead_monthly = 5000
  )
  expect_error(
    project_scenarios(assumptions, scenario_params = list(bad_name = list())),
    class = "dcPlanR_invalid_argument"
  )
})
