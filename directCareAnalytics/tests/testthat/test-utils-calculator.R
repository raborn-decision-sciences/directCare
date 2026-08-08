# Tests for compute_calculator_results() (utils_calculator.R).

test_that("compute_calculator_results() computes single-total overhead and one tier", {
  inputs <- list(
    ovhd_multi = FALSE,
    monthly_overhead = 3000,
    tiers = list(list(label = "Adult", members = 40, fee = 90)),
    income_items = list(),
    target_income = 5000
  )

  res <- compute_calculator_results(inputs)

  expect_equal(res$total_overhead, 3000)
  expect_equal(res$tier_total_members, 40)
  expect_equal(res$tier_total_revenue, 3600)
  expect_equal(res$avg_fee_per_member, 90)
  expect_equal(res$other_income, 0)
  expect_equal(res$total_revenue, 3600)
  expect_equal(res$net, 600)
})

test_that("compute_calculator_results() sums itemized overhead sources", {
  inputs <- list(
    ovhd_multi = TRUE,
    overhead_items = list(
      list(label = "Rent", amount = 1500),
      list(label = "Payroll", amount = 1000)
    ),
    tiers = list(),
    income_items = list()
  )

  res <- compute_calculator_results(inputs)

  expect_equal(res$total_overhead, 2500)
})

test_that("compute_calculator_results() sums multiple tiers into a weighted average fee", {
  inputs <- list(
    monthly_overhead = 0,
    tiers = list(
      list(members = 50, fee = 80),
      list(members = 50, fee = 100)
    ),
    income_items = list()
  )

  res <- compute_calculator_results(inputs)

  expect_equal(res$tier_total_members, 100)
  expect_equal(res$tier_total_revenue, 50 * 80 + 50 * 100)
  expect_equal(res$avg_fee_per_member, 90)
})

test_that("compute_calculator_results() applies other income before computing net", {
  inputs <- list(
    monthly_overhead = 3000,
    tiers = list(list(members = 60, fee = 90)),
    income_items = list(list(label = "FFS", amount = 500))
  )

  res <- compute_calculator_results(inputs)

  expect_equal(res$other_income, 500)
  expect_equal(res$total_revenue, 5900)
  expect_equal(res$net, 2900)
})

test_that("compute_calculator_results() nets other income against the amount needed for break-even/target", {
  # $3000 overhead, $500 other income -> only $2500 needs to come from dues.
  inputs <- list(
    monthly_overhead = 3000,
    tiers = list(list(members = 100, fee = 90)),
    income_items = list(list(label = "FFS", amount = 500)),
    target_income = 1000
  )

  res <- compute_calculator_results(inputs)

  expect_equal(res$members_for_breakeven, ceiling(2500 / 90))
  expect_equal(res$members_for_target, ceiling(3500 / 90))
  expect_equal(res$fee_for_breakeven, 2500 / 100)
  expect_equal(res$fee_for_target, 3500 / 100)
})

test_that("compute_calculator_results() returns NA scenario fields when there's no panel yet", {
  inputs <- list(monthly_overhead = 1000, tiers = list(), income_items = list())

  res <- compute_calculator_results(inputs)

  expect_equal(res$avg_fee_per_member, 0)
  expect_true(is.na(res$members_for_breakeven))
  expect_true(is.na(res$fee_for_breakeven))
})

test_that("compute_calculator_results() handles the JSON round-trip shape from a saved scenario", {
  inputs <- list(
    ovhd_multi = FALSE,
    monthly_overhead = 3000,
    overhead_items = list(),
    tiers = list(list(label = "Adult", members = 100, fee = 90)),
    income_items = list(list(label = "FFS", amount = 500)),
    target_income = 4000
  )
  decoded <- jsonlite::fromJSON(
    jsonlite::toJSON(inputs, auto_unbox = TRUE),
    simplifyVector = FALSE
  )

  res <- compute_calculator_results(decoded)

  expect_equal(res$total_overhead, 3000)
  expect_equal(res$tier_total_members, 100)
  expect_equal(res$net, 6500)
})

test_that("compute_calculator_results() defaults missing fields to zero without erroring", {
  res <- compute_calculator_results(list())

  expect_equal(res$total_overhead, 0)
  expect_equal(res$net, 0)
  expect_equal(res$target_income, 0)
})
