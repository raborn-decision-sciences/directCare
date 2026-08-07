# Tests for interpret_breakeven()'s goal_seek parameter (utils_interpret.R).
# Only covers the new goal-seek behavior -- interpret_breakeven() itself has
# no prior direct unit tests (previously exercised only indirectly through
# module-server tests), so this deliberately stays scoped to what changed
# rather than attempting to backfill full coverage in the same PR.

# Self-contained fixture (not shared with test-utils-forecast.R's
# make_breakeven_result()/make_flat_breakeven_result() -- testthat sources
# test files in filename order, and "test-interpret.R" sorts before
# "test-utils-forecast.R", so relying on the latter's helpers here would be
# load-order-fragile).
make_flat_result <- function(revenue = 1800, overhead = 2000, n_forecast = 12L) {
  fd <- tibble::tibble(
    period_start = seq(as.Date("2025-07-01"), by = "month", length.out = n_forecast),
    revenue_forecast = rep(revenue, n_forecast),
    revenue_lower = rep(revenue * 0.9, n_forecast),
    revenue_upper = rep(revenue * 1.1, n_forecast),
    overhead_forecast = rep(overhead, n_forecast),
    overhead_lower = rep(overhead * 0.9, n_forecast),
    overhead_upper = rep(overhead * 1.1, n_forecast),
    net_forecast = rep(revenue - overhead, n_forecast)
  )
  list(
    breakeven_date = if (revenue >= overhead) fd$period_start[1] else as.Date(NA),
    periods_to_breakeven = if (revenue >= overhead) 0L else NA_integer_,
    current_surplus_deficit = revenue - overhead,
    current_revenue = revenue,
    current_overhead = overhead,
    current_overhead_avg = overhead,
    overhead_avg_n = 4L,
    confidence_interval = c(lower = as.Date(NA), upper = as.Date(NA)),
    forecast_data = fd,
    method = "linear",
    frequency = "monthly",
    data_warnings = NULL
  )
}

test_that("interpret_breakeven() omits the goal-seek paragraph when none is supplied", {
  result <- make_flat_result(revenue = 1800, overhead = 2000)
  text <- interpret_breakeven(result)

  expect_false(grepl("either change alone would do it", text))
  expect_false(grepl("this assumption alone", text))
})

test_that("interpret_breakeven() appends the goal-seek paragraph when break-even isn't reached", {
  result <- make_flat_result(revenue = 1800, overhead = 2000)
  gs <- goal_seek_breakeven(result)

  text <- interpret_breakeven(result, goal_seek = gs)

  expect_true(grepl("either change alone would do it", text))
  expect_true(grepl("raise assumed income growth", text))
  expect_true(grepl("lower assumed overhead growth", text))
})

test_that("interpret_breakeven() ignores goal_seek once break-even is already achieved", {
  result <- make_flat_result(revenue = 2500, overhead = 2000)
  gs <- goal_seek_breakeven(make_flat_result(revenue = 1800, overhead = 2000))

  text <- interpret_breakeven(result, goal_seek = gs)

  expect_false(grepl("either change alone would do it", text))
  expect_true(grepl("operating at a surplus", text))
})
