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

  expect_false(grepl("any one of the following would do it", text))
  expect_false(grepl("this assumption alone", text))
})

test_that("interpret_breakeven() appends the goal-seek paragraph when break-even isn't reached", {
  result <- make_flat_result(revenue = 1800, overhead = 2000)
  gs <- goal_seek_breakeven(result)

  text <- interpret_breakeven(result, goal_seek = gs)

  expect_true(grepl("any one of the following would do it", text))
  expect_true(grepl("Raise assumed income growth", text))
  expect_true(grepl("Lower assumed overhead growth", text))
  expect_true(grepl("Make a smaller change to both together", text))
})

test_that("interpret_breakeven() ignores goal_seek once break-even is already achieved", {
  result <- make_flat_result(revenue = 2500, overhead = 2000)
  gs <- goal_seek_breakeven(make_flat_result(revenue = 1800, overhead = 2000))

  text <- interpret_breakeven(result, goal_seek = gs)

  expect_false(grepl("any one of the following would do it", text))
  expect_true(grepl("operating at a surplus", text))
})

# -- interpret_breakeven()'s stress_test parameter ----------------------------

test_that("interpret_breakeven() omits the stress-test paragraph when none is supplied", {
  result <- make_flat_result(revenue = 2500, overhead = 2000)
  text <- interpret_breakeven(result)

  expect_false(grepl("could absorb losing up to", text))
})

test_that("interpret_breakeven() appends the stress-test paragraph once break-even is achieved", {
  result <- make_flat_result(revenue = 2500, overhead = 2000)
  st <- stress_test_breakeven(result, panel_size = 50, membership_fee = 50)

  text <- interpret_breakeven(result, panel_size = 50, membership_fee = 50, stress_test = st)

  expect_true(grepl("could absorb losing up to", text))
  expect_true(grepl("members</strong> before break-even", text))
})

test_that("interpret_breakeven() ignores stress_test when break-even isn't reached", {
  result <- make_flat_result(revenue = 1800, overhead = 2000)
  # A stress-test result computed for a *different*, achieved scenario --
  # passing it in here should still be ignored, mirroring how goal_seek is
  # ignored once break-even is already met (the reverse gate).
  st <- stress_test_breakeven(
    make_flat_result(revenue = 2500, overhead = 2000),
    panel_size = 50,
    membership_fee = 50
  )

  text <- interpret_breakeven(result, stress_test = st)

  expect_false(grepl("could absorb losing up to", text))
})

# -- interpret_target()'s goal_seek parameter --------------------------------

make_flat_target_result <- function(
  revenue = 1800,
  required_revenue = 2500,
  overhead = 2000,
  n_forecast = 12L
) {
  fd <- tibble::tibble(
    period_start = seq(as.Date("2025-07-01"), by = "month", length.out = n_forecast),
    revenue_forecast = rep(revenue, n_forecast),
    revenue_lower = rep(revenue * 0.9, n_forecast),
    revenue_upper = rep(revenue * 1.1, n_forecast),
    overhead_forecast = rep(overhead, n_forecast),
    overhead_lower = rep(overhead * 0.9, n_forecast),
    overhead_upper = rep(overhead * 1.1, n_forecast),
    required_revenue = rep(required_revenue, n_forecast),
    # No rep() wrapper: within tibble(), `required_revenue` here refers to
    # the column just defined above (data-masking), already length
    # n_forecast -- `revenue` (the function arg, a scalar) recycles
    # against it naturally.
    net_vs_target = revenue - required_revenue
  )
  list(
    target_date = if (revenue >= required_revenue) fd$period_start[1] else as.Date(NA),
    periods_to_target = if (revenue >= required_revenue) 0L else NA_integer_,
    current_gap = revenue - required_revenue,
    required_revenue_now = required_revenue,
    confidence_interval = c(lower = as.Date(NA), upper = as.Date(NA)),
    forecast_data = fd,
    target_income = required_revenue - overhead,
    method = "linear",
    frequency = "monthly",
    data_warnings = NULL
  )
}

test_that("interpret_target() omits the goal-seek paragraph when none is supplied", {
  result <- make_flat_target_result(revenue = 1800, required_revenue = 2500, overhead = 2000)
  text <- interpret_target(result, target_income = 500)

  expect_false(grepl("any one of the following would do it", text))
  expect_false(grepl("this assumption alone", text))
})

test_that("interpret_target() appends the goal-seek paragraph when the target isn't reached", {
  result <- make_flat_target_result(revenue = 1800, required_revenue = 2500, overhead = 2000)
  gs <- goal_seek_target(result, target_income_override = 500)

  text <- interpret_target(result, target_income = 500, goal_seek = gs)

  expect_true(grepl("any one of the following would do it", text))
  expect_true(grepl("To reach the income target within", text))
  expect_true(grepl("Raise assumed income growth", text))
  expect_true(grepl("Lower assumed overhead growth", text))
  expect_true(grepl("Make a smaller change to both together", text))
})

test_that("interpret_target() ignores goal_seek once the target is already met", {
  result <- make_flat_target_result(revenue = 3000, required_revenue = 2500, overhead = 2000)
  gs <- goal_seek_target(
    make_flat_target_result(revenue = 1800, required_revenue = 2500, overhead = 2000),
    target_income_override = 500
  )

  text <- interpret_target(result, target_income = 500, goal_seek = gs)

  expect_false(grepl("any one of the following would do it", text))
})

# -- compare_breakeven_scenarios ----------------------------------------------

mk_bkevn_result <- function(periods_to_breakeven, frequency = "monthly") {
  list(periods_to_breakeven = periods_to_breakeven, frequency = frequency)
}

test_that("compare_breakeven_scenarios() names the earlier scenario when both reach break-even", {
  scenarios <- list(
    list(label = "Scenario A", result = mk_bkevn_result(9L)),
    list(label = "Scenario B", result = mk_bkevn_result(5L))
  )

  text <- compare_breakeven_scenarios(scenarios)

  expect_true(grepl("Scenario B.*reaches break-even soonest, in month 5", text))
  expect_true(grepl("Scenario A.*follows 4 months later, in month 9", text))
})

test_that("compare_breakeven_scenarios() singularizes a 1-period gap", {
  scenarios <- list(
    list(label = "A", result = mk_bkevn_result(6L)),
    list(label = "B", result = mk_bkevn_result(5L))
  )

  text <- compare_breakeven_scenarios(scenarios)

  expect_true(grepl("follows 1 month later", text))
  expect_false(grepl("1 months", text))
})

test_that("compare_breakeven_scenarios() handles 3 scenarios with one not reached", {
  scenarios <- list(
    list(label = "A", result = mk_bkevn_result(9L)),
    list(label = "B", result = mk_bkevn_result(5L)),
    list(label = "C", result = mk_bkevn_result(NA_integer_))
  )

  text <- compare_breakeven_scenarios(scenarios)

  expect_true(grepl("<strong>B</strong> reaches break-even soonest, in month 5", text))
  expect_true(grepl("<strong>A</strong> follows 4 months later, in month 9", text))
  expect_true(grepl("<strong>C</strong> does not reach break-even within its forecast window", text))
})

test_that("compare_breakeven_scenarios() reports when none reach break-even", {
  scenarios <- list(
    list(label = "A", result = mk_bkevn_result(NA_integer_)),
    list(label = "B", result = mk_bkevn_result(NA_integer_))
  )

  text <- compare_breakeven_scenarios(scenarios)

  expect_true(grepl("None of your saved scenarios", text))
  expect_true(grepl("goal-seek breakdown above", text))
})

test_that("compare_breakeven_scenarios() drops the numeric gap across mismatched frequencies", {
  scenarios <- list(
    list(label = "A", result = mk_bkevn_result(9L, "monthly")),
    list(label = "B", result = mk_bkevn_result(20L, "weekly"))
  )

  text <- compare_breakeven_scenarios(scenarios)

  expect_true(grepl("reaches break-even soonest\\.", text)) # no ", in month N" clause
  expect_true(grepl("reaches break-even in period 20", text))
})

test_that("compare_breakeven_scenarios() returns NULL for fewer than 2 scenarios", {
  expect_null(compare_breakeven_scenarios(list()))
  expect_null(compare_breakeven_scenarios(list(list(label = "A", result = mk_bkevn_result(1L)))))
})

test_that("compare_breakeven_scenarios() HTML-escapes scenario labels", {
  scenarios <- list(
    list(label = "A <script>", result = mk_bkevn_result(5L)),
    list(label = "B", result = mk_bkevn_result(9L))
  )

  text <- compare_breakeven_scenarios(scenarios)

  expect_false(grepl("<script>", text, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;", text, fixed = TRUE))
})

# -- compare_calculator_scenarios ---------------------------------------------

mk_calc_result <- function(net, target_income = 0) {
  list(net = net, target_income = target_income)
}

test_that("compare_calculator_scenarios() names the scenario with the strongest net position", {
  scenarios <- list(
    list(label = "A", result = mk_calc_result(600)),
    list(label = "B", result = mk_calc_result(2900))
  )

  text <- compare_calculator_scenarios(scenarios)

  expect_true(grepl("<strong>B</strong> has the strongest net position.*\\$2,900\\.00/mo surplus", text))
  expect_true(grepl("<strong>A</strong> follows.*\\$600\\.00/mo surplus.*\\$2,300\\.00/mo less", text))
})

test_that("compare_calculator_scenarios() labels a negative net as a deficit", {
  scenarios <- list(
    list(label = "A", result = mk_calc_result(-800)),
    list(label = "B", result = mk_calc_result(-200))
  )

  text <- compare_calculator_scenarios(scenarios)

  expect_true(grepl("<strong>B</strong> has the strongest net position.*\\$200\\.00/mo deficit", text))
  expect_true(grepl("<strong>A</strong> follows.*\\$800\\.00/mo deficit", text))
})

test_that("compare_calculator_scenarios() notes scenarios that meet their income target", {
  scenarios <- list(
    list(label = "Meets", result = mk_calc_result(6500, target_income = 4000)),
    list(label = "Weak", result = mk_calc_result(-2100, target_income = 4000))
  )

  text <- compare_calculator_scenarios(scenarios)

  expect_true(grepl("<strong>Meets</strong> meets its income target\\.", text))
  expect_false(grepl("<strong>Weak</strong> meets its income target", text))
})

test_that("compare_calculator_scenarios() pluralizes when multiple scenarios meet target", {
  scenarios <- list(
    list(label = "A", result = mk_calc_result(5000, target_income = 4000)),
    list(label = "B", result = mk_calc_result(4500, target_income = 4000))
  )

  text <- compare_calculator_scenarios(scenarios)

  expect_true(grepl("meet their income targets\\.", text))
})

test_that("compare_calculator_scenarios() omits the target sentence when no scenario has a target set", {
  scenarios <- list(
    list(label = "A", result = mk_calc_result(600)),
    list(label = "B", result = mk_calc_result(2900))
  )

  text <- compare_calculator_scenarios(scenarios)

  expect_false(grepl("income target", text))
})

test_that("compare_calculator_scenarios() returns NULL for fewer than 2 scenarios", {
  expect_null(compare_calculator_scenarios(list()))
  expect_null(compare_calculator_scenarios(list(list(label = "A", result = mk_calc_result(100)))))
})

test_that("compare_calculator_scenarios() HTML-escapes scenario labels", {
  scenarios <- list(
    list(label = "A <script>", result = mk_calc_result(100)),
    list(label = "B", result = mk_calc_result(200))
  )

  text <- compare_calculator_scenarios(scenarios)

  expect_false(grepl("<script>", text, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;", text, fixed = TRUE))
})
