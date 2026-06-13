# Tests for apply_growth_assumptions(), breakeven_is_sustained(), and
# target_is_sustained() in utils_forecast.R.

# ── Fixtures ───────────────────────────────────────────────────────────────────

make_breakeven_result <- function(
  periods_to_breakeven = 3L,
  n_forecast = 12L,
  revenue_start = 1000,
  overhead = 2000
) {
  fd <- tibble::tibble(
    period_start = seq(
      as.Date("2025-07-01"),
      by = "month",
      length.out = n_forecast
    ),
    revenue_forecast = seq(
      revenue_start,
      revenue_start * 2,
      length.out = n_forecast
    ),
    revenue_lower = seq(
      revenue_start * 0.8,
      revenue_start * 1.6,
      length.out = n_forecast
    ),
    revenue_upper = seq(
      revenue_start * 1.2,
      revenue_start * 2.4,
      length.out = n_forecast
    ),
    overhead_forecast = rep(overhead, n_forecast),
    overhead_lower = rep(overhead * 0.9, n_forecast),
    overhead_upper = rep(overhead * 1.1, n_forecast),
    net_forecast = seq(
      revenue_start,
      revenue_start * 2,
      length.out = n_forecast
    ) -
      overhead
  )
  list(
    breakeven_date = if (identical(periods_to_breakeven, 0L)) {
      as.Date("2025-06-01")
    } else {
      fd$period_start[periods_to_breakeven]
    },
    periods_to_breakeven = periods_to_breakeven,
    current_surplus_deficit = -500,
    current_revenue = revenue_start,
    current_overhead = overhead,
    current_overhead_avg = overhead,
    overhead_avg_n = 4L,
    confidence_interval = c(
      lower = as.Date("2025-09-01"),
      upper = as.Date("2025-11-01")
    ),
    forecast_data = fd,
    method = "linear",
    frequency = "monthly",
    data_warnings = NULL
  )
}

make_target_result <- function(current_gap = -200, n_forecast = 12L) {
  fd <- tibble::tibble(
    period_start = seq(
      as.Date("2025-07-01"),
      by = "month",
      length.out = n_forecast
    ),
    revenue_forecast = seq(1000, 3000, length.out = n_forecast),
    revenue_lower = seq(800, 2400, length.out = n_forecast),
    revenue_upper = seq(1200, 3600, length.out = n_forecast),
    overhead_forecast = rep(2000, n_forecast),
    required_revenue = rep(2500, n_forecast),
    net_vs_target = seq(1000, 3000, length.out = n_forecast) - 2500
  )
  list(
    target_date = fd$period_start[which(fd$net_vs_target >= 0)[1]],
    periods_to_target = which(fd$net_vs_target >= 0)[1],
    current_gap = current_gap,
    required_revenue_now = 2500,
    confidence_interval = c(lower = as.Date(NA), upper = as.Date(NA)),
    forecast_data = fd,
    target_income = 500,
    method = "linear",
    frequency = "monthly",
    data_warnings = NULL
  )
}

# ── apply_growth_assumptions: revenue scaling ──────────────────────────────────

test_that("apply_growth_assumptions scales revenue columns by income growth", {
  result <- make_breakeven_result()
  original_rev <- result$forecast_data$revenue_forecast

  adj <- apply_growth_assumptions(result, income_growth_pct = 12)

  # Each period n gets multiplier (1 + 0.12/12)^n = 1.01^n
  n <- seq_len(nrow(result$forecast_data))
  expected <- original_rev * (1.01^n)
  expect_equal(adj$forecast_data$revenue_forecast, expected, tolerance = 1e-9)
})

test_that("apply_growth_assumptions leaves revenue unchanged at 0% growth", {
  result <- make_breakeven_result()
  adj <- apply_growth_assumptions(result, income_growth_pct = 0)
  expect_equal(
    adj$forecast_data$revenue_forecast,
    result$forecast_data$revenue_forecast
  )
})

# ── apply_growth_assumptions: overhead flat model ─────────────────────────────

test_that("apply_growth_assumptions replaces overhead with flat value", {
  result <- make_breakeven_result()
  adj <- apply_growth_assumptions(result, overhead_flat = 1800)

  expect_true(all(adj$forecast_data$overhead_forecast == 1800))
  expect_true(all(adj$forecast_data$overhead_lower == 1800))
  expect_true(all(adj$forecast_data$overhead_upper == 1800))
})

# ── apply_growth_assumptions: CI re-derivation ────────────────────────────────

test_that("apply_growth_assumptions re-derives confidence_interval after growth", {
  # Build a result where the original CI is stale (set to fixed dates).
  result <- make_breakeven_result(periods_to_breakeven = 6L)
  original_ci <- result$confidence_interval

  # Apply strong income growth — pessimistic CI crossing should move earlier.
  adj <- apply_growth_assumptions(result, income_growth_pct = 50)

  # CI must be re-derived, not just copied from the original
  expect_false(identical(adj$confidence_interval, original_ci))
  expect_named(adj$confidence_interval, c("lower", "upper"))
})

test_that("apply_growth_assumptions CI lower is a Date or NA", {
  result <- make_breakeven_result(periods_to_breakeven = 6L)
  adj <- apply_growth_assumptions(result, income_growth_pct = 10)

  ci <- adj$confidence_interval
  check_date_or_na <- function(x) is.na(x) || inherits(x, "Date")
  expect_true(check_date_or_na(ci["lower"]))
  expect_true(check_date_or_na(ci["upper"]))
})

test_that("apply_growth_assumptions does not re-derive CI when already at breakeven", {
  result <- make_breakeven_result(periods_to_breakeven = 0L)
  result$confidence_interval <- c(
    lower = as.Date("2025-06-01"),
    upper = as.Date("2025-06-01")
  )

  adj <- apply_growth_assumptions(result, income_growth_pct = 10)

  # periods_to_breakeven == 0L → already branch skips crossing re-derivation
  expect_equal(adj$periods_to_breakeven, 0L)
})

# ── apply_growth_assumptions: target forecast ─────────────────────────────────

test_that("apply_growth_assumptions re-derives target_date for target result", {
  result <- make_target_result()
  original_date <- result$target_date

  adj <- apply_growth_assumptions(result, income_growth_pct = 60)

  # Strong income growth should move the target date earlier
  expect_true(is.na(adj$target_date) || adj$target_date <= original_date)
})

test_that("apply_growth_assumptions returns NULL unchanged", {
  expect_null(apply_growth_assumptions(NULL))
})

# ── breakeven_is_sustained ────────────────────────────────────────────────────

test_that("breakeven_is_sustained returns NA when not yet at breakeven", {
  result <- make_breakeven_result(periods_to_breakeven = 3L)
  expect_identical(breakeven_is_sustained(result), NA)
})

test_that("breakeven_is_sustained returns NA for NULL input", {
  expect_identical(breakeven_is_sustained(NULL), NA)
})

test_that("breakeven_is_sustained returns TRUE when revenue exceeds overhead across forecast", {
  result <- make_breakeven_result(
    periods_to_breakeven = 0L,
    revenue_start = 3000,
    overhead = 2000
  )
  # All 12 periods: revenue_forecast (3000→6000) > overhead (2000) → 100% coverage
  expect_true(breakeven_is_sustained(result))
})

test_that("breakeven_is_sustained returns FALSE when overhead outpaces revenue late in horizon", {
  result <- make_breakeven_result(
    periods_to_breakeven = 0L,
    revenue_start = 2100,
    overhead = 2000
  )
  # Override overhead to exceed revenue in all but the first period
  result$forecast_data$overhead_forecast <- rep(5000, 12)
  expect_false(breakeven_is_sustained(result))
})

# ── target_is_sustained ────────────────────────────────────────────────────────

test_that("target_is_sustained returns NA when target not yet met", {
  result <- make_target_result(current_gap = -200)
  expect_identical(target_is_sustained(result), NA)
})

test_that("target_is_sustained returns NA for NULL input", {
  expect_identical(target_is_sustained(NULL), NA)
})

test_that("target_is_sustained returns TRUE when revenue exceeds required across forecast", {
  result <- make_target_result(current_gap = 500)
  # Override: revenue always exceeds required
  result$forecast_data$revenue_forecast <- rep(4000, 12)
  result$forecast_data$required_revenue <- rep(2500, 12)
  expect_true(target_is_sustained(result))
})

test_that("target_is_sustained returns FALSE when revenue falls below required in most periods", {
  result <- make_target_result(current_gap = 500)
  result$forecast_data$revenue_forecast <- rep(1000, 12) # well below required (2500)
  expect_false(target_is_sustained(result))
})
