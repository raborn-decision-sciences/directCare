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

# ── apply_membership_fee_events ────────────────────────────────────────────────

test_that("apply_membership_fee_events returns result unchanged when events is NULL", {
  result <- make_breakeven_result()
  adj <- apply_membership_fee_events(result, NULL)
  expect_identical(adj, result)
})

test_that("apply_membership_fee_events steps revenue up from the event start date", {
  result <- make_breakeven_result(periods_to_breakeven = NA_integer_)
  fd0 <- result$forecast_data
  events <- data.frame(
    start_date = fd0$period_start[6],
    revenue_delta = 500,
    stringsAsFactors = FALSE
  )

  adj <- apply_membership_fee_events(result, events)
  fd1 <- adj$forecast_data

  # Before the event, revenue is unchanged.
  expect_equal(fd1$revenue_forecast[1:5], fd0$revenue_forecast[1:5])
  # From the event forward, revenue is stepped up by the delta.
  expect_equal(fd1$revenue_forecast[6:12], fd0$revenue_forecast[6:12] + 500)
  expect_equal(fd1$revenue_lower[6:12], fd0$revenue_lower[6:12] + 500)
  expect_equal(fd1$revenue_upper[6:12], fd0$revenue_upper[6:12] + 500)
})

test_that("apply_membership_fee_events re-derives break-even crossing", {
  # Revenue starts below overhead and only crosses after the fee bump.
  result <- make_breakeven_result(
    periods_to_breakeven = NA_integer_,
    revenue_start = 1000,
    overhead = 2000
  )
  result$forecast_data$revenue_forecast <- rep(1500, 12)
  events <- data.frame(
    start_date = result$forecast_data$period_start[4],
    revenue_delta = 600,
    stringsAsFactors = FALSE
  )

  adj <- apply_membership_fee_events(result, events)

  expect_equal(adj$periods_to_breakeven, 4L)
  expect_equal(adj$breakeven_date, result$forecast_data$period_start[4])
})

test_that("apply_membership_fee_events re-derives target crossing", {
  result <- make_target_result(current_gap = -500)
  result$forecast_data$revenue_forecast <- rep(2000, 12)
  result$forecast_data$required_revenue <- rep(2500, 12)
  events <- data.frame(
    start_date = result$forecast_data$period_start[3],
    revenue_delta = 700,
    stringsAsFactors = FALSE
  )

  adj <- apply_membership_fee_events(result, events)

  expect_equal(adj$periods_to_target, 3L)
  expect_equal(adj$target_date, result$forecast_data$period_start[3])
})

test_that("apply_membership_fee_events ignores zero and non-finite deltas", {
  result <- make_breakeven_result()
  fd0 <- result$forecast_data
  events <- data.frame(
    start_date = fd0$period_start[1],
    revenue_delta = c(0, NA_real_),
    stringsAsFactors = FALSE
  )

  adj <- apply_membership_fee_events(result, events)
  expect_equal(adj$forecast_data$revenue_forecast, fd0$revenue_forecast)
})

# -- .solve_combined_growth_rate ----------------------------------------------
# Unit-tested directly against a synthetic periods_at() (rather than only
# indirectly via goal_seek_breakeven()'s real forecast pipeline), so the
# blend-fraction bisection's correctness doesn't depend on any particular
# forecast fixture's actual monotonicity.

test_that(".solve_combined_growth_rate() finds the smallest blend fraction that reaches the horizon", {
  # A simple synthetic model: periods-to-goal decreases linearly as either
  # lever moves toward its target, reaching exactly the horizon (6) when
  # the blend fraction t = 0.5 (income at +5, overhead at -5, halfway
  # between current 0/0 and the solo targets of 10/-10).
  periods_at <- function(income_pct, overhead_pct) {
    # Each lever contributes up to 6 periods of improvement from a
    # baseline of 12, scaled by how far it's moved toward its own solo
    # target (10 for income, -10 for overhead).
    income_progress <- min(1, max(0, income_pct / 10))
    overhead_progress <- min(1, max(0, overhead_pct / -10))
    round(12 - 6 * income_progress - 6 * overhead_progress)
  }

  result <- .solve_combined_growth_rate(
    periods_at = periods_at,
    current_income_growth_pct = 0,
    current_overhead_growth_pct = 0,
    income_target_pct = 10,
    overhead_target_pct = -10,
    horizon_periods = 6L
  )

  expect_true(result$achievable)
  expect_equal(result$new_income_growth_pct, 5, tolerance = 0.2)
  expect_equal(result$new_overhead_growth_pct, -5, tolerance = 0.2)
})

test_that(".solve_combined_growth_rate() reports not achievable when even the full blend (t = 1) misses the horizon", {
  # periods_at() never drops below 10, so no blend fraction reaches a
  # horizon of 6.
  periods_at <- function(income_pct, overhead_pct) 10L

  result <- .solve_combined_growth_rate(
    periods_at = periods_at,
    current_income_growth_pct = 0,
    current_overhead_growth_pct = 0,
    income_target_pct = 10,
    overhead_target_pct = -10,
    horizon_periods = 6L
  )

  expect_false(result$achievable)
})

# -- goal_seek_breakeven ------------------------------------------------------

# Flat-revenue-and-overhead fixture: make_breakeven_result()'s revenue is
# always increasing, which makes it hard to construct a "never reaches
# break-even, but is close enough that a modest growth-rate change would
# fix it" case deterministically -- flat series make the achievable/
# not-achievable boundary easy to reason about directly.
make_flat_breakeven_result <- function(revenue = 1800, overhead = 2000, n_forecast = 12L) {
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
    breakeven_date = as.Date(NA),
    periods_to_breakeven = NA_integer_,
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

test_that("goal_seek_breakeven() reports both levers achievable for a small gap", {
  result <- make_flat_breakeven_result(revenue = 1800, overhead = 2000)

  gs <- goal_seek_breakeven(result)

  expect_s3_class(gs, "dcAnalytics_goal_seek")
  expect_identical(gs$target_period, 12L)
  expect_true(gs$income$achievable)
  expect_true(gs$income$new_growth_pct > 0)
  expect_true(gs$overhead$achievable)
  expect_true(gs$overhead$new_growth_pct < 0)
})

test_that("goal_seek_breakeven() reports a combined lever when both solo levers are achievable", {
  result <- make_flat_breakeven_result(revenue = 1800, overhead = 2000)

  gs <- goal_seek_breakeven(result)

  expect_true(gs$combined$achievable)
  # The blend is strictly partial: neither combined value should reach as
  # far as its own solo counterpart (that would mean the blend fraction
  # hit 1, i.e. no smaller-than-solo change was actually found), and both
  # should still move in the same helpful direction as their solo lever.
  expect_true(gs$combined$new_income_growth_pct > 0)
  expect_true(gs$combined$new_income_growth_pct < gs$income$new_growth_pct)
  expect_true(gs$combined$new_overhead_growth_pct < 0)
  expect_true(gs$combined$new_overhead_growth_pct > gs$overhead$new_growth_pct)
})

test_that("goal_seek_breakeven() reports neither lever achievable for a huge gap", {
  result <- make_flat_breakeven_result(revenue = 500, overhead = 5000)

  gs <- goal_seek_breakeven(result)

  expect_false(gs$income$achievable)
  expect_false(gs$overhead$achievable)
  expect_false(gs$combined$achievable)
})

test_that("goal_seek_breakeven() omits the overhead and combined levers when overhead_flat is set", {
  result <- make_flat_breakeven_result(revenue = 1800, overhead = 2000)

  gs <- goal_seek_breakeven(result, overhead_flat = 2000)

  expect_true(gs$income$achievable)
  expect_null(gs$overhead)
  expect_null(gs$combined)
})

test_that("goal_seek_breakeven() returns NULL for a NULL breakeven_result", {
  expect_null(goal_seek_breakeven(NULL))
})

test_that("goal_seek_breakeven() solves the fee lever when panel_size/membership_fee are supplied", {
  # revenue 1800, overhead 2000: a $200 deficit across a 100-member panel
  # is exactly a $2/member fee increase.
  result <- make_flat_breakeven_result(revenue = 1800, overhead = 2000)

  gs <- goal_seek_breakeven(result, panel_size = 100, membership_fee = 18)

  expect_true(gs$fee$achievable)
  expect_equal(gs$fee$fee_increase, 2, tolerance = 1e-6)
  expect_equal(gs$fee$new_fee, 20, tolerance = 1e-6)
})

test_that("goal_seek_breakeven() omits the fee lever when panel_size/membership_fee aren't supplied", {
  result <- make_flat_breakeven_result(revenue = 1800, overhead = 2000)

  gs <- goal_seek_breakeven(result)

  expect_null(gs$fee)
})

test_that("goal_seek_breakeven() reports the fee lever unachievable for a huge gap", {
  result <- make_flat_breakeven_result(revenue = 500, overhead = 5000)

  gs <- goal_seek_breakeven(result, panel_size = 10, membership_fee = 20)

  expect_false(gs$fee$achievable)
})

test_that("goal_seek_breakeven() attaches a members_equiv annotation to the income lever when panel_size/membership_fee are supplied", {
  result <- make_flat_breakeven_result(revenue = 1800, overhead = 2000)

  gs <- goal_seek_breakeven(result, panel_size = 100, membership_fee = 18)

  expect_true(gs$income$achievable)
  expect_false(is.null(gs$income$members_equiv))
  expect_true(gs$income$members_equiv$per_month >= 1)
  expect_equal(gs$income$members_equiv$fee_used, 18)
})

test_that("goal_seek_breakeven() omits members_equiv when panel_size/membership_fee aren't supplied", {
  result <- make_flat_breakeven_result(revenue = 1800, overhead = 2000)

  gs <- goal_seek_breakeven(result)

  expect_true(gs$income$achievable)
  expect_null(gs$income$members_equiv)
})

# -- .income_growth_members_equiv ---------------------------------------------
# Unit-tested directly against a synthetic recompute() (rather than only
# indirectly via goal_seek_breakeven()'s real forecast pipeline), so the
# revenue-diff/fee-event arithmetic doesn't depend on any particular
# forecast fixture's actual growth-assumption math.

test_that(".income_growth_members_equiv() converts a solved income-growth % into an extra-members/month figure", {
  # Solved scenario adds $1800/month of revenue by the target period; at
  # $15/member and a 12-month horizon, that's 120 member-equivalents
  # total, spread evenly = 10/month.
  recompute <- function(income_pct, overhead_pct) {
    revenue <- if (income_pct > 0) 6800 else 5000
    list(forecast_data = list(revenue_forecast = rep(revenue, 12)))
  }

  result <- .income_growth_members_equiv(
    recompute = recompute,
    current_income_growth_pct = 0,
    current_overhead_growth_pct = 0,
    new_income_growth_pct = 8,
    target_period = 12L,
    panel_size = 100,
    membership_fee = 15,
    frequency = "monthly"
  )

  expect_equal(result$per_month, 10)
  expect_equal(result$fee_used, 15)
  expect_null(result$per_month_planned)
})

test_that(".income_growth_members_equiv() converts weekly revenue to a monthly-equivalent before dividing by fee", {
  # extra_pp = $100/week -> $433/month (x4.33); at $20/member over 12
  # months that's 21.65 member-equivalents total, ~2/month.
  recompute <- function(income_pct, overhead_pct) {
    revenue <- if (income_pct > 0) 1100 else 1000
    list(forecast_data = list(revenue_forecast = rep(revenue, 12)))
  }

  result <- .income_growth_members_equiv(
    recompute = recompute,
    current_income_growth_pct = 0,
    current_overhead_growth_pct = 0,
    new_income_growth_pct = 5,
    target_period = 12L,
    panel_size = 50,
    membership_fee = 20,
    frequency = "weekly"
  )

  expect_equal(result$per_month, 2)
})

test_that(".income_growth_members_equiv() returns NULL when panel_size/membership_fee aren't both supplied", {
  recompute <- function(income_pct, overhead_pct) {
    list(forecast_data = list(revenue_forecast = rep(5000, 12)))
  }

  expect_null(.income_growth_members_equiv(
    recompute, 0, 0, 5, 12L,
    panel_size = NULL, membership_fee = 15, frequency = "monthly"
  ))
  expect_null(.income_growth_members_equiv(
    recompute, 0, 0, 5, 12L,
    panel_size = 100, membership_fee = NULL, frequency = "monthly"
  ))
})

test_that(".income_growth_members_equiv() returns NULL when the solved growth rate implies no positive revenue gap", {
  recompute <- function(income_pct, overhead_pct) {
    list(forecast_data = list(revenue_forecast = rep(5000, 12)))
  }

  result <- .income_growth_members_equiv(
    recompute, 0, 0, 0, 12L,
    panel_size = 100, membership_fee = 15, frequency = "monthly"
  )

  expect_null(result)
})

test_that(".income_growth_members_equiv() adds a planned-fee annotation when a fee event is already active by the target date", {
  # Same $1800/month gap as the first test above. A +$600/month fee
  # event across the 100-member panel raises the average fee from $15
  # to $21, so the planned-fee member count differs from the
  # current-fee one: 1800/21 = 85.71 total, ~7/month.
  recompute <- function(income_pct, overhead_pct) {
    revenue <- if (income_pct > 0) 6800 else 5000
    list(forecast_data = list(revenue_forecast = rep(revenue, 12)))
  }
  fee_events <- data.frame(
    start_date = as.Date("2025-06-01"),
    revenue_delta = 600
  )

  result <- .income_growth_members_equiv(
    recompute = recompute,
    current_income_growth_pct = 0,
    current_overhead_growth_pct = 0,
    new_income_growth_pct = 8,
    target_period = 12L,
    panel_size = 100,
    membership_fee = 15,
    frequency = "monthly",
    fee_events = fee_events,
    target_date = as.Date("2026-01-01")
  )

  expect_equal(result$per_month, 10)
  expect_equal(result$fee_used_planned, 21)
  expect_equal(result$per_month_planned, 7)
})

test_that(".income_growth_members_equiv() ignores fee events that haven't started by the target date", {
  recompute <- function(income_pct, overhead_pct) {
    revenue <- if (income_pct > 0) 6800 else 5000
    list(forecast_data = list(revenue_forecast = rep(revenue, 12)))
  }
  fee_events <- data.frame(
    start_date = as.Date("2027-01-01"),
    revenue_delta = 600
  )

  result <- .income_growth_members_equiv(
    recompute = recompute,
    current_income_growth_pct = 0,
    current_overhead_growth_pct = 0,
    new_income_growth_pct = 8,
    target_period = 12L,
    panel_size = 100,
    membership_fee = 15,
    frequency = "monthly",
    fee_events = fee_events,
    target_date = as.Date("2026-01-01")
  )

  expect_null(result$per_month_planned)
  expect_null(result$fee_used_planned)
})

test_that(".income_growth_members_equiv() skips the planned-fee annotation when the event doesn't change the average fee", {
  recompute <- function(income_pct, overhead_pct) {
    revenue <- if (income_pct > 0) 6800 else 5000
    list(forecast_data = list(revenue_forecast = rep(revenue, 12)))
  }
  fee_events <- data.frame(
    start_date = as.Date("2025-06-01"),
    revenue_delta = 0
  )

  result <- .income_growth_members_equiv(
    recompute = recompute,
    current_income_growth_pct = 0,
    current_overhead_growth_pct = 0,
    new_income_growth_pct = 8,
    target_period = 12L,
    panel_size = 100,
    membership_fee = 15,
    frequency = "monthly",
    fee_events = fee_events,
    target_date = as.Date("2026-01-01")
  )

  expect_null(result$per_month_planned)
})

# -- .format_members_annotation ------------------------------------------------

test_that(".format_members_annotation() renders an empty string for NULL", {
  expect_equal(.format_members_annotation(NULL), "")
})

test_that(".format_members_annotation() renders a single-fee annotation", {
  txt <- .format_members_annotation(list(per_month = 3, fee_used = 18))

  expect_true(grepl(
    "roughly <strong>3 extra members/month</strong> at your current \\$18\\.00/member fee",
    txt
  ))
})

test_that(".format_members_annotation() singularizes 'member' for a pace of exactly 1", {
  txt <- .format_members_annotation(list(per_month = 1, fee_used = 18))

  expect_true(grepl("1 extra member/month", txt, fixed = TRUE))
  expect_false(grepl("1 extra members/month", txt, fixed = TRUE))
})

test_that(".format_members_annotation() renders both current and planned fee when both are present", {
  txt <- .format_members_annotation(list(
    per_month = 10, fee_used = 15,
    per_month_planned = 7, fee_used_planned = 21
  ))

  expect_true(grepl(
    "roughly <strong>10 extra members/month</strong> at your current \\$15\\.00/member fee",
    txt
  ))
  expect_true(grepl(
    "or about <strong>7 extra members/month</strong> once your planned fee change takes effect at about \\$21\\.00/member",
    txt
  ))
})

test_that(".describe_goal_seek() includes the members/month annotation on the income clause", {
  result <- make_flat_breakeven_result(revenue = 1800, overhead = 2000)
  gs <- goal_seek_breakeven(result, panel_size = 100, membership_fee = 18)
  pu <- list(singular = "month", plural = "months", per = "/month", adj = "monthly")

  text <- .describe_goal_seek(gs, pu, "break-even")

  expect_true(grepl(
    "extra member.*/month</strong> at your current \\$18\\.00/member fee",
    text
  ))
})

test_that(".describe_goal_seek() omits the members/month annotation when members_equiv is NULL", {
  result <- make_flat_breakeven_result(revenue = 1800, overhead = 2000)
  gs <- goal_seek_breakeven(result)
  pu <- list(singular = "month", plural = "months", per = "/month", adj = "monthly")

  text <- .describe_goal_seek(gs, pu, "break-even")

  expect_false(grepl("extra member", text, fixed = TRUE))
})

# -- goal_seek_target ----------------------------------------------------------

# Flat-revenue/overhead/required_revenue fixture, same rationale as
# make_flat_breakeven_result() above.
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
    target_date = as.Date(NA),
    periods_to_target = NA_integer_,
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

test_that("goal_seek_target() reports both levers achievable for a small gap", {
  result <- make_flat_target_result(revenue = 1800, required_revenue = 2500, overhead = 2000)

  gs <- goal_seek_target(result, target_income_override = 500)

  expect_s3_class(gs, "dcAnalytics_goal_seek")
  expect_identical(gs$target_period, 12L)
  expect_true(gs$income$achievable)
  expect_true(gs$income$new_growth_pct > 0)
  expect_true(gs$overhead$achievable)
  expect_true(gs$overhead$new_growth_pct < 0)
})

test_that("goal_seek_target() reports neither lever achievable for a huge gap", {
  result <- make_flat_target_result(revenue = 200, required_revenue = 5000, overhead = 2000)

  gs <- goal_seek_target(result, target_income_override = 3000)

  expect_false(gs$income$achievable)
  expect_false(gs$overhead$achievable)
})

test_that("goal_seek_target() omits the overhead lever when overhead_flat is set", {
  result <- make_flat_target_result(revenue = 1800, required_revenue = 2500, overhead = 2000)

  gs <- goal_seek_target(result, overhead_flat = 2000, target_income_override = 500)

  expect_true(gs$income$achievable)
  expect_null(gs$overhead)
})

test_that("goal_seek_target() returns NULL for a NULL target_result", {
  expect_null(goal_seek_target(NULL))
})

test_that("goal_seek_target() solves the fee lever when panel_size/membership_fee are supplied", {
  # revenue 1800, required_revenue 2500: a $700 gap across a 100-member
  # panel is exactly a $7/member fee increase.
  result <- make_flat_target_result(revenue = 1800, required_revenue = 2500, overhead = 2000)

  gs <- goal_seek_target(result, target_income_override = 500, panel_size = 100, membership_fee = 18)

  expect_true(gs$fee$achievable)
  expect_equal(gs$fee$fee_increase, 7, tolerance = 1e-6)
  expect_equal(gs$fee$new_fee, 25, tolerance = 1e-6)
})

test_that("goal_seek_target() omits the fee lever when panel_size/membership_fee aren't supplied", {
  result <- make_flat_target_result(revenue = 1800, required_revenue = 2500, overhead = 2000)

  gs <- goal_seek_target(result, target_income_override = 500)

  expect_null(gs$fee)
})

test_that("goal_seek_target() attaches a members_equiv annotation to the income lever when panel_size/membership_fee are supplied", {
  result <- make_flat_target_result(revenue = 1800, required_revenue = 2500, overhead = 2000)

  gs <- goal_seek_target(result, target_income_override = 500, panel_size = 100, membership_fee = 18)

  expect_true(gs$income$achievable)
  expect_false(is.null(gs$income$members_equiv))
  expect_true(gs$income$members_equiv$per_month >= 1)
  expect_equal(gs$income$members_equiv$fee_used, 18)
})

# -- .describe_goal_seek ------------------------------------------------------

test_that(".describe_goal_seek() describes both levers, plus the combined option, as a bulleted list", {
  result <- make_flat_breakeven_result(revenue = 1800, overhead = 2000)
  gs <- goal_seek_breakeven(result)
  pu <- list(singular = "month", plural = "months", per = "/month", adj = "monthly")

  text <- .describe_goal_seek(gs, pu, "break-even")

  expect_true(grepl("<ul", text, fixed = TRUE))
  expect_true(grepl("any one of the following would do it", text))
  expect_true(grepl("<li>Raise assumed income growth", text, fixed = TRUE))
  expect_true(grepl("<li>Lower assumed overhead growth", text, fixed = TRUE))
  expect_true(grepl(
    "<li>Make a smaller change to both together: raise income growth to about",
    text,
    fixed = TRUE
  ))
  expect_true(grepl("To reach break-even within", text))
})

test_that(".describe_goal_seek() reports non-achievability plainly", {
  result <- make_flat_breakeven_result(revenue = 500, overhead = 5000)
  gs <- goal_seek_breakeven(result)
  pu <- list(singular = "month", plural = "months", per = "/month", adj = "monthly")

  text <- .describe_goal_seek(gs, pu, "break-even")

  expect_true(grepl("wouldn't reach break-even", text))
  expect_false(grepl("<ul", text, fixed = TRUE))
})

test_that(".describe_goal_seek() returns NULL for a NULL goal-seek result", {
  pu <- list(singular = "month", plural = "months", per = "/month", adj = "monthly")
  expect_null(.describe_goal_seek(NULL, pu, "break-even"))
})

test_that(".describe_goal_seek() interpolates a custom goal_label", {
  result <- make_flat_breakeven_result(revenue = 1800, overhead = 2000)
  gs <- goal_seek_breakeven(result)
  pu <- list(singular = "month", plural = "months", per = "/month", adj = "monthly")

  text <- .describe_goal_seek(gs, pu, "the income target")

  expect_true(grepl("To reach the income target within", text))
  expect_false(grepl("break-even", text))
})

test_that(".describe_goal_seek() describes all four levers (income/overhead/fee/combined) as bullets", {
  result <- make_flat_breakeven_result(revenue = 1800, overhead = 2000)
  gs <- goal_seek_breakeven(result, panel_size = 100, membership_fee = 18)
  pu <- list(singular = "month", plural = "months", per = "/month", adj = "monthly")

  text <- .describe_goal_seek(gs, pu, "break-even")

  expect_true(grepl("any one of the following would do it", text))
  expect_true(grepl("<li>Raise assumed income growth", text, fixed = TRUE))
  expect_true(grepl("<li>Lower assumed overhead growth", text, fixed = TRUE))
  expect_true(grepl(
    "Raise your average membership fee by about <strong>\\$2\\.00/member/month</strong> \\(to about <strong>\\$20\\.00/member/month</strong>\\)",
    text
  ))
  expect_true(grepl("Make a smaller change to both together", text, fixed = TRUE))
})

test_that(".describe_goal_seek() falls back to a single-sentence, non-bulleted form for exactly one achievable lever", {
  # overhead_flat set -- see goal_seek_breakeven()'s own doc -- makes the
  # overhead lever unsearchable, leaving income as the only lever (no fee
  # lever, since panel_size/membership_fee aren't supplied either), so
  # this exercises the length(clauses) == 1L branch, not the <ul> one.
  result <- make_flat_breakeven_result(revenue = 1800, overhead = 2000)
  gs <- goal_seek_breakeven(result, overhead_flat = 2000)
  pu <- list(singular = "month", plural = "months", per = "/month", adj = "monthly")

  text <- .describe_goal_seek(gs, pu, "break-even")

  expect_false(grepl("<ul", text, fixed = TRUE))
  expect_true(grepl("on this assumption alone, you'd need to raise assumed income growth", text))
})

test_that(".describe_goal_seek() omits the combined lever when only one solo lever is achievable", {
  # overhead_flat makes overhead unsearchable -- gs$combined should never
  # even be computed (goal_seek_breakeven()/.goal_seek_growth_rate() both
  # gate combined's construction on overhead_searchable), so there's
  # nothing for .describe_goal_seek() to describe beyond income alone.
  result <- make_flat_breakeven_result(revenue = 1800, overhead = 2000)
  gs <- goal_seek_breakeven(result, overhead_flat = 2000)

  expect_null(gs$combined)
})

test_that(".describe_goal_seek() carries a Pro badge on both the achievable and non-achievable branches by default", {
  pu <- list(singular = "month", plural = "months", per = "/month", adj = "monthly")

  achievable <- .describe_goal_seek(
    goal_seek_breakeven(make_flat_breakeven_result(revenue = 1800, overhead = 2000)),
    pu, "break-even"
  )
  expect_true(grepl("badge", achievable))
  expect_true(grepl(">Pro<", achievable))

  not_achievable <- .describe_goal_seek(
    goal_seek_breakeven(make_flat_breakeven_result(revenue = 500, overhead = 5000)),
    pu, "break-even"
  )
  expect_true(grepl("badge", not_achievable))
  expect_true(grepl(">Pro<", not_achievable))
})

test_that(".describe_goal_seek() omits the Pro badge entirely when badge = FALSE, for both branches", {
  pu <- list(singular = "month", plural = "months", per = "/month", adj = "monthly")

  achievable <- .describe_goal_seek(
    goal_seek_breakeven(make_flat_breakeven_result(revenue = 1800, overhead = 2000)),
    pu, "break-even",
    badge = FALSE
  )
  expect_false(grepl("badge", achievable))
  expect_false(grepl(">Pro<", achievable))
  # The narrative content itself must still be present -- badge = FALSE
  # only drops the badge prefix, nothing else.
  expect_true(grepl("Raise assumed income growth", achievable))

  not_achievable <- .describe_goal_seek(
    goal_seek_breakeven(make_flat_breakeven_result(revenue = 500, overhead = 5000)),
    pu, "break-even",
    badge = FALSE
  )
  expect_false(grepl("badge", not_achievable))
  expect_false(grepl(">Pro<", not_achievable))
})

# -- stress_test_breakeven -----------------------------------------------------

test_that("stress_test_breakeven() reports a positive, uncapped loss room for a sustained surplus", {
  result <- make_breakeven_result(
    periods_to_breakeven = 0L,
    revenue_start = 3000,
    overhead = 2000
  )

  st <- stress_test_breakeven(result, panel_size = 60, membership_fee = 50)

  expect_s3_class(st, "dcAnalytics_stress_test")
  expect_false(st$capped)
  expect_true(st$members_loss_room > 0)
  expect_true(st$members_loss_room < 60)
})

test_that("stress_test_breakeven() caps at the full panel when the margin is very wide", {
  result <- make_breakeven_result(
    periods_to_breakeven = 0L,
    revenue_start = 100000,
    overhead = 2000
  )

  st <- stress_test_breakeven(result, panel_size = 10, membership_fee = 50)

  expect_true(st$capped)
  expect_identical(st$members_loss_room, 10)
})

test_that("stress_test_breakeven() returns NULL when break-even isn't yet achieved", {
  result <- make_breakeven_result(periods_to_breakeven = 3L)
  expect_null(stress_test_breakeven(result, panel_size = 50, membership_fee = 50))
})

test_that("stress_test_breakeven() returns NULL when panel_size/membership_fee are missing", {
  result <- make_breakeven_result(periods_to_breakeven = 0L, revenue_start = 3000, overhead = 2000)

  expect_null(stress_test_breakeven(result, panel_size = NULL, membership_fee = 50))
  expect_null(stress_test_breakeven(result, panel_size = 50, membership_fee = NULL))
  expect_null(stress_test_breakeven(result, panel_size = 0, membership_fee = 50))
})

test_that("stress_test_breakeven() returns NULL when the achieved state isn't sustained", {
  result <- make_breakeven_result(
    periods_to_breakeven = 0L,
    revenue_start = 2100,
    overhead = 2000
  )
  result$forecast_data$overhead_forecast <- rep(5000, 12)

  expect_null(stress_test_breakeven(result, panel_size = 50, membership_fee = 50))
})

test_that("stress_test_breakeven() returns NULL for a NULL result", {
  expect_null(stress_test_breakeven(NULL, panel_size = 50, membership_fee = 50))
})

# -- .describe_stress_test ------------------------------------------------------

test_that(".describe_stress_test() describes a normal loss-room figure", {
  st <- structure(list(members_loss_room = 15L, capped = FALSE), class = "dcAnalytics_stress_test")

  text <- .describe_stress_test(st, list(singular = "month", plural = "months"))

  expect_true(grepl("could absorb losing up to <strong>15 members</strong>", text))
})

test_that(".describe_stress_test() flags a thin margin at zero loss room", {
  st <- structure(list(members_loss_room = 0L, capped = FALSE), class = "dcAnalytics_stress_test")

  text <- .describe_stress_test(st, list(singular = "month", plural = "months"))

  expect_true(grepl("margin is thin", text))
})

test_that(".describe_stress_test() flags a capped (full-panel) margin", {
  st <- structure(list(members_loss_room = 60L, capped = TRUE), class = "dcAnalytics_stress_test")

  text <- .describe_stress_test(st, list(singular = "month", plural = "months"))

  expect_true(grepl("entire panel", text))
})

test_that(".describe_stress_test() returns NULL for a NULL stress-test result", {
  expect_null(.describe_stress_test(NULL, list(singular = "month", plural = "months")))
})

test_that(".describe_stress_test() carries a Pro badge on all three non-NULL branches", {
  pu <- list(singular = "month", plural = "months")

  normal <- .describe_stress_test(
    structure(list(members_loss_room = 15L, capped = FALSE), class = "dcAnalytics_stress_test"), pu
  )
  thin <- .describe_stress_test(
    structure(list(members_loss_room = 0L, capped = FALSE), class = "dcAnalytics_stress_test"), pu
  )
  capped <- .describe_stress_test(
    structure(list(members_loss_room = 60L, capped = TRUE), class = "dcAnalytics_stress_test"), pu
  )

  for (text in list(normal, thin, capped)) {
    expect_true(grepl("badge", text))
    expect_true(grepl(">Pro<", text))
  }
})

test_that(".describe_stress_test() omits the Pro badge when badge = FALSE, on all three branches", {
  pu <- list(singular = "month", plural = "months")

  normal <- .describe_stress_test(
    structure(list(members_loss_room = 15L, capped = FALSE), class = "dcAnalytics_stress_test"),
    pu, badge = FALSE
  )
  thin <- .describe_stress_test(
    structure(list(members_loss_room = 0L, capped = FALSE), class = "dcAnalytics_stress_test"),
    pu, badge = FALSE
  )
  capped <- .describe_stress_test(
    structure(list(members_loss_room = 60L, capped = TRUE), class = "dcAnalytics_stress_test"),
    pu, badge = FALSE
  )

  for (text in list(normal, thin, capped)) {
    expect_false(grepl("badge", text))
    expect_false(grepl(">Pro<", text))
  }
  expect_true(grepl("could absorb losing up to <strong>15 members</strong>", normal))
})
