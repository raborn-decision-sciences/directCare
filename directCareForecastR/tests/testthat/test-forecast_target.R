make_monthly_income <- function(n = 6, start = 500, end = 4000) {
  tibble::tibble(
    practice_id = rep(1, n),
    year = rep(2025, n),
    month = seq_len(n),
    total_revenue = seq(start, end, length.out = n)
  )
}

make_monthly_overhead <- function(n = 6, overhead = 2000) {
  tibble::tibble(
    practice_id = rep(1, n),
    year = rep(2025, n),
    month = seq_len(n),
    total_overhead = rep(overhead, n),
    gross_overhead = rep(overhead, n),
    total_refunds = rep(0, n)
  )
}

# --- Structure ----------------------------------------------------------------

test_that("forecast_target returns the correct list structure", {
  result <- forecast_target(
    make_monthly_income(),
    make_monthly_overhead(),
    target_income = 500
  )

  expect_type(result, "list")
  expect_named(
    result,
    c(
      "target_date",
      "periods_to_target",
      "current_gap",
      "required_revenue_now",
      "confidence_interval",
      "forecast_data",
      "target_income",
      "method",
      "frequency",
      "data_warnings"
    )
  )
})

test_that("forecast_target forecast_data has the correct columns", {
  result <- forecast_target(
    make_monthly_income(),
    make_monthly_overhead(),
    target_income = 500
  )

  expect_s3_class(result$forecast_data, "data.frame")
  expect_true(all(
    c(
      "period_start",
      "revenue_forecast",
      "revenue_lower",
      "revenue_upper",
      "overhead_forecast",
      "required_revenue",
      "net_vs_target"
    ) %in%
      names(result$forecast_data)
  ))
})

# --- required_revenue calculation ---------------------------------------------

test_that("required_revenue equals overhead_forecast + target_income", {
  result <- forecast_target(
    make_monthly_income(),
    make_monthly_overhead(overhead = 2000),
    target_income = 500
  )

  expect_equal(
    result$forecast_data$required_revenue,
    result$forecast_data$overhead_forecast + 500
  )
})

test_that("net_vs_target equals revenue_forecast minus required_revenue", {
  result <- forecast_target(
    make_monthly_income(),
    make_monthly_overhead(),
    target_income = 500
  )

  expect_equal(
    result$forecast_data$net_vs_target,
    result$forecast_data$revenue_forecast -
      result$forecast_data$required_revenue
  )
})

# --- required_revenue_now and current_gap ------------------------------------

test_that("required_revenue_now is the latest overhead plus target_income", {
  result <- forecast_target(
    make_monthly_income(),
    make_monthly_overhead(overhead = 2000),
    target_income = 500
  )
  expect_equal(result$required_revenue_now, 2500)
})

test_that("current_gap is negative when revenue has not yet hit target", {
  income <- make_monthly_income(n = 6, start = 500, end = 1000) # low revenue
  # Revenue never crosses required (2000 + 500 = 2500) within horizon; suppress expected warning
  result <- suppressWarnings(
    forecast_target(
      income,
      make_monthly_overhead(overhead = 2000),
      target_income = 500
    )
  )
  expect_lt(result$current_gap, 0)
})

test_that("current_gap is positive when revenue already exceeds the target threshold", {
  income <- make_monthly_income(n = 6, start = 3000, end = 5000) # high revenue
  result <- forecast_target(
    income,
    make_monthly_overhead(overhead = 1000),
    target_income = 500
  )
  expect_gt(result$current_gap, 0)
})

# --- Target detection ---------------------------------------------------------

test_that("forecast_target finds a target date for an achievable target", {
  # Revenue trending up from 500 → 4000; overhead flat at 2000; target = 500
  # Required = 2500, revenue will cross that within the horizon
  result <- forecast_target(
    make_monthly_income(n = 6, start = 500, end = 4000),
    make_monthly_overhead(overhead = 2000),
    target_income = 500,
    horizon = 12
  )

  expect_false(is.na(result$target_date))
  expect_false(is.na(result$periods_to_target))
  expect_s3_class(result$target_date, "Date")
  expect_type(result$periods_to_target, "integer")
})

test_that("target_date is consistent with periods_to_target", {
  result <- forecast_target(
    make_monthly_income(n = 6, start = 500, end = 4000),
    make_monthly_overhead(overhead = 2000),
    target_income = 500,
    horizon = 12
  )

  if (!is.na(result$target_date)) {
    expect_equal(
      result$forecast_data$period_start[result$periods_to_target],
      result$target_date
    )
  }
})

test_that("forecast_target warns when target is not reached within horizon", {
  # Revenue flat at 500; required = 2000 + 5000 = 7000 — never reached
  income <- make_monthly_income(n = 6, start = 500, end = 500)
  expect_warning(
    result <- forecast_target(
      income,
      make_monthly_overhead(overhead = 2000),
      target_income = 5000,
      horizon = 6
    ),
    class = "dcForecastR_target_not_reached"
  )

  expect_true(is.na(result$target_date))
  expect_true(is.na(result$periods_to_target))
})

# --- Horizon and frequency ----------------------------------------------------

test_that("forecast_target defaults to 12 periods for monthly data", {
  result <- forecast_target(
    make_monthly_income(),
    make_monthly_overhead(),
    target_income = 500
  )
  expect_equal(nrow(result$forecast_data), 12)
})

test_that("forecast_target respects an explicit horizon", {
  result <- forecast_target(
    make_monthly_income(),
    make_monthly_overhead(),
    target_income = 500,
    horizon = 6
  )
  expect_equal(nrow(result$forecast_data), 6)
})

test_that("forecast_target detects frequency and records method", {
  result <- forecast_target(
    make_monthly_income(),
    make_monthly_overhead(),
    target_income = 500,
    method = "linear"
  )
  expect_equal(result$frequency, "monthly")
  expect_equal(result$method, "linear")
})

test_that("forecast_target defaults to 52 periods for weekly income data", {
  income_weekly <- tibble::tibble(
    practice_id = rep(1, 20),
    week_start = seq(as.Date("2025-01-06"), by = "week", length.out = 20),
    revenue = seq(300, 2000, length.out = 20)
  )
  overhead_monthly <- make_monthly_overhead(n = 5, overhead = 1500)

  expect_warning(
    result <- forecast_target(
      income_weekly,
      overhead_monthly,
      target_income = 200
    ),
    "weekly.*monthly|monthly.*weekly"
  )

  expect_equal(nrow(result$forecast_data), 52)
  expect_equal(result$frequency, "weekly")
})

# --- target_income passthrough ------------------------------------------------

test_that("forecast_target echoes target_income in return value", {
  result <- forecast_target(
    make_monthly_income(),
    make_monthly_overhead(),
    target_income = 1234
  )
  expect_equal(result$target_income, 1234)
})

# --- Input validation ---------------------------------------------------------

test_that("forecast_target errors on non-numeric target_income", {
  expect_error(
    forecast_target(
      make_monthly_income(),
      make_monthly_overhead(),
      target_income = "high"
    ),
    class = "dcForecastR_invalid_target"
  )
})

test_that("forecast_target errors on vector target_income", {
  expect_error(
    forecast_target(
      make_monthly_income(),
      make_monthly_overhead(),
      target_income = c(100, 200)
    ),
    class = "dcForecastR_invalid_target"
  )
})

# --- Confidence intervals -----------------------------------------------------

test_that("forecast_target confidence interval bounds are dates or NA", {
  result <- forecast_target(
    make_monthly_income(n = 6, start = 500, end = 4000),
    make_monthly_overhead(overhead = 2000),
    target_income = 500
  )

  ci <- result$confidence_interval
  expect_length(ci, 2)
  expect_named(ci, c("lower", "upper"))
  # Each bound is either a Date or NA
  check_date_or_na <- function(x) is.na(x) || inherits(x, "Date")
  expect_true(check_date_or_na(ci["lower"]))
  expect_true(check_date_or_na(ci["upper"]))
})

# --- Multi-practice guard -----------------------------------------------------

test_that("forecast_target errors on multi-practice income", {
  income <- tibble::tibble(
    practice_id = c(rep(1, 6), rep(2, 6)),
    year = rep(2025, 12),
    month = rep(1:6, 2),
    total_revenue = rep(2000, 12)
  )
  expect_snapshot(
    forecast_target(
      income,
      make_monthly_overhead(),
      target_income = 1000,
      method = "linear"
    ),
    error = TRUE
  )
})

test_that("forecast_target errors on multi-practice overhead", {
  overhead <- tibble::tibble(
    practice_id = c(rep(1, 6), rep(2, 6)),
    year = rep(2025, 12),
    month = rep(1:6, 2),
    total_overhead = rep(1500, 12),
    gross_overhead = rep(1500, 12),
    total_refunds = rep(0, 12)
  )
  expect_snapshot(
    forecast_target(
      make_monthly_income(),
      overhead,
      target_income = 1000,
      method = "linear"
    ),
    error = TRUE
  )
})

# --- Weekly income + monthly overhead -----------------------------------------

test_that("required_revenue_now includes overhead for weekly income + monthly overhead", {
  # $2 000/month overhead ~ $462/week. target_income = $200/week.
  # required_revenue_now must be ~$662/week, not just $200 (the old bug).
  income_weekly <- tibble::tibble(
    practice_id = rep(1, 12),
    week_start = seq(as.Date("2025-01-06"), by = "week", length.out = 12),
    revenue = rep(300, 12)
  )
  overhead_monthly <- tibble::tibble(
    practice_id = rep(1, 3),
    year = rep(2025, 3),
    month = 1:3,
    total_overhead = rep(2000, 3),
    gross_overhead = rep(2000, 3),
    total_refunds = rep(0, 3)
  )

  result <- suppressWarnings(
    forecast_target(income_weekly, overhead_monthly, target_income = 200)
  )

  # required_revenue_now must exceed target_income by at least the overhead
  expect_gt(result$required_revenue_now, 200 + 400) # 400 ≈ 2000 / 4.33 * 0.9 (conservative)
  # And current_gap must be negative: $300/week revenue < ~$662 required
  expect_lt(result$current_gap, 0)
})
