test_that("forecast_breakeven works with monthly data", {
  # Create sample monthly income data
  income_monthly <- tibble::tibble(
    practice_id = rep(1, 6),
    year = rep(2025, 6),
    month = 1:6,
    total_revenue = c(500, 800, 1200, 1800, 2500, 3000)
  )
  
  # Create sample monthly overhead data
  overhead_monthly <- tibble::tibble(
    practice_id = rep(1, 6),
    year = rep(2025, 6),
    month = 1:6,
    total_overhead = rep(2000, 6),
    gross_overhead = rep(2000, 6),
    total_refunds = rep(0, 6)
  )
  
  result <- forecast_breakeven(
    income_monthly,
    overhead_monthly,
    method = "linear",
    horizon = 12
  )
  
  # Check structure
  expect_type(result, "list")
  expect_named(result, c("breakeven_date", "periods_to_breakeven",
                         "current_surplus_deficit", "current_revenue",
                         "current_overhead", "current_overhead_avg",
                         "overhead_avg_n", "confidence_interval",
                         "forecast_data", "method", "frequency",
                         "data_warnings"))
  
  # Check frequency detection
  expect_equal(result$frequency, "monthly")
  
  # Check method
  expect_equal(result$method, "linear")
  
  # Check forecast_data
  expect_s3_class(result$forecast_data, "data.frame")
  expect_equal(nrow(result$forecast_data), 12)
  expect_true(all(c("period_start", "revenue_forecast", "overhead_forecast", 
                    "net_forecast") %in% names(result$forecast_data)))
})

test_that("forecast_breakeven works with weekly data", {
  # Create sample weekly income data
  weeks <- seq(as.Date("2025-01-01"), by = "week", length.out = 20)
  income_weekly <- tibble::tibble(
    practice_id = rep(1, 20),
    week_start = weeks,
    revenue = seq(500, 2000, length.out = 20)
  )
  
  # Create sample monthly overhead (will be converted to weekly)
  overhead_monthly <- tibble::tibble(
    practice_id = rep(1, 5),
    year = rep(2025, 5),
    month = 1:5,
    total_overhead = rep(1500, 5),
    gross_overhead = rep(1500, 5),
    total_refunds = rep(0, 5)
  )
  
  expect_snapshot(
    result <- forecast_breakeven(
      income_weekly,
      overhead_monthly,
      method = "linear",
      horizon = 52
    )
  )

  result <- suppressWarnings(
    forecast_breakeven(income_weekly, overhead_monthly, method = "linear", horizon = 52)
  )
  expect_equal(result$frequency, "weekly")
  expect_equal(nrow(result$forecast_data), 52)
})

test_that("forecast_breakeven detects break-even point", {
  # Create data that will cross break-even
  income_monthly <- tibble::tibble(
    practice_id = rep(1, 6),
    year = rep(2025, 6),
    month = 1:6,
    total_revenue = seq(1000, 3000, length.out = 6)
  )
  
  overhead_monthly <- tibble::tibble(
    practice_id = rep(1, 6),
    year = rep(2025, 6),
    month = 1:6,
    total_overhead = rep(2500, 6),
    gross_overhead = rep(2500, 6),
    total_refunds = rep(0, 6)
  )
  
  result <- forecast_breakeven(
    income_monthly,
    overhead_monthly,
    method = "linear",
    horizon = 12
  )
  
  # Should find a break-even point
  expect_false(is.na(result$breakeven_date))
  expect_false(is.na(result$periods_to_breakeven))
  expect_s3_class(result$breakeven_date, "Date")
  expect_type(result$periods_to_breakeven, "integer")
})

test_that("forecast_breakeven warns when break-even not reached", {
  income_monthly <- tibble::tibble(
    practice_id = rep(1, 6),
    year = rep(2025, 6),
    month = 1:6,
    total_revenue = rep(500, 6)
  )
  overhead_monthly <- tibble::tibble(
    practice_id = rep(1, 6),
    year = rep(2025, 6),
    month = 1:6,
    total_overhead = rep(2000, 6),
    gross_overhead = rep(2000, 6),
    total_refunds = rep(0, 6)
  )

  expect_snapshot(
    forecast_breakeven(income_monthly, overhead_monthly, method = "linear", horizon = 6)
  )

  result <- suppressWarnings(
    forecast_breakeven(income_monthly, overhead_monthly, method = "linear", horizon = 6)
  )
  expect_true(is.na(result$breakeven_date))
  expect_true(is.na(result$periods_to_breakeven))
})

test_that("forecast_breakeven calculates current surplus/deficit", {
  income_monthly <- tibble::tibble(
    practice_id = rep(1, 6),
    year = rep(2025, 6),
    month = 1:6,
    total_revenue = c(1000, 1500, 2000, 2500, 3000, 3500)
  )
  overhead_monthly <- tibble::tibble(
    practice_id = rep(1, 6),
    year = rep(2025, 6),
    month = 1:6,
    total_overhead = rep(2000, 6),
    gross_overhead = rep(2000, 6),
    total_refunds = rep(0, 6)
  )

  result <- forecast_breakeven(income_monthly, overhead_monthly, method = "linear")

  expect_type(result$current_surplus_deficit, "double")
  # Last month revenue 3500 > overhead 2000 → already profitable
  expect_gt(result$current_surplus_deficit, 0)
})

test_that("forecast_breakeven returns last observed date when already profitable", {
  income_monthly <- tibble::tibble(
    practice_id = rep(1, 6),
    year = rep(2025, 6),
    month = 1:6,
    total_revenue = rep(5000, 6)  # always above overhead
  )
  overhead_monthly <- tibble::tibble(
    practice_id = rep(1, 6),
    year = rep(2025, 6),
    month = 1:6,
    total_overhead = rep(2000, 6),
    gross_overhead = rep(2000, 6),
    total_refunds = rep(0, 6)
  )

  result <- forecast_breakeven(income_monthly, overhead_monthly, method = "linear")

  expect_equal(result$periods_to_breakeven, 0L)
  expect_equal(result$breakeven_date, as.Date("2025-06-01"))
  expect_equal(result$confidence_interval[["lower"]], as.Date("2025-06-01"))
  expect_equal(result$confidence_interval[["upper"]], as.Date("2025-06-01"))
})

test_that("forecast_breakeven accepts different methods", {
  income_monthly <- tibble::tibble(
    practice_id = rep(1, 12),
    year = rep(2025, 12),
    month = 1:12,
    total_revenue = seq(1000, 3000, length.out = 12)
  )
  
  overhead_monthly <- tibble::tibble(
    practice_id = rep(1, 12),
    year = rep(2025, 12),
    month = 1:12,
    total_overhead = rep(2000, 12),
    gross_overhead = rep(2000, 12),
    total_refunds = rep(0, 12)
  )
  
  # Linear should always work
  result_linear <- forecast_breakeven(
    income_monthly, overhead_monthly, method = "linear"
  )
  expect_equal(result_linear$method, "linear")
  
  # ETS and ARIMA require the forecast package
  skip_if_not_installed("forecast")
  
  result_ets <- forecast_breakeven(
    income_monthly, overhead_monthly, method = "ets"
  )
  expect_equal(result_ets$method, "ets")
  
  result_arima <- forecast_breakeven(
    income_monthly, overhead_monthly, method = "arima"
  )
  expect_equal(result_arima$method, "arima")
})

# --- Multi-practice guard -----------------------------------------------------

test_that("forecast_breakeven errors on multi-practice income", {
  income <- tibble::tibble(
    practice_id = c(rep(1, 6), rep(2, 6)),
    year = rep(2025, 12), month = rep(1:6, 2),
    total_revenue = rep(2000, 12)
  )
  overhead <- tibble::tibble(
    practice_id = rep(1, 6), year = rep(2025, 6), month = 1:6,
    total_overhead = rep(1500, 6), gross_overhead = rep(1500, 6), total_refunds = rep(0, 6)
  )
  expect_snapshot(forecast_breakeven(income, overhead, method = "linear"), error = TRUE)
})

test_that("forecast_breakeven errors on multi-practice overhead", {
  income <- tibble::tibble(
    practice_id = rep(1, 6), year = rep(2025, 6), month = 1:6,
    total_revenue = rep(2000, 6)
  )
  overhead <- tibble::tibble(
    practice_id = c(rep(1, 6), rep(2, 6)),
    year = rep(2025, 12), month = rep(1:6, 2),
    total_overhead = rep(1500, 12), gross_overhead = rep(1500, 12), total_refunds = rep(0, 12)
  )
  expect_snapshot(forecast_breakeven(income, overhead, method = "linear"), error = TRUE)
})

# --- Weekly income + monthly overhead -----------------------------------------

test_that("current_surplus_deficit uses rolling overhead avg for weekly income + monthly overhead", {
  # Practice with weekly income trending up and flat monthly overhead.
  # Overhead is $2 000/month ~ $462/week. Income starts at $200/week (deficit).
  # The old bug: full_join zeros out overhead for most weeks, making
  # current_surplus_deficit = latest weekly revenue > 0 → false "already at
  # breakeven". The fix uses the pre-join overhead rolling average instead.
  income_weekly <- tibble::tibble(
    practice_id = rep(1, 20),
    week_start  = seq(as.Date("2025-01-06"), by = "week", length.out = 20),
    revenue     = seq(200, 400, length.out = 20)
  )
  overhead_monthly <- tibble::tibble(
    practice_id    = rep(1, 5),
    year           = rep(2025, 5),
    month          = 1:5,
    total_overhead = rep(2000, 5),
    gross_overhead = rep(2000, 5),
    total_refunds  = rep(0, 5)
  )

  result <- suppressWarnings(
    forecast_breakeven(income_weekly, overhead_monthly, method = "linear")
  )

  # current_surplus_deficit must be negative: ~$400/week revenue vs ~$462/week overhead
  expect_lt(result$current_surplus_deficit, 0)
  # And therefore break-even should NOT be reported as already achieved
  expect_false(identical(result$periods_to_breakeven, 0L))
})

test_that("current_overhead_avg is non-zero for weekly income + monthly overhead", {
  income_weekly <- tibble::tibble(
    practice_id = rep(1, 12),
    week_start  = seq(as.Date("2025-01-06"), by = "week", length.out = 12),
    revenue     = rep(300, 12)
  )
  overhead_monthly <- tibble::tibble(
    practice_id    = rep(1, 3),
    year           = rep(2025, 3),
    month          = 1:3,
    total_overhead = rep(1500, 3),
    gross_overhead = rep(1500, 3),
    total_refunds  = rep(0, 3)
  )

  result <- suppressWarnings(
    forecast_breakeven(income_weekly, overhead_monthly, method = "linear")
  )

  # Rolling average should be close to 1500 / 4.33 ≈ 346/week, not 0
  expect_gt(result$current_overhead_avg, 300)
})
