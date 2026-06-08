test_that(".detect_frequency identifies weekly data correctly", {
  # Weekly data has week_start column
  weekly_data <- tibble::tibble(
    practice_id = 1,
    week_start = as.Date("2025-01-01"),
    revenue = 1000,
    month = 1,  # Even if it has month, week_start takes precedence
    year = 2025
  )
  
  expect_equal(.detect_frequency(weekly_data), "weekly")
})

test_that(".detect_frequency identifies monthly data correctly", {
  # Monthly data has year and month columns
  monthly_data <- tibble::tibble(
    practice_id = 1,
    year = 2025,
    month = 1,
    total_revenue = 1000
  )
  
  expect_equal(.detect_frequency(monthly_data), "monthly")
})

test_that(".prepare_income handles monthly data correctly", {
  monthly_income <- tibble::tibble(
    practice_id = rep(1, 3),
    year = rep(2025, 3),
    month = 1:3,
    total_revenue = c(1000, 1500, 2000)
  )
  
  result <- .prepare_income(monthly_income)
  
  # Check structure
  expect_type(result, "list")
  expect_named(result, c("data", "frequency", "periods_per_year"))
  
  # Check values
  expect_equal(result$frequency, "monthly")
  expect_equal(result$periods_per_year, 12)
  
  # Check data
  expect_s3_class(result$data, "data.frame")
  expect_true("period_start" %in% names(result$data))
  expect_true("revenue" %in% names(result$data))
  expect_equal(nrow(result$data), 3)
})

test_that(".prepare_income handles weekly data correctly", {
  weeks <- seq(as.Date("2025-01-01"), by = "week", length.out = 4)
  weekly_income <- tibble::tibble(
    practice_id = rep(1, 4),
    week_start = weeks,
    revenue = c(500, 600, 700, 800)
  )
  
  result <- .prepare_income(weekly_income)
  
  # Check values
  expect_equal(result$frequency, "weekly")
  expect_equal(result$periods_per_year, 52)
  
  # Check data
  expect_equal(nrow(result$data), 4)
  expect_equal(result$data$revenue, c(500, 600, 700, 800))
})

test_that(".prepare_overhead handles monthly data correctly", {
  monthly_overhead <- tibble::tibble(
    practice_id = rep(1, 3),
    year = rep(2025, 3),
    month = 1:3,
    total_overhead = c(1000, 1000, 1000),
    gross_overhead = c(1000, 1000, 1000),
    total_refunds = c(0, 0, 0)
  )
  
  result <- .prepare_overhead(monthly_overhead)
  
  # Check structure
  expect_type(result, "list")
  expect_equal(result$frequency, "monthly")
  expect_equal(result$periods_per_year, 12)
  
  # Check data
  expect_true("period_start" %in% names(result$data))
  expect_true("overhead" %in% names(result$data))
  expect_equal(result$data$overhead, c(1000, 1000, 1000))
})

test_that(".forecast_series returns correct structure", {
  test_series <- c(100, 120, 140, 160, 180, 200)
  
  result <- .forecast_series(
    test_series,
    method = "linear",
    horizon = 6,
    frequency = 12,
    level = 0.95
  )
  
  # Check structure
  expect_type(result, "list")
  expect_named(result, c("point", "lower", "upper"))
  
  # Check lengths
  expect_equal(length(result$point), 6)
  expect_equal(length(result$lower), 6)
  expect_equal(length(result$upper), 6)
  
  # Check that point forecast is between lower and upper
  expect_true(all(result$lower <= result$point))
  expect_true(all(result$point <= result$upper))
})

test_that(".forecast_series handles insufficient data gracefully", {
  # Only 1 data point
  test_series <- c(100)
  
  result <- .forecast_series(
    test_series,
    method = "linear",
    horizon = 3,
    frequency = 12
  )
  
  # Should return flat forecast
  expect_equal(length(result$point), 3)
  expect_equal(result$point, rep(100, 3))
  expect_equal(result$lower, rep(100, 3))
  expect_equal(result$upper, rep(100, 3))
})

test_that(".forecast_linear produces increasing forecasts for increasing data", {
  # Clearly increasing trend
  test_series <- seq(100, 200, by = 20)
  ts_obj <- ts(test_series, frequency = 12)
  
  result <- .forecast_linear(ts_obj, horizon = 5, level = 0.95)
  
  # Point forecasts should be increasing
  expect_true(all(diff(result$point) > 0))
  
  # First forecast should be greater than last observation
  expect_true(result$point[1] > test_series[length(test_series)])
})

test_that(".forecast_linear produces confidence intervals", {
  test_series <- seq(100, 200, by = 20)
  ts_obj <- ts(test_series, frequency = 12)
  
  result <- .forecast_linear(ts_obj, horizon = 5, level = 0.95)
  
  # Confidence intervals should not shrink over time (increasing uncertainty)
  # Use a small tolerance to handle floating-point precision
  interval_widths <- result$upper - result$lower
  expect_true(all(diff(interval_widths) >= -1e-10))
})
