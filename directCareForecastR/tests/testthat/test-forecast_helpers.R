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

test_that(".forecast_series warns and returns flat forecast when n < 2", {
  expect_warning(
    result <- .forecast_series(c(100), method = "linear", horizon = 3, frequency = 12),
    class = "dcForecastR_insufficient_data"
  )
  expect_equal(result$point, rep(100, 3))
  expect_equal(result$lower, rep(100, 3))
  expect_equal(result$upper, rep(100, 3))
})

test_that(".forecast_series returns finite flat forecast for zero-obs series", {
  expect_warning(
    result <- .forecast_series(numeric(0), method = "linear", horizon = 3, frequency = 12),
    class = "dcForecastR_insufficient_data"
  )
  expect_true(all(is.finite(result$point)))
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

test_that(".prepare_income sorts by period_start regardless of input order", {
  shuffled <- tibble::tibble(
    practice_id   = rep(1, 4),
    year          = rep(2025, 4),
    month         = c(3L, 1L, 4L, 2L),
    total_revenue = c(300, 100, 400, 200)
  )

  result <- .prepare_income(shuffled)

  expect_equal(result$data$period_start, sort(result$data$period_start))
  expect_equal(result$data$revenue, c(100, 200, 300, 400))
})

test_that(".prepare_income sorts weekly data by period_start", {
  shuffled <- tibble::tibble(
    practice_id = rep(1, 3),
    week_start  = as.Date(c("2025-03-03", "2025-01-06", "2025-02-03")),
    revenue     = c(300, 100, 200)
  )

  result <- .prepare_income(shuffled)

  expect_equal(result$data$period_start, sort(result$data$period_start))
  expect_equal(result$data$revenue, c(100, 200, 300))
})

# --- Data-volume guards -------------------------------------------------------

test_that(".forecast_series falls back to linear and warns when n < MIN_ETS for ets", {
  short_series <- seq(100, 150, length.out = 4)  # 4 obs < 6 minimum for ETS

  expect_snapshot(
    result <- .forecast_series(short_series, method = "ets", horizon = 6, frequency = 12)
  )
  # Must still return a valid forecast (from linear fallback)
  expect_length(result$point, 6)
  expect_true(all(is.finite(result$point)))
})

test_that(".forecast_series falls back to linear and warns when n < MIN_ARIMA for arima", {
  short_series <- seq(100, 150, length.out = 6)  # 6 obs < 8 minimum for ARIMA

  expect_snapshot(
    result <- .forecast_series(short_series, method = "arima", horizon = 6, frequency = 12)
  )
  expect_length(result$point, 6)
  expect_true(all(is.finite(result$point)))
})

test_that(".forecast_series emits method_fallback class for ets below minimum", {
  short_series <- seq(100, 150, length.out = 4)
  expect_warning(
    .forecast_series(short_series, method = "ets", horizon = 6, frequency = 12),
    class = "dcForecastR_method_fallback"
  )
})

test_that(".forecast_series emits method_fallback class for arima below minimum", {
  short_series <- seq(100, 150, length.out = 6)
  expect_warning(
    .forecast_series(short_series, method = "arima", horizon = 6, frequency = 12),
    class = "dcForecastR_method_fallback"
  )
})

test_that(".forecast_series emits low_data_advisory for ets between minimum and recommended", {
  skip_if_not_installed("forecast")
  mid_series <- seq(100, 200, length.out = 12)  # 12 obs: >= 6 min, < 20 recommended

  expect_warning(
    .forecast_series(mid_series, method = "ets", horizon = 6, frequency = 12),
    class = "dcForecastR_low_data_advisory"
  )
})

test_that(".forecast_series emits low_data_advisory for arima between minimum and recommended", {
  skip_if_not_installed("forecast")
  mid_series <- seq(100, 200, length.out = 12)  # 12 obs: >= 8 min, < 20 recommended

  expect_warning(
    .forecast_series(mid_series, method = "arima", horizon = 6, frequency = 12),
    class = "dcForecastR_low_data_advisory"
  )
})

test_that(".forecast_series emits no warning for ets with sufficient data", {
  skip_if_not_installed("forecast")
  long_series <- seq(100, 300, length.out = 24)

  expect_no_warning(
    .forecast_series(long_series, method = "ets", horizon = 6, frequency = 12)
  )
})

test_that(".forecast_series emits low_data_advisory for linear with exactly 2 observations", {
  expect_warning(
    .forecast_series(c(100, 200), method = "linear", horizon = 6, frequency = 12),
    class = "dcForecastR_low_data_advisory"
  )
})

test_that(".forecast_series is silent for linear with 3 or more observations", {
  expect_no_warning(
    .forecast_series(seq(100, 130, length.out = 3), method = "linear", horizon = 6, frequency = 12)
  )
})

test_that(".forecast_series is silent for linear with many observations", {
  expect_no_warning(
    .forecast_series(seq(100, 300, length.out = 24), method = "linear", horizon = 6, frequency = 12)
  )
})
