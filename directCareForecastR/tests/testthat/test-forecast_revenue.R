make_monthly_income <- function(n = 6, start_revenue = 500, end_revenue = 3000) {
  tibble::tibble(
    practice_id   = rep(1, n),
    year          = rep(2025, n),
    month         = seq_len(n),
    total_revenue = seq(start_revenue, end_revenue, length.out = n)
  )
}

make_weekly_income <- function(n = 20, start_revenue = 500, end_revenue = 2000) {
  tibble::tibble(
    practice_id = rep(1, n),
    week_start  = seq(as.Date("2025-01-06"), by = "week", length.out = n),
    revenue     = seq(start_revenue, end_revenue, length.out = n)
  )
}

# --- Structure ----------------------------------------------------------------

test_that("forecast_revenue returns correct list structure", {
  result <- forecast_revenue(make_monthly_income())

  expect_type(result, "list")
  expect_named(result, c("current_revenue", "forecast_data", "method", "frequency", "data_warnings"))
})

test_that("forecast_revenue forecast_data has correct columns", {
  result <- forecast_revenue(make_monthly_income())

  expect_s3_class(result$forecast_data, "data.frame")
  expect_true(all(c(
    "period_start", "revenue_forecast", "revenue_lower", "revenue_upper"
  ) %in% names(result$forecast_data)))
})

# --- Frequency detection ------------------------------------------------------

test_that("forecast_revenue detects monthly frequency", {
  result <- forecast_revenue(make_monthly_income())
  expect_equal(result$frequency, "monthly")
})

test_that("forecast_revenue detects weekly frequency", {
  result <- forecast_revenue(make_weekly_income())
  expect_equal(result$frequency, "weekly")
})

# --- Default horizon ----------------------------------------------------------

test_that("forecast_revenue defaults to 12 periods for monthly data", {
  result <- forecast_revenue(make_monthly_income())
  expect_equal(nrow(result$forecast_data), 12)
})

test_that("forecast_revenue defaults to 52 periods for weekly data", {
  result <- forecast_revenue(make_weekly_income())
  expect_equal(nrow(result$forecast_data), 52)
})

test_that("forecast_revenue respects an explicit horizon", {
  result <- forecast_revenue(make_monthly_income(), horizon = 6)
  expect_equal(nrow(result$forecast_data), 6)
})

# --- Forecast dates -----------------------------------------------------------

test_that("forecast_revenue period_start begins one period after last observation", {
  income <- make_monthly_income(n = 6)  # months 1–6 of 2025
  result <- forecast_revenue(income, horizon = 3)

  expect_equal(result$forecast_data$period_start[1], as.Date("2025-07-01"))
  expect_equal(result$forecast_data$period_start[2], as.Date("2025-08-01"))
})

test_that("forecast_revenue period_start is weekly when data is weekly", {
  income <- make_weekly_income(n = 4)
  result <- forecast_revenue(income, horizon = 2)

  expected_first <- max(income$week_start) + 7L
  expect_equal(result$forecast_data$period_start[1], expected_first)
})

# --- Current revenue ----------------------------------------------------------

test_that("forecast_revenue returns the most recent observed revenue", {
  income <- make_monthly_income(n = 6, start_revenue = 500, end_revenue = 3000)
  result <- forecast_revenue(income)

  expect_equal(result$current_revenue, 3000)
})

# --- Confidence intervals -----------------------------------------------------

test_that("forecast_revenue confidence intervals bound the point forecast", {
  result <- forecast_revenue(make_monthly_income(n = 12))

  expect_true(all(result$forecast_data$revenue_lower <= result$forecast_data$revenue_forecast))
  expect_true(all(result$forecast_data$revenue_forecast <= result$forecast_data$revenue_upper))
})

# --- Increasing trend ---------------------------------------------------------

test_that("forecast_revenue point forecasts continue an upward trend", {
  result <- forecast_revenue(make_monthly_income(n = 12), method = "linear")

  # First forecast > last observed
  last_observed <- tail(seq(500, 3000, length.out = 12), 1)
  expect_gt(result$forecast_data$revenue_forecast[1], last_observed)
})

# --- Methods ------------------------------------------------------------------

test_that("forecast_revenue records the chosen method", {
  result <- forecast_revenue(make_monthly_income(), method = "linear")
  expect_equal(result$method, "linear")
})

# --- Multi-practice guard -----------------------------------------------------

test_that("forecast_revenue errors on multi-practice input", {
  income <- tibble::tibble(
    practice_id = c(rep(1, 6), rep(2, 6)),
    year = rep(2025, 12), month = rep(1:6, 2),
    total_revenue = seq(1000, 6000, length.out = 12)
  )
  expect_snapshot(forecast_revenue(income, method = "linear"), error = TRUE)
})

# --- Methods ------------------------------------------------------------------

test_that("forecast_revenue ets and arima require the forecast package", {
  skip_if_not_installed("forecast")

  result_ets   <- forecast_revenue(make_monthly_income(n = 24), method = "ets")
  result_arima <- forecast_revenue(make_monthly_income(n = 24), method = "arima")

  expect_equal(result_ets$method,   "ets")
  expect_equal(result_arima$method, "arima")
  expect_equal(nrow(result_ets$forecast_data),   12)
  expect_equal(nrow(result_arima$forecast_data), 12)
})
