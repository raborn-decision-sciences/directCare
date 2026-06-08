#' Forecast Time Series with Multiple Methods
#'
#' Routes to appropriate forecasting method based on user selection
#'
#' @param series Numeric vector of time series data
#' @param method Character string: "linear", "ets", or "arima"
#' @param horizon Integer number of periods to forecast
#' @param frequency Integer periods per year (12 for monthly, 52 for weekly)
#' @param level Numeric confidence level (0-1)
#' @return List with point forecasts, lower, and upper confidence bounds
#' @noRd
.forecast_series <- function(series, method, horizon, frequency = 12, level = 0.95) {
  # Remove NAs and ensure we have data
  series_clean <- series[!is.na(series)]
  n <- length(series_clean)
  
  if (n < 2) {
    # Not enough data, return flat forecast
    return(list(
      point = rep(mean(series_clean, na.rm = TRUE), horizon),
      lower = rep(mean(series_clean, na.rm = TRUE), horizon),
      upper = rep(mean(series_clean, na.rm = TRUE), horizon)
    ))
  }
  
  # Create time series object with appropriate frequency
  ts_obj <- ts(series_clean, frequency = frequency)
  
  forecast_result <- switch(
    method,
    linear = .forecast_linear(ts_obj, horizon, level),
    ets = .forecast_ets(ts_obj, horizon, level),
    arima = .forecast_arima(ts_obj, horizon, level)
  )
  
  forecast_result
}

#' Linear Regression Forecast
#'
#' Simple linear trend forecast with confidence intervals
#'
#' @param ts_obj Time series object
#' @param horizon Integer number of periods to forecast
#' @param level Numeric confidence level (0-1)
#' @return List with point forecasts, lower, and upper confidence bounds
#' @noRd
.forecast_linear <- function(ts_obj, horizon, level) {
  n <- length(ts_obj)
  time_idx <- seq_len(n)
  
  # Fit linear model
  lm_fit <- lm(ts_obj ~ time_idx)
  
  # Predict future values
  future_time <- seq(n + 1, n + horizon)
  pred <- predict(lm_fit, newdata = data.frame(time_idx = future_time), se.fit = TRUE)
  
  # Calculate confidence intervals
  z_score <- qnorm((1 + level) / 2)
  
  list(
    point = as.numeric(pred$fit),
    lower = as.numeric(pred$fit - z_score * pred$se.fit),
    upper = as.numeric(pred$fit + z_score * pred$se.fit)
  )
}

#' ETS Forecast
#'
#' Exponential smoothing forecast using the forecast package
#'
#' @param ts_obj Time series object
#' @param horizon Integer number of periods to forecast
#' @param level Numeric confidence level (0-1)
#' @return List with point forecasts, lower, and upper confidence bounds
#' @noRd
.forecast_ets <- function(ts_obj, horizon, level) {
  if (!requireNamespace("forecast", quietly = TRUE)) {
    rlang::abort("Package 'forecast' required for ETS method. Install with install.packages('forecast')")
  }
  
  ets_fit <- forecast::ets(ts_obj)
  ets_forecast <- forecast::forecast(ets_fit, h = horizon, level = level * 100)
  
  list(
    point = as.numeric(ets_forecast$mean),
    lower = as.numeric(ets_forecast$lower[, 1]),  # Extract first (and only) column
    upper = as.numeric(ets_forecast$upper[, 1])   # Extract first (and only) column
  )
}

#' ARIMA Forecast
#'
#' Auto ARIMA forecast using the forecast package
#'
#' @param ts_obj Time series object
#' @param horizon Integer number of periods to forecast
#' @param level Numeric confidence level (0-1)
#' @return List with point forecasts, lower, and upper confidence bounds
#' @noRd
.forecast_arima <- function(ts_obj, horizon, level) {
  if (!requireNamespace("forecast", quietly = TRUE)) {
    rlang::abort("Package 'forecast' required for ARIMA method. Install with install.packages('forecast')")
  }
  
  arima_fit <- forecast::auto.arima(ts_obj)
  arima_forecast <- forecast::forecast(arima_fit, h = horizon, level = level * 100)
  
  list(
    point = as.numeric(arima_forecast$mean),
    lower = as.numeric(arima_forecast$lower[, 1]),  # Extract first (and only) column
    upper = as.numeric(arima_forecast$upper[, 1])   # Extract first (and only) column
  )
}