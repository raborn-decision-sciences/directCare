#' Run .forecast_series() and capture data-volume warnings
#'
#' Wraps .forecast_series() with a withCallingHandlers layer that records the
#' text of any data-volume warnings without muffling them, so they still
#' propagate to the caller for UI notification. Returns a list with the forecast
#' result ($fc) and any captured messages ($warnings).
#' @noRd
.forecast_series_tracked <- function(...) {
  msgs <- character(0)
  record <- function(w) msgs <<- c(msgs, conditionMessage(w))
  fc <- withCallingHandlers(
    .forecast_series(...),
    dcForecastR_insufficient_data = record,
    dcForecastR_method_fallback = record,
    dcForecastR_low_data_advisory = record
  )
  list(fc = fc, warnings = msgs)
}

# Minimum observations required before ETS/ARIMA will run without erroring.
.MIN_OBS_ETS <- 6L
.MIN_OBS_ARIMA <- 8L
# Below this threshold the method runs but results are flagged as rough.
.RECOMMENDED_OBS <- 20L

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
#' @importFrom stats lm predict qnorm ts
.forecast_series <- function(
  series,
  method,
  horizon,
  frequency = 12,
  level = 0.95
) {
  series_clean <- series[!is.na(series)]
  n <- length(series_clean)

  if (n < 2) {
    val <- if (n == 1L) series_clean[[1L]] else 0
    rlang::warn(
      paste0(
        "Only ",
        n,
        " observation",
        if (n != 1L) "s" else "",
        " available \u2014 ",
        "not enough data to fit any trend. ",
        "A flat forecast is shown. Add more periods of data for meaningful projections."
      ),
      class = "dcForecastR_insufficient_data",
      n_obs = n
    )
    return(list(
      point = rep(val, horizon),
      lower = rep(val, horizon),
      upper = rep(val, horizon)
    ))
  }

  ts_obj <- stats::ts(series_clean, frequency = frequency)

  # Hard guard: too few observations to run the method reliably \u2014 fall back.
  min_obs <- switch(method, ets = .MIN_OBS_ETS, arima = .MIN_OBS_ARIMA, 2L)
  if (n < min_obs) {
    method_label <- switch(
      method,
      ets = "ETS",
      arima = "ARIMA",
      toupper(method)
    )
    rlang::warn(
      paste0(
        "Only ",
        n,
        " observations available. ",
        method_label,
        " requires at least ",
        min_obs,
        " periods to run reliably. The linear method was used instead."
      ),
      class = "dcForecastR_method_fallback",
      n_obs = n,
      min_obs = min_obs,
      requested_method = method
    )
    method <- "linear"
  } else if (n < 3L && method == "linear") {
    rlang::warn(
      paste0(
        "Only ",
        n,
        " observations available. ",
        "Linear forecasts with fewer than 3 data points are highly uncertain. ",
        "Treat this projection as a rough estimate."
      ),
      class = "dcForecastR_low_data_advisory",
      n_obs = n,
      recommended_obs = 3L
    )
  } else if (n < .RECOMMENDED_OBS && method != "linear") {
    method_label <- switch(
      method,
      ets = "ETS",
      arima = "ARIMA",
      toupper(method)
    )
    rlang::warn(
      paste0(
        "Only ",
        n,
        " observations available. ",
        method_label,
        " produces more reliable forecasts with ",
        .RECOMMENDED_OBS,
        " or more periods of data. ",
        "Treat this projection as a rough estimate."
      ),
      class = "dcForecastR_low_data_advisory",
      n_obs = n,
      recommended_obs = .RECOMMENDED_OBS
    )
  }

  switch(
    method,
    linear = .forecast_linear(ts_obj, horizon, level),
    ets = .forecast_ets(ts_obj, horizon, level),
    arima = .forecast_arima(ts_obj, horizon, level)
  )
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

  lm_fit <- lm(ts_obj ~ time_idx)

  future_time <- seq(n + 1, n + horizon)
  pred <- predict(
    lm_fit,
    newdata = data.frame(time_idx = future_time),
    se.fit = TRUE
  )

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
    rlang::abort(
      "Package 'forecast' required for ETS method. Install with install.packages('forecast')"
    )
  }

  ets_fit <- forecast::ets(ts_obj)
  ets_forecast <- forecast::forecast(ets_fit, h = horizon, level = level * 100)

  list(
    point = as.numeric(ets_forecast$mean),
    lower = as.numeric(ets_forecast$lower[, 1]),
    upper = as.numeric(ets_forecast$upper[, 1])
  )
}

#' ARIMA Forecast
#'
#' Auto ARIMA forecast using the forecast package.
#'
#' High-frequency seasonality (frequency > 24, e.g. weekly data with
#' frequency = 52) cannot be modelled by standard ARIMA -- there are
#' insufficient observations to estimate 52-lag seasonal parameters, and
#' \code{auto.arima} silently degenerates to ARIMA(0,0,0), producing a flat
#' forecast. When the ts object has a frequency above 24 we strip the
#' seasonal hint (reset to frequency = 1) so that \code{auto.arima} fits a
#' trend model instead.
#'
#' @param ts_obj Time series object
#' @param horizon Integer number of periods to forecast
#' @param level Numeric confidence level (0-1)
#' @return List with point forecasts, lower, and upper confidence bounds
#' @noRd
.forecast_arima <- function(ts_obj, horizon, level) {
  if (!requireNamespace("forecast", quietly = TRUE)) {
    rlang::abort(
      "Package 'forecast' required for ARIMA method. Install with install.packages('forecast')"
    )
  }

  # Strip high-frequency seasonality: ARIMA cannot fit seasonal lags of 52
  # (weekly) or similar large values -- it needs hundreds of observations per
  # cycle. Resetting to frequency = 1 allows auto.arima to model the trend.
  if (stats::frequency(ts_obj) > 24) {
    ts_obj <- stats::ts(as.numeric(ts_obj), frequency = 1)
  }

  arima_fit <- forecast::auto.arima(ts_obj)
  arima_forecast <- forecast::forecast(
    arima_fit,
    h = horizon,
    level = level * 100
  )

  list(
    point = as.numeric(arima_forecast$mean),
    lower = as.numeric(arima_forecast$lower[, 1]),
    upper = as.numeric(arima_forecast$upper[, 1])
  )
}
