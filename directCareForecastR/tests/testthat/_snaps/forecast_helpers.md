# .forecast_series falls back to linear and warns when n < MIN_ETS for ets

    Code
      result <- .forecast_series(short_series, method = "ets", horizon = 6,
        frequency = 12)
    Condition
      Warning:
      Only 4 observations available. ETS requires at least 6 periods to run reliably. The linear method was used instead.

# .forecast_series falls back to linear and warns when n < MIN_ARIMA for arima

    Code
      result <- .forecast_series(short_series, method = "arima", horizon = 6,
        frequency = 12)
    Condition
      Warning:
      Only 6 observations available. ARIMA requires at least 8 periods to run reliably. The linear method was used instead.

