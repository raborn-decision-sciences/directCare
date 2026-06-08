# forecast_target errors on multi-practice income

    Code
      forecast_target(income, make_monthly_overhead(), target_income = 1000, method = "linear")
    Condition
      Error in `forecast_target()`:
      ! forecast_target() requires a single practice. The supplied income_summary contains 2 distinct practice_id values. Filter to one practice before forecasting.

# forecast_target errors on multi-practice overhead

    Code
      forecast_target(make_monthly_income(), overhead, target_income = 1000, method = "linear")
    Condition
      Error in `forecast_target()`:
      ! forecast_target() requires a single practice. The supplied overhead_summary contains 2 distinct practice_id values. Filter to one practice before forecasting.

