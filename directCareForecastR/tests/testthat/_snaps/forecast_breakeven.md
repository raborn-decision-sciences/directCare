# forecast_breakeven works with weekly data

    Code
      result <- forecast_breakeven(income_weekly, overhead_monthly, method = "linear",
        horizon = 52)
    Condition
      Warning:
      Income data is weekly but overhead data is monthly. Using weekly frequency.

# forecast_breakeven warns when break-even not reached

    Code
      forecast_breakeven(income_monthly, overhead_monthly, method = "linear",
        horizon = 6)
    Condition
      Warning:
      Break-even not reached within the forecast horizon of 6 periods.
    Output
      $breakeven_date
      [1] NA
      
      $periods_to_breakeven
      [1] NA
      
      $current_surplus_deficit
      [1] -1500
      
      $current_revenue
      [1] 500
      
      $current_overhead
      [1] 2000
      
      $current_overhead_avg
      [1] 2000
      
      $overhead_avg_n
      [1] 4
      
      $confidence_interval
      lower upper 
         NA    NA 
      
      $forecast_data
      # A tibble: 6 x 8
        period_start revenue_forecast revenue_lower revenue_upper overhead_forecast
        <date>                  <dbl>         <dbl>         <dbl>             <dbl>
      1 2025-07-01                500          500           500.              2000
      2 2025-08-01                500          500           500.              2000
      3 2025-09-01                500          500.          500.              2000
      4 2025-10-01                500          500.          500.              2000
      5 2025-11-01                500          500.          500.              2000
      6 2025-12-01                500          500.          500.              2000
      # i 3 more variables: overhead_lower <dbl>, overhead_upper <dbl>,
      #   net_forecast <dbl>
      
      $method
      [1] "linear"
      
      $frequency
      [1] "monthly"
      

# forecast_breakeven errors on multi-practice income

    Code
      forecast_breakeven(income, overhead, method = "linear")
    Condition
      Error in `forecast_breakeven()`:
      ! forecast_breakeven() requires a single practice. The supplied income_summary contains 2 distinct practice_id values. Filter to one practice before forecasting.

# forecast_breakeven errors on multi-practice overhead

    Code
      forecast_breakeven(income, overhead, method = "linear")
    Condition
      Error in `forecast_breakeven()`:
      ! forecast_breakeven() requires a single practice. The supplied overhead_summary contains 2 distinct practice_id values. Filter to one practice before forecasting.

