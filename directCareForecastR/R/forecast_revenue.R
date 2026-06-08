forecast_revenue <- function(income_summary, 
                              horizon_weeks = 12,
                              method = c("linear", "ets", "arima")) {
  # Returns: forecast_points tibble matching your schema
  # method = 'linear' for early data, 
  # switch to ets/arima once you have 20+ weeks
}