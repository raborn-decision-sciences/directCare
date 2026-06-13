#' Forecast Break-Even Point
#'
#' Estimates when revenue will consistently exceed overhead expenses, returning
#' the projected break-even date, weeks/months to break-even, current surplus/deficit,
#' and confidence intervals. Automatically detects whether data is weekly or monthly.
#'
#' @param income_summary A data frame with income data. For weekly: columns
#'   practice_id, week_start, revenue. For monthly: columns practice_id, year,
#'   month, total_revenue (or revenue).
#' @param overhead_summary A data frame from \code{summarize_overhead_monthly()}
#'   with columns: practice_id, year, month, total_overhead, gross_overhead,
#'   total_refunds.
#' @param method Character string specifying the forecasting method: "linear"
#'   (default, for early/sparse data), "ets" (exponential smoothing), or "arima"
#'   (autoregressive integrated moving average). Use "linear" with < 20 data points.
#' @param horizon Integer specifying how many periods ahead to forecast
#'   (default: 52 for weekly, 12 for monthly).
#' @param confidence_level Numeric between 0 and 1 for confidence interval
#'   (default: 0.95).
#'
#' @return A list containing:
#'   \item{breakeven_date}{Date when revenue is projected to exceed overhead}
#'   \item{periods_to_breakeven}{Number of periods (weeks/months) until break-even}
#'   \item{current_surplus_deficit}{Current surplus (positive) or deficit (negative) per period}
#'   \item{confidence_interval}{Lower and upper bounds of break-even estimate}
#'   \item{forecast_data}{Tibble with forecasted revenue and overhead by period}
#'   \item{method}{Forecasting method used}
#'   \item{frequency}{Time frequency detected ("weekly" or "monthly")}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' income <- ingest_gnucash_csv("income.csv", 1) |> normalize_gnucash_income()
#' overhead <- ingest_gnucash_csv("expenses.csv", 1) |>
#'   filter_gnucash_overhead() |>
#'   summarize_overhead_monthly()
#' 
#' forecast_breakeven(income, overhead, method = "linear")
#' }
forecast_breakeven <- function(income_summary,
                               overhead_summary,
                               method = c("linear", "ets", "arima"),
                               horizon = NULL,
                               confidence_level = 0.95) {
  
  method <- match.arg(method)

  if (dplyr::n_distinct(income_summary$practice_id) > 1L) {
    rlang::abort(
      paste0(
        "forecast_breakeven() requires a single practice. ",
        "The supplied income_summary contains ",
        dplyr::n_distinct(income_summary$practice_id),
        " distinct practice_id values. Filter to one practice before forecasting."
      ),
      class = "dcForecastR_multiple_practices"
    )
  }

  if (dplyr::n_distinct(overhead_summary$practice_id) > 1L) {
    rlang::abort(
      paste0(
        "forecast_breakeven() requires a single practice. ",
        "The supplied overhead_summary contains ",
        dplyr::n_distinct(overhead_summary$practice_id),
        " distinct practice_id values. Filter to one practice before forecasting."
      ),
      class = "dcForecastR_multiple_practices"
    )
  }

  # Prepare data and detect frequency
  income_prep <- .prepare_income(income_summary)
  overhead_prep <- .prepare_overhead(overhead_summary)
  
  # Check that both have same frequency
  if (income_prep$frequency != overhead_prep$frequency) {
    rlang::warn(
      paste0(
        "Income data is ", income_prep$frequency, " but overhead data is ",
        overhead_prep$frequency, ". Using ", income_prep$frequency, " frequency."
      )
    )
  }
  
  # Use income frequency as primary
  frequency_str <- income_prep$frequency
  periods_per_year <- income_prep$periods_per_year
  
  # Set default horizon if not provided
  if (is.null(horizon)) {
    horizon <- if (frequency_str == "weekly") 52 else 12
  }
  
  # Merge income and overhead by period.
  # Derive latest_period *before* replace_na so the NA filter is meaningful:
  # after replace_na both columns are always non-NA, making the filter a no-op
  # and potentially picking a period where one side was a synthetic zero.
  combined_raw <- dplyr::full_join(
    income_prep$data,
    overhead_prep$data,
    by = c("practice_id", "period_start")
  ) |>
    dplyr::arrange(period_start)

  latest_period <- combined_raw |>
    dplyr::filter(!is.na(revenue) & !is.na(overhead)) |>
    dplyr::slice_tail(n = 1)

  combined <- combined_raw |>
    dplyr::mutate(
      revenue  = tidyr::replace_na(revenue, 0),
      overhead = tidyr::replace_na(overhead, 0),
      net      = revenue - overhead
    )

  # Most-recent income period — use the pre-join income series so we always
  # get the latest week/month regardless of when overhead was posted.
  current_revenue <- income_prep$data |>
    dplyr::slice_tail(n = 1) |>
    dplyr::pull(revenue)

  # Stable per-period overhead estimate for member-count calculations and for
  # the current surplus/deficit check.
  #
  # When income is weekly but overhead is monthly (the common DPC case), almost
  # every week in `combined` gets overhead = 0 after the full_join -- monthly
  # totals land only on the handful of rows whose period_start happens to be the
  # first of a month.  Pulling `latest_period$overhead` therefore returns 0 or a
  # single month's lump sum depending on luck, making per-member calculations
  # nonsensical.
  #
  # Fix: average the last up-to-3 periods of the *pre-join* overhead series
  # (which still holds monthly totals), then divide by 4.33 if the income
  # frequency is weekly.  This gives a realistic, stable weekly overhead figure
  # regardless of when overhead was posted.
  n_avg <- min(nrow(overhead_prep$data), 4L)
  recent_ovhd <- overhead_prep$data |>
    dplyr::arrange(period_start) |>
    dplyr::slice_tail(n = n_avg)
  avg_overhead_native <- mean(recent_ovhd$overhead, na.rm = TRUE)

  # Convert to the same time unit as income when frequencies differ.
  current_overhead_avg <- if (overhead_prep$frequency != frequency_str) {
    # Overhead is monthly, income is weekly: divide to get $/week
    avg_overhead_native / 4.33
  } else {
    avg_overhead_native
  }

  # Use the most-recent income period's revenue and the frequency-adjusted
  # overhead average so the already-at-breakeven check is correct when income
  # is weekly and overhead is monthly (the common DPC case).
  current_surplus_deficit <- current_revenue - current_overhead_avg

  # Forecast revenue and overhead separately; capture any data-volume warnings
  # so they can flow through to interpretation text and reports.
  data_warnings <- character(0)
  rev_tracked  <- .forecast_series_tracked(
    combined$revenue,
    method = method,
    horizon = horizon,
    frequency = periods_per_year,
    level = confidence_level
  )
  data_warnings <- c(data_warnings, rev_tracked$warnings)
  rev_fc <- rev_tracked$fc

  ovhd_tracked <- .forecast_series_tracked(
    combined$overhead,
    method = method,
    horizon = horizon,
    frequency = periods_per_year,
    level = confidence_level
  )
  data_warnings <- c(data_warnings, ovhd_tracked$warnings)
  ovhd_fc <- ovhd_tracked$fc

  # Generate forecast dates
  last_date <- max(combined$period_start, na.rm = TRUE)
  date_unit <- if (frequency_str == "weekly") "week" else "month"
  forecast_dates <- seq(
    last_date + lubridate::period(1, units = date_unit),
    by = date_unit,
    length.out = horizon
  )

  # Create forecast data frame
  forecast_data <- tibble::tibble(
    period_start      = forecast_dates,
    revenue_forecast  = rev_fc$point,
    revenue_lower     = rev_fc$lower,
    revenue_upper     = rev_fc$upper,
    overhead_forecast = ovhd_fc$point,
    overhead_lower    = ovhd_fc$lower,
    overhead_upper    = ovhd_fc$upper,
    net_forecast      = rev_fc$point - ovhd_fc$point
  )
  
  # Find break-even point (first period where revenue >= overhead).
  # If the practice is already at or above break-even in the most recent
  # observed period, report that immediately rather than looking into forecast
  # periods. The crossing check uses >= 0 to match: a net of exactly $0 means
  # revenue covers overhead and is treated as achieved.
  if (current_surplus_deficit >= 0) {
    breakeven_date       <- last_date
    periods_to_breakeven <- 0L
    ci_lower             <- last_date
    ci_upper             <- last_date
  } else {
    breakeven_idx <- which(forecast_data$net_forecast >= 0)[1]

    if (is.na(breakeven_idx)) {
      breakeven_date       <- NA
      periods_to_breakeven <- NA
      ci_lower             <- NA
      ci_upper             <- NA
      rlang::warn(
        paste0(
          "Break-even not reached within the forecast horizon of ",
          horizon, " periods."
        ),
        class = "dcForecastR_breakeven_not_reached"
      )
    } else {
      breakeven_date       <- forecast_data$period_start[breakeven_idx]
      periods_to_breakeven <- breakeven_idx

      # Confidence interval: pessimistic (lower rev - upper overhead)
      #                      and optimistic (upper rev - lower overhead)
      ci_lower_idx <- which((forecast_data$revenue_lower - forecast_data$overhead_upper) >= 0)[1]
      ci_upper_idx <- which((forecast_data$revenue_upper - forecast_data$overhead_lower) >= 0)[1]

      ci_lower <- if (!is.na(ci_lower_idx)) forecast_data$period_start[ci_lower_idx] else NA
      ci_upper <- if (!is.na(ci_upper_idx)) forecast_data$period_start[ci_upper_idx] else NA
    }
  }
  
  # Return results
  list(
    breakeven_date          = breakeven_date,
    periods_to_breakeven    = periods_to_breakeven,
    current_surplus_deficit = current_surplus_deficit,
    current_revenue         = current_revenue,
    # current_overhead: raw single-period value from the most recent period
    #   where both income and overhead were observed. May be 0 for weekly
    #   data if no such period exists in the horizon; use current_overhead_avg.
    current_overhead        = as.numeric(latest_period$overhead),
    # current_overhead_avg: rolling mean over the last n_avg overhead periods,
    #   converted to per-income-period units.  Use this for member-count
    #   calculations -- it is robust to the posting-day spikes that make the
    #   raw single-period value unreliable.
    current_overhead_avg    = current_overhead_avg,
    overhead_avg_n          = n_avg,
    confidence_interval     = c(lower = ci_lower, upper = ci_upper),
    forecast_data           = forecast_data,
    method                  = method,
    frequency               = frequency_str,
    data_warnings           = if (length(data_warnings) > 0L) data_warnings else NULL
  )
}