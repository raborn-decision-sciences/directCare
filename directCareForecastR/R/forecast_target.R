#' Forecast When Revenue Will Hit a Net Income Target
#'
#' Given a desired net income (revenue minus overhead), calculates the required
#' revenue in each future period and projects when the current revenue trend
#' will cross that threshold. Required revenue in each period is
#' \code{overhead_forecast + target_income}, so this function accounts for
#' overhead that may itself be growing or shrinking over time.
#'
#' @param income_summary A data frame with income data. For weekly data: columns
#'   \code{practice_id}, \code{week_start}, \code{revenue}. For monthly data:
#'   columns \code{practice_id}, \code{year}, \code{month}, and either
#'   \code{total_revenue} or \code{revenue}.
#' @param overhead_summary A data frame from \code{summarize_overhead_monthly()}
#'   with columns: \code{practice_id}, \code{year}, \code{month},
#'   \code{total_overhead}, \code{gross_overhead}, \code{total_refunds}.
#' @param target_income Numeric. The desired net income per period (revenue
#'   minus overhead). Use the same time unit as your data — e.g. monthly net
#'   income for monthly data.
#' @param method Character string specifying the forecasting method:
#'   \code{"linear"} (default), \code{"ets"}, or \code{"arima"}.
#' @param horizon Integer number of periods to forecast. Defaults to 52 for
#'   weekly data and 12 for monthly data.
#' @param confidence_level Numeric between 0 and 1 for the confidence interval
#'   (default: 0.95).
#'
#' @return A list containing:
#'   \item{target_date}{Date when revenue is projected to meet or exceed
#'     required revenue, or \code{NA} if not reached within the horizon}
#'   \item{periods_to_target}{Number of periods until the target is reached,
#'     or \code{NA}}
#'   \item{current_gap}{Current revenue minus current required revenue.
#'     Negative means revenue must still grow to hit the target}
#'   \item{required_revenue_now}{Overhead in the most recent period plus
#'     \code{target_income} — what revenue needs to be right now}
#'   \item{confidence_interval}{Named vector with \code{lower} and \code{upper}
#'     date bounds for when the target will be reached}
#'   \item{forecast_data}{Tibble with per-period forecasts: \code{period_start},
#'     \code{revenue_forecast}, \code{revenue_lower}, \code{revenue_upper},
#'     \code{overhead_forecast}, \code{required_revenue}, \code{net_vs_target}}
#'   \item{target_income}{The \code{target_income} value that was passed in}
#'   \item{method}{Forecasting method used}
#'   \item{frequency}{Time frequency detected: \code{"weekly"} or
#'     \code{"monthly"}}
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
#' # Project when the practice will clear $5,000/month net
#' forecast_target(income, overhead, target_income = 5000, method = "linear")
#' }
forecast_target <- function(income_summary,
                            overhead_summary,
                            target_income,
                            method = c("linear", "ets", "arima"),
                            horizon = NULL,
                            confidence_level = 0.95) {

  method <- match.arg(method)

  if (!is.numeric(target_income) || length(target_income) != 1) {
    rlang::abort(
      "target_income must be a single numeric value.",
      class = "dcForecastR_invalid_target"
    )
  }

  if (dplyr::n_distinct(income_summary$practice_id) > 1L) {
    rlang::abort(
      paste0(
        "forecast_target() requires a single practice. ",
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
        "forecast_target() requires a single practice. ",
        "The supplied overhead_summary contains ",
        dplyr::n_distinct(overhead_summary$practice_id),
        " distinct practice_id values. Filter to one practice before forecasting."
      ),
      class = "dcForecastR_multiple_practices"
    )
  }

  # Prepare data and detect frequency
  income_prep   <- .prepare_income(income_summary)
  overhead_prep <- .prepare_overhead(overhead_summary)

  if (income_prep$frequency != overhead_prep$frequency) {
    rlang::warn(
      paste0(
        "Income data is ", income_prep$frequency, " but overhead data is ",
        overhead_prep$frequency, ". Using ", income_prep$frequency, " frequency."
      )
    )
  }

  frequency_str    <- income_prep$frequency
  periods_per_year <- income_prep$periods_per_year

  if (is.null(horizon)) {
    horizon <- if (frequency_str == "weekly") 52L else 12L
  }

  # Merge historical income and overhead to establish current status
  combined <- dplyr::full_join(
    income_prep$data,
    overhead_prep$data,
    by = c("practice_id", "period_start")
  ) |>
    dplyr::arrange(period_start) |>
    dplyr::mutate(
      revenue  = tidyr::replace_na(revenue, 0),
      overhead = tidyr::replace_na(overhead, 0)
    )

  latest_period <- combined |>
    dplyr::filter(!is.na(revenue) & !is.na(overhead)) |>
    dplyr::slice_tail(n = 1)

  required_revenue_now <- latest_period$overhead + target_income
  current_gap          <- latest_period$revenue - required_revenue_now

  # Forecast revenue and overhead independently
  rev_fc  <- .forecast_series(
    combined$revenue,
    method = method,
    horizon = horizon,
    frequency = periods_per_year,
    level = confidence_level
  )

  ovhd_fc <- .forecast_series(
    combined$overhead,
    method = method,
    horizon = horizon,
    frequency = periods_per_year,
    level = confidence_level
  )

  # Generate forecast dates
  last_date      <- max(combined$period_start, na.rm = TRUE)
  date_unit      <- if (frequency_str == "weekly") "week" else "month"
  forecast_dates <- seq(
    last_date + lubridate::period(1, units = date_unit),
    by = date_unit,
    length.out = horizon
  )

  # Required revenue each period = projected overhead + target_income
  required_revenue <- ovhd_fc$point + target_income

  forecast_data <- tibble::tibble(
    period_start      = forecast_dates,
    revenue_forecast  = rev_fc$point,
    revenue_lower     = rev_fc$lower,
    revenue_upper     = rev_fc$upper,
    overhead_forecast = ovhd_fc$point,
    required_revenue  = required_revenue,
    net_vs_target     = rev_fc$point - required_revenue
  )

  # First period where revenue forecast meets or exceeds required revenue
  target_idx <- which(forecast_data$net_vs_target >= 0)[1]

  if (is.na(target_idx)) {
    target_date       <- NA
    periods_to_target <- NA
    ci_lower          <- NA
    ci_upper          <- NA
    rlang::warn(
      paste0(
        "Target net income of ", target_income,
        " not reached within the forecast horizon of ", horizon, " periods."
      ),
      class = "dcForecastR_target_not_reached"
    )
  } else {
    target_date       <- forecast_data$period_start[target_idx]
    periods_to_target <- target_idx

    # CI bounds: pessimistic (lower CI revenue vs upper CI overhead + target)
    #            and optimistic (upper CI revenue vs lower CI overhead + target)
    ci_lower_required <- ovhd_fc$upper + target_income
    ci_upper_required <- ovhd_fc$lower + target_income

    ci_lower_idx <- which((rev_fc$lower  - ci_lower_required) >= 0)[1]
    ci_upper_idx <- which((rev_fc$upper  - ci_upper_required) >= 0)[1]

    ci_lower <- if (!is.na(ci_lower_idx)) forecast_data$period_start[ci_lower_idx] else NA
    ci_upper <- if (!is.na(ci_upper_idx)) forecast_data$period_start[ci_upper_idx] else NA
  }

  list(
    target_date          = target_date,
    periods_to_target    = periods_to_target,
    current_gap          = current_gap,
    required_revenue_now = required_revenue_now,
    confidence_interval  = c(lower = ci_lower, upper = ci_upper),
    forecast_data        = forecast_data,
    target_income        = target_income,
    method               = method,
    frequency            = frequency_str
  )
}
