#' Forecast Future Revenue
#'
#' Projects future revenue from historical income data using the chosen
#' statistical method. Automatically detects whether the input is weekly or
#' monthly and sets a sensible default horizon. Use \code{"linear"} when fewer
#' than 20 data points are available; switch to \code{"ets"} or \code{"arima"}
#' once the series is longer.
#'
#' @param income_summary A data frame with income data. For weekly data: columns
#'   \code{practice_id}, \code{week_start}, \code{revenue}. For monthly data:
#'   columns \code{practice_id}, \code{year}, \code{month}, and either
#'   \code{total_revenue} or \code{revenue}.
#' @param method Character string specifying the forecasting method:
#'   \code{"linear"} (default, suitable for sparse/early data),
#'   \code{"ets"} (exponential smoothing), or \code{"arima"} (auto ARIMA).
#'   \code{"ets"} and \code{"arima"} require the \pkg{forecast} package and at
#'   least 20 observations for reliable results.
#' @param horizon Integer number of periods ahead to forecast. Defaults to 52
#'   for weekly data and 12 for monthly data.
#' @param confidence_level Numeric between 0 and 1 specifying the width of the
#'   confidence interval (default: 0.95).
#'
#' @return A list containing:
#'   \item{current_revenue}{Revenue in the most recent observed period}
#'   \item{forecast_data}{Tibble with one row per forecast period and columns
#'     \code{period_start}, \code{revenue_forecast}, \code{revenue_lower},
#'     \code{revenue_upper}}
#'   \item{method}{Forecasting method used}
#'   \item{frequency}{Time frequency detected: \code{"weekly"} or
#'     \code{"monthly"}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' income <- ingest_gnucash_csv("income.csv", 1) |> normalize_gnucash_income()
#'
#' forecast_revenue(income, method = "linear")
#' }
forecast_revenue <- function(income_summary,
                             method = c("linear", "ets", "arima"),
                             horizon = NULL,
                             confidence_level = 0.95) {

  method <- match.arg(method)

  if (dplyr::n_distinct(income_summary$practice_id) > 1L) {
    rlang::abort(
      paste0(
        "forecast_revenue() requires a single practice. ",
        "The supplied income_summary contains ",
        dplyr::n_distinct(income_summary$practice_id),
        " distinct practice_id values. Filter to one practice before forecasting."
      ),
      class = "dcForecastR_multiple_practices"
    )
  }

  # Prepare data and detect frequency
  income_prep      <- .prepare_income(income_summary)
  frequency_str    <- income_prep$frequency
  periods_per_year <- income_prep$periods_per_year

  # Default horizon based on frequency
  if (is.null(horizon)) {
    horizon <- if (frequency_str == "weekly") 52L else 12L
  }

  # Current revenue: most recent observed period
  current_revenue <- income_prep$data |>
    dplyr::arrange(period_start) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::pull(revenue)

  # Forecast the revenue series; capture any data-volume warnings.
  rev_tracked <- .forecast_series_tracked(
    income_prep$data$revenue,
    method = method,
    horizon = horizon,
    frequency = periods_per_year,
    level = confidence_level
  )
  rev_fc        <- rev_tracked$fc
  data_warnings <- rev_tracked$warnings

  # Generate forecast dates
  last_date   <- max(income_prep$data$period_start, na.rm = TRUE)
  date_unit   <- if (frequency_str == "weekly") "week" else "month"
  forecast_dates <- seq(
    last_date + lubridate::period(1, units = date_unit),
    by = date_unit,
    length.out = horizon
  )

  forecast_data <- tibble::tibble(
    period_start     = forecast_dates,
    revenue_forecast = rev_fc$point,
    revenue_lower    = rev_fc$lower,
    revenue_upper    = rev_fc$upper
  )

  list(
    current_revenue = current_revenue,
    forecast_data   = forecast_data,
    method          = method,
    frequency       = frequency_str,
    data_warnings   = if (length(data_warnings) > 0L) data_warnings else NULL
  )
}
