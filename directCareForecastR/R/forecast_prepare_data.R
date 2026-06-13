#' Detect Time Series Frequency
#'
#' Determines whether income/overhead data is weekly or monthly based on structure
#'
#' @param data A data frame with time series data
#' @return Character string: "weekly" or "monthly"
#' @noRd
.detect_frequency <- function(data) {
  has_week_start <- "week_start" %in% names(data)
  has_year_month <- "year" %in% names(data) && "month" %in% names(data)

  # If has week_start, it's weekly (even if it also has month/year)
  if (has_week_start) {
    return("weekly")
  }

  # If has year + month, it's monthly
  if (has_year_month) {
    return("monthly")
  }

  # Default to monthly if ambiguous
  return("monthly")
}

#' Prepare Income Data for Forecasting
#'
#' Converts income data to consistent format while preserving frequency
#'
#' @param income_summary A data frame with income data
#' @return List with: data frame (period_start, revenue), frequency string, periods per year
#' @noRd
.prepare_income <- function(income_summary) {
  # Identify revenue column
  revenue_col <- if ("total_revenue" %in% names(income_summary)) {
    "total_revenue"
  } else if ("revenue" %in% names(income_summary)) {
    "revenue"
  } else {
    rlang::abort("Income summary must have 'total_revenue' or 'revenue' column")
  }

  # Detect frequency
  freq <- .detect_frequency(income_summary)

  if (freq == "weekly") {
    prepared <- income_summary |>
      dplyr::select(
        practice_id,
        period_start = week_start,
        revenue = dplyr::all_of(revenue_col)
      ) |>
      dplyr::arrange(period_start)

    list(
      data = prepared,
      frequency = "weekly",
      periods_per_year = 52
    )
  } else {
    prepared <- income_summary |>
      dplyr::mutate(
        period_start = lubridate::make_date(year, month, 1),
        revenue = .data[[revenue_col]]
      ) |>
      dplyr::select(practice_id, period_start, revenue) |>
      dplyr::arrange(period_start)

    list(
      data = prepared,
      frequency = "monthly",
      periods_per_year = 12
    )
  }
}

#' Prepare Overhead Data for Forecasting
#'
#' Converts overhead data to consistent format while preserving frequency
#'
#' @param overhead_summary A data frame with overhead data
#' @return List with: data frame (period_start, overhead), frequency string, periods per year
#' @noRd
.prepare_overhead <- function(overhead_summary) {
  # Detect frequency
  freq <- .detect_frequency(overhead_summary)

  if (freq == "weekly") {
    # Weekly data
    prepared <- overhead_summary |>
      dplyr::mutate(overhead = total_overhead) |>
      dplyr::select(practice_id, period_start = week_start, overhead)

    list(
      data = prepared,
      frequency = "weekly",
      periods_per_year = 52
    )
  } else {
    # Monthly data
    prepared <- overhead_summary |>
      dplyr::mutate(
        period_start = lubridate::make_date(year, month, 1),
        overhead = total_overhead
      ) |>
      dplyr::select(practice_id, period_start, overhead)

    list(
      data = prepared,
      frequency = "monthly",
      periods_per_year = 12
    )
  }
}

#' Prepare Weekly Income Data
#'
#' Converts monthly or weekly income data to consistent weekly format
#'
#' @param income_summary A data frame with income data
#' @return A data frame with practice_id, week_start, and revenue columns
#' @noRd
.prepare_weekly_income <- function(income_summary) {
  result <- .prepare_income(income_summary)

  if (result$frequency == "monthly") {
    # Convert monthly to weekly
    result$data |>
      dplyr::mutate(
        revenue = revenue / 4.33
      ) |>
      dplyr::rename(week_start = period_start)
  } else {
    # Already weekly
    result$data |>
      dplyr::rename(week_start = period_start)
  }
}

#' Prepare Weekly Overhead Data
#'
#' Converts monthly overhead data to consistent weekly format
#'
#' @param overhead_summary A data frame with overhead data
#' @return A data frame with practice_id, week_start, and overhead columns
#' @noRd
.prepare_weekly_overhead <- function(overhead_summary) {
  result <- .prepare_overhead(overhead_summary)

  if (result$frequency == "monthly") {
    # Convert monthly to weekly
    result$data |>
      dplyr::mutate(
        overhead = overhead / 4.33
      ) |>
      dplyr::rename(week_start = period_start)
  } else {
    # Already weekly
    result$data |>
      dplyr::rename(week_start = period_start)
  }
}
