#' Forecast post-processing utilities
#'
#' These helpers operate on the list objects returned by
#' `directCareForecastR::forecast_*()` and are applied in
#' `mod_projections_server()` after the model has been fit.
#'
# -- Growth-rate adjustment ----------------------------------------------------

#' Apply assumed annual growth rates on top of a fitted forecast result.
#'
#' The statistical model captures only the *historical* trend present in the
#' uploaded data.  This function layers an additional compounding drift so
#' users can model scenarios such as "10 % annual revenue growth" or
#' "3 % annual overhead increase" independently of the fitted curve.
#'
#' Growth is compounded monthly: in forecast period *n*, the multiplier is
#' `(1 + rate/12)^n` where `rate` is the annual rate expressed as a
#' proportion (e.g. 0.10 for 10 %).
#'
#' @param result             List returned by a `forecast_*()` function.
#' @param income_growth_pct  Annual income/revenue growth rate (%).
#'   Positive = growth, negative = decline.  Applied to `revenue_*` columns.
#' @param overhead_growth_pct Annual overhead growth rate (%).
#'   Applied to `overhead_*` columns when present (break-even forecast only).
#'   Does **not** adjust `required_revenue` in the target forecast because
#'   the split between the overhead and target-income components is not
#'   available post-hoc; see note below.
#'
#' @return The same list structure with `forecast_data` adjusted in-place and
#'   `breakeven_date` / `periods_to_breakeven` (or `target_date` /
#'   `periods_to_target`) re-derived from the adjusted series.
#'   When `periods_to_breakeven == 0L` (already achieved), that status is
#'   preserved; use [breakeven_is_sustained()] to determine whether the
#'   forecast sustains it.
#'
#' @noRd
apply_growth_assumptions <- function(
  result,
  income_growth_pct = 0,
  overhead_growth_pct = 0,
  overhead_flat = NULL,
  target_income_override = NULL
) {
  if (is.null(result)) {
    return(result)
  }

  fd <- result$forecast_data
  n <- seq_len(nrow(fd))

  inc_mult <- (1 + income_growth_pct / 100 / 12)^n
  ovhd_mult <- (1 + overhead_growth_pct / 100 / 12)^n

  # Revenue / income columns (all three forecast types)
  rev_cols <- intersect(
    c("revenue_forecast", "revenue_lower", "revenue_upper"),
    names(fd)
  )
  for (col in rev_cols) {
    fd[[col]] <- fd[[col]] * inc_mult
  }

  # Overhead columns (break-even forecast only) ---------------------------------
  # When overhead_flat is provided the entire overhead series is replaced with a
  # constant, collapsing the CI ribbon to a flat line.  Otherwise the fitted
  # series is scaled by the compounding growth multiplier.
  ovhd_cols <- intersect(
    c("overhead_forecast", "overhead_lower", "overhead_upper"),
    names(fd)
  )
  if (!is.null(overhead_flat) && length(ovhd_cols) > 0L) {
    for (col in ovhd_cols) {
      fd[[col]] <- overhead_flat
    }
  } else if (overhead_growth_pct != 0) {
    for (col in ovhd_cols) {
      fd[[col]] <- fd[[col]] * ovhd_mult
    }
  }

  # For the income-target forecast, required_revenue = overhead + target_income.
  # We keep this consistent with however overhead was adjusted above.
  if (!is.null(target_income_override) && "required_revenue" %in% names(fd)) {
    tgt <- as.numeric(target_income_override)

    if (!is.null(overhead_flat)) {
      # Flat model: required_revenue is a constant across all periods.
      req_flat <- overhead_flat + tgt
      fd$required_revenue <- req_flat

      # Also update scalar summary fields read by value boxes and
      # interpretation text (these live outside forecast_data).
      result$required_revenue_now <- req_flat
      if (!is.null(result$current_revenue)) {
        result$current_gap <- result$current_revenue - req_flat
      }
    } else if (overhead_growth_pct != 0 && "overhead_forecast" %in% names(fd)) {
      # Fitted-trend model with non-zero overhead growth: overhead_forecast was
      # already scaled above by ovhd_mult.  required_revenue must track it so
      # the chart line stays coherent (overhead + target, not just raw overhead).
      # The "Revenue Needed Now" scalar is intentionally left unchanged \u2014
      # overhead growth is a forward-looking assumption, not a current-state change.
      fd$required_revenue <- fd$overhead_forecast + tgt
    }
    # Trend model at 0% growth: required_revenue from model run is already
    # overhead_forecast + target_income, so nothing to update.
  }

  result$forecast_data <- fd

  # Re-derive crossing dates from the adjusted series -------------------------

  if ("overhead_forecast" %in% names(fd)) {
    # Break-even: first forecast period where revenue_forecast >= overhead_forecast.
    # If already at break-even (periods_to_breakeven == 0L) that status is kept;
    # breakeven_is_sustained() checks the endpoint separately.
    # identical() safely handles NULL / NA / non-0L without length-1 coercion issues
    already <- identical(result$periods_to_breakeven, 0L)
    if (!already) {
      cross <- which(fd$revenue_forecast >= fd$overhead_forecast)
      if (length(cross) > 0L) {
        result$periods_to_breakeven <- cross[1L]
        result$breakeven_date <- fd$period_start[cross[1L]]
      } else {
        result$periods_to_breakeven <- NA_integer_
        result$breakeven_date <- as.Date(NA)
      }
    }

    # Re-derive CI date bounds from the growth-adjusted CI columns so the
    # narrative text ("The 95% CI spans X to Y") stays consistent with the
    # chart ribbon. Without this, the text always shows the original model CI
    # regardless of what growth assumptions the user applied.
    if (
      all(
        c(
          "revenue_lower",
          "revenue_upper",
          "overhead_lower",
          "overhead_upper"
        ) %in%
          names(fd)
      )
    ) {
      ci_lo_idx <- which((fd$revenue_lower - fd$overhead_upper) >= 0)[1]
      ci_hi_idx <- which((fd$revenue_upper - fd$overhead_lower) >= 0)[1]
      result$confidence_interval <- c(
        lower = if (!is.na(ci_lo_idx)) {
          fd$period_start[ci_lo_idx]
        } else {
          as.Date(NA)
        },
        upper = if (!is.na(ci_hi_idx)) {
          fd$period_start[ci_hi_idx]
        } else {
          as.Date(NA)
        }
      )
    }
  }

  if ("required_revenue" %in% names(fd)) {
    # Target: first period where adjusted revenue_forecast >= (unadjusted)
    # required_revenue.  required_revenue is left as-is because we cannot
    # cleanly separate its overhead and target-income components post-hoc.
    cross <- which(fd$revenue_forecast >= fd$required_revenue)
    if (length(cross) > 0L) {
      result$periods_to_target <- cross[1L]
      result$target_date <- fd$period_start[cross[1L]]
    } else {
      result$periods_to_target <- NA_integer_
      result$target_date <- as.Date(NA)
    }
  }

  result
}


# -- Break-even sustainability check ------------------------------------------

#' Is an already-achieved break-even sustained through the forecast horizon?
#'
#' @param result A (possibly growth-adjusted) break-even forecast result.
#' @return `TRUE`  \u2013 the final forecast period has revenue >= overhead.
#'         `FALSE` \u2013 overhead is projected to outpace revenue at end of horizon.
#'         `NA`    \u2013 break-even has not yet been achieved (not applicable).
#' @noRd
#' @param threshold Proportion of forecast periods that must have
#'   revenue >= overhead to qualify as "sustained" (default 0.70).
#' @noRd
breakeven_is_sustained <- function(result, threshold = 0.70) {
  if (is.null(result)) {
    return(NA)
  }
  if (!identical(result$periods_to_breakeven, 0L)) {
    return(NA)
  }

  fd <- result$forecast_data
  if (!"overhead_forecast" %in% names(fd)) {
    return(NA)
  }

  n_total <- sum(!is.na(fd$revenue_forecast) & !is.na(fd$overhead_forecast))
  if (n_total == 0L) {
    return(NA)
  }
  n_above <- sum(fd$revenue_forecast >= fd$overhead_forecast, na.rm = TRUE)
  (n_above / n_total) >= threshold
}


# -- Income-target sustainability check ---------------------------------------

#' Is an already-achieved income target sustained through the forecast horizon?
#'
#' Mirrors [breakeven_is_sustained()]: when the practice currently meets its
#' income target (`current_gap >= 0`), this checks whether a supermajority of
#' forecast periods also have `revenue_forecast >= required_revenue`.
#'
#' @param result    A (possibly growth-adjusted) income-target forecast result.
#' @param threshold Proportion threshold (default 0.70).
#' @return `TRUE` / `FALSE` / `NA` (NA when target is not yet achieved).
#' @noRd
target_is_sustained <- function(result, threshold = 0.70) {
  if (is.null(result)) {
    return(NA)
  }
  if (!isTRUE(result$current_gap >= 0)) {
    return(NA)
  } # not yet at target

  fd <- result$forecast_data
  if (!"required_revenue" %in% names(fd)) {
    return(NA)
  }

  n_total <- sum(!is.na(fd$revenue_forecast) & !is.na(fd$required_revenue))
  if (n_total == 0L) {
    return(NA)
  }
  n_above <- sum(fd$revenue_forecast >= fd$required_revenue, na.rm = TRUE)
  (n_above / n_total) >= threshold
}
