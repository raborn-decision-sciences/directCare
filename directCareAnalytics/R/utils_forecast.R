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


# -- Planned future overhead events -------------------------------------------

#' Apply user-defined future overhead step changes to a forecast result.
#'
#' Each event adds a permanent monthly cost increase from its start period
#' forward. The function adds the cumulative event overhead to the fitted
#' overhead forecast columns, then re-derives crossing dates.
#'
#' @param result  List returned by `apply_growth_assumptions()`.
#' @param events  Data frame with columns `start_date` (Date), `monthly_cost`
#'   (numeric). May be NULL or 0-row, in which case `result` is returned unchanged.
#'
#' @return Updated result list with overhead columns and crossing dates adjusted.
#' @noRd
apply_overhead_events <- function(result, events = NULL) {
  if (is.null(result)) {
    return(result)
  }
  if (is.null(events) || nrow(events) == 0L) {
    return(result)
  }

  fd <- result$forecast_data
  ovhd_cols <- intersect(
    c("overhead_forecast", "overhead_lower", "overhead_upper"),
    names(fd)
  )
  if (length(ovhd_cols) == 0L || !"period_start" %in% names(fd)) {
    return(result)
  }

  for (i in seq_len(nrow(events))) {
    ev_start <- events$start_date[i]
    ev_cost <- as.numeric(events$monthly_cost[i])
    if (!is.finite(ev_cost) || ev_cost == 0) {
      next
    }
    mask <- fd$period_start >= ev_start
    for (col in ovhd_cols) {
      fd[[col]][mask] <- fd[[col]][mask] + ev_cost
    }
  }

  result$forecast_data <- fd

  # Re-derive break-even crossing
  if ("overhead_forecast" %in% names(fd)) {
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
  }

  # Re-derive target crossing (required_revenue already includes event overhead
  # if apply_growth_assumptions set it; if not, adjust required_revenue too)
  if ("required_revenue" %in% names(fd) && "overhead_forecast" %in% names(fd)) {
    # Re-align required_revenue with event-adjusted overhead
    if (!is.null(result$target_income)) {
      fd$required_revenue <- fd$overhead_forecast + result$target_income
      result$forecast_data <- fd
    }
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


# -- Planned future membership fee changes -------------------------------------

#' Apply user-defined future membership fee step changes to a forecast result.
#'
#' Each event adds a permanent revenue step (positive or negative) from its
#' start period forward, computed upstream as
#' `(new_fee - current_fee) * tier_members`. The function adds the cumulative
#' event revenue delta to the fitted revenue forecast columns, then re-derives
#' crossing dates for both break-even and income-target results.
#'
#' @param result  List returned by `apply_growth_assumptions()` /
#'   `apply_overhead_events()`.
#' @param events  Data frame with columns `start_date` (Date), `revenue_delta`
#'   (numeric). May be NULL or 0-row, in which case `result` is returned
#'   unchanged.
#'
#' @return Updated result list with revenue columns and crossing dates
#'   adjusted.
#' @noRd
apply_membership_fee_events <- function(result, events = NULL) {
  if (is.null(result)) {
    return(result)
  }
  if (is.null(events) || nrow(events) == 0L) {
    return(result)
  }

  fd <- result$forecast_data
  rev_cols <- intersect(
    c("revenue_forecast", "revenue_lower", "revenue_upper"),
    names(fd)
  )
  if (length(rev_cols) == 0L || !"period_start" %in% names(fd)) {
    return(result)
  }

  for (i in seq_len(nrow(events))) {
    ev_start <- events$start_date[i]
    ev_delta <- as.numeric(events$revenue_delta[i])
    if (!is.finite(ev_delta) || ev_delta == 0) {
      next
    }
    mask <- fd$period_start >= ev_start
    for (col in rev_cols) {
      fd[[col]][mask] <- fd[[col]][mask] + ev_delta
    }
  }

  result$forecast_data <- fd

  # Re-derive break-even crossing
  if ("overhead_forecast" %in% names(fd)) {
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
  }

  # Re-derive target crossing
  if ("required_revenue" %in% names(fd)) {
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


# -- Goal-seek: what growth-rate change reaches a forecast goal --------------

#' Binary-search a growth rate that reaches a goal within a period horizon
#'
#' Shared bisection core behind [goal_seek_breakeven()] and
#' [goal_seek_target()] -- income-growth up, overhead-growth down, holding
#' the other fixed. `recompute` encapsulates whichever
#' `apply_growth_assumptions()`/`apply_overhead_events()`/
#' `apply_membership_fee_events()` pipeline the caller needs (break-even and
#' income-target differ slightly: target also needs
#' `target_income_override`), and `extract_periods` pulls the right
#' `periods_to_*` field out of the result. Search range matches the
#' `income_growth`/`overhead_growth` sliders' [-60, 60] range in
#' `mod_projections.R`. Monotonic in both directions (higher income growth
#' or lower overhead growth only ever helps or leaves the crossing period
#' unchanged), which is what makes bisection valid here -- same approach as
#' directCarePlanR's `goal_seek_projection_recovery()`.
#'
#' @param recompute `function(income_pct, overhead_pct)` returning an
#'   adjusted forecast result for those two growth rates.
#' @param extract_periods `function(result)` returning the relevant
#'   `periods_to_*` field (integer or `NA`).
#' @param current_income_growth_pct,current_overhead_growth_pct Numeric
#'   annual growth rates (%) currently in effect -- the search starts from
#'   these and only moves in the helpful direction.
#' @param overhead_searchable Logical; `FALSE` skips the overhead lever
#'   entirely (e.g. a flat overhead model has no growth rate to solve for).
#' @param horizon_periods Integer number of forecast periods the goal
#'   should be reached within.
#'
#' @return A list with `income` and `overhead` (`NULL` when
#'   `overhead_searchable` is `FALSE`), each a list with `achievable` and,
#'   when `TRUE`, `new_growth_pct`.
#'
#' @noRd
.goal_seek_growth_rate <- function(
  recompute,
  extract_periods,
  current_income_growth_pct,
  current_overhead_growth_pct,
  overhead_searchable,
  horizon_periods
) {
  .periods_at <- function(income_pct, overhead_pct) {
    p <- extract_periods(recompute(income_pct, overhead_pct))
    if (is.null(p) || is.na(p)) NA_integer_ else as.integer(p)
  }

  .solve_income <- function() {
    at_cap <- .periods_at(60, current_overhead_growth_pct)
    if (is.na(at_cap) || at_cap > horizon_periods) {
      return(list(achievable = FALSE))
    }
    lo <- current_income_growth_pct
    hi <- 60
    for (i in seq_len(25)) {
      mid <- (lo + hi) / 2
      r <- .periods_at(mid, current_overhead_growth_pct)
      if (!is.na(r) && r <= horizon_periods) hi <- mid else lo <- mid
    }
    list(achievable = TRUE, new_growth_pct = round(hi, 1))
  }

  .solve_overhead <- function() {
    at_floor <- .periods_at(current_income_growth_pct, -60)
    if (is.na(at_floor) || at_floor > horizon_periods) {
      return(list(achievable = FALSE))
    }
    lo <- -60
    hi <- current_overhead_growth_pct
    for (i in seq_len(25)) {
      mid <- (lo + hi) / 2
      r <- .periods_at(current_income_growth_pct, mid)
      if (!is.na(r) && r <= horizon_periods) lo <- mid else hi <- mid
    }
    list(achievable = TRUE, new_growth_pct = round(lo, 1))
  }

  income <- .solve_income()
  overhead <- if (overhead_searchable) .solve_overhead() else NULL

  # A third option alongside "either lever alone": the smallest single
  # blend fraction t in [0, 1] that moves BOTH income growth and overhead
  # growth simultaneously and proportionally from their current values
  # toward each lever's own independently-solved solo target -- i.e. "a
  # smaller change to both together" rather than the full change to just
  # one. Only computed when both solo levers are independently achievable
  # (blending toward a lever with no achievable "full" endpoint has
  # nothing well-defined to blend toward). Monotonic in t for the same
  # reason the two solo searches above are: increasing t only ever moves
  # both levers further in their already-established helpful direction,
  # never backward.
  combined <- if (!overhead_searchable) {
    NULL
  } else if (isTRUE(income$achievable) && isTRUE(overhead$achievable)) {
    .solve_combined_growth_rate(
      periods_at = .periods_at,
      current_income_growth_pct = current_income_growth_pct,
      current_overhead_growth_pct = current_overhead_growth_pct,
      income_target_pct = income$new_growth_pct,
      overhead_target_pct = overhead$new_growth_pct,
      horizon_periods = horizon_periods
    )
  } else {
    list(achievable = FALSE)
  }

  list(income = income, overhead = overhead, combined = combined)
}

# Bisection core behind .goal_seek_growth_rate()'s "combined" lever -- see
# its own inline comment for the full rationale. Split out as its own
# function (rather than nested inside .goal_seek_growth_rate()) purely so
# it can be unit-tested directly against a synthetic `periods_at()`
# without needing a full forecast fixture.
#' @noRd
.solve_combined_growth_rate <- function(
  periods_at,
  current_income_growth_pct,
  current_overhead_growth_pct,
  income_target_pct,
  overhead_target_pct,
  horizon_periods
) {
  blend_income <- function(t) {
    current_income_growth_pct + t * (income_target_pct - current_income_growth_pct)
  }
  blend_overhead <- function(t) {
    current_overhead_growth_pct + t * (overhead_target_pct - current_overhead_growth_pct)
  }

  at_full <- periods_at(blend_income(1), blend_overhead(1))
  if (is.na(at_full) || at_full > horizon_periods) {
    return(list(achievable = FALSE))
  }

  lo <- 0
  hi <- 1
  for (i in seq_len(25)) {
    mid <- (lo + hi) / 2
    r <- periods_at(blend_income(mid), blend_overhead(mid))
    if (!is.na(r) && r <= horizon_periods) hi <- mid else lo <- mid
  }

  list(
    achievable = TRUE,
    new_income_growth_pct = round(blend_income(hi), 1),
    new_overhead_growth_pct = round(blend_overhead(hi), 1)
  )
}

# -- Goal-seek: what fee increase reaches a forecast goal --------------------

# Uniformly increase every forecast period's revenue by `fee_increase_pp *
# panel_size` (a permanent panel-wide fee increase effective immediately,
# not a dated event like apply_membership_fee_events()'s Planned Fee
# Changes), re-deriving periods_to_breakeven/breakeven_date or
# periods_to_target/target_date. `fee_increase_pp` is already in the
# result's own period units ($/week for weekly data, $/month for
# monthly) -- callers convert from a user-facing monthly figure before
# calling this. Same "frozen at 0L once already achieved" convention as
# .apply_member_loss()/apply_growth_assumptions(), though goal-seek
# callers never actually hit that branch (a goal not yet reached never
# starts at 0L).
#' @noRd
.apply_fee_increase <- function(result, fee_increase_pp, panel_size) {
  if (fee_increase_pp <= 0) {
    return(result)
  }
  delta <- fee_increase_pp * panel_size
  fd <- result$forecast_data
  rev_cols <- intersect(
    c("revenue_forecast", "revenue_lower", "revenue_upper"),
    names(fd)
  )
  for (col in rev_cols) {
    fd[[col]] <- fd[[col]] + delta
  }
  result$forecast_data <- fd
  result$current_revenue <- result$current_revenue + delta
  if (!is.null(result$current_surplus_deficit)) {
    result$current_surplus_deficit <- result$current_surplus_deficit + delta
  }
  if (!is.null(result$current_gap)) {
    result$current_gap <- result$current_gap + delta
  }

  if (
    "overhead_forecast" %in% names(fd) &&
      !identical(result$periods_to_breakeven, 0L)
  ) {
    cross <- which(fd$revenue_forecast >= fd$overhead_forecast)
    if (length(cross) > 0L) {
      result$periods_to_breakeven <- cross[1L]
      result$breakeven_date <- fd$period_start[cross[1L]]
    } else {
      result$periods_to_breakeven <- NA_integer_
      result$breakeven_date <- as.Date(NA)
    }
  }
  if (
    "required_revenue" %in% names(fd) &&
      !identical(result$periods_to_target, 0L)
  ) {
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

# Binary-search the smallest per-period fee increase that reaches the
# goal within horizon_periods, searched from 0 up to a cap of 2x the
# current per-period fee (i.e. tripling it) -- if even that isn't
# enough, achievable = FALSE. Mirrors .goal_seek_growth_rate()'s
# bisection shape; monotonic because a larger fee increase only ever
# adds more revenue, never less. Returns `fee_increase` already
# converted back to a user-facing monthly figure via `pp_to_monthly`
# (the new_fee = membership_fee + fee_increase step is left to the
# caller, which is the one that actually has membership_fee in scope).
#' @noRd
.solve_fee_increase <- function(
  recompute,
  extract_periods,
  fee_pp,
  pp_to_monthly,
  horizon_periods
) {
  .periods_at <- function(fee_increase_pp) {
    p <- extract_periods(recompute(fee_increase_pp))
    if (is.null(p) || is.na(p)) NA_integer_ else as.integer(p)
  }

  cap_pp <- fee_pp * 2
  at_cap <- .periods_at(cap_pp)
  if (is.na(at_cap) || at_cap > horizon_periods) {
    return(list(achievable = FALSE))
  }

  lo <- 0
  hi <- cap_pp
  for (i in seq_len(25)) {
    mid <- (lo + hi) / 2
    r <- .periods_at(mid)
    if (!is.na(r) && r <= horizon_periods) hi <- mid else lo <- mid
  }

  list(achievable = TRUE, fee_increase = round(hi * pp_to_monthly, 2))
}

# Shared wrapper behind goal_seek_breakeven()/goal_seek_target()'s `fee`
# lever: handles the panel_size/membership_fee NULL-skip, the weekly/
# monthly per-period conversion, and computing new_fee (in monthly
# dollars) from the solved fee_increase -- the one piece
# .solve_fee_increase() itself can't do, since it works in per-period
# units and never sees the caller's monthly membership_fee.
#' @noRd
.goal_seek_fee_lever <- function(
  base_result,
  extract_periods,
  frequency,
  panel_size,
  membership_fee,
  horizon_periods
) {
  if (!isTRUE(panel_size > 0) || !isTRUE(membership_fee > 0)) {
    return(NULL)
  }

  is_weekly <- identical(frequency, "weekly")
  fee_pp <- if (is_weekly) membership_fee / 4.33 else membership_fee
  pp_to_monthly <- if (is_weekly) 4.33 else 1

  fl <- .solve_fee_increase(
    recompute = function(fee_increase_pp) {
      .apply_fee_increase(base_result, fee_increase_pp, panel_size)
    },
    extract_periods = extract_periods,
    fee_pp = fee_pp,
    pp_to_monthly = pp_to_monthly,
    horizon_periods = horizon_periods
  )

  if (isTRUE(fl$achievable)) {
    fl$new_fee <- round(membership_fee + fl$fee_increase, 2)
  }
  fl
}

# -- Goal-seek: income-growth lever, expressed as extra members/month -------

#' Convert a solved income-growth percentage into an approximate
#' extra-members-per-month figure
#'
#' The income-growth lever itself is a compounding rate applied to
#' whichever revenue trajectory the model already produced (see
#' `apply_growth_assumptions()`) -- membership revenue isn't tracked
#' separately from fee-for-service, so there's no way to derive an exact
#' member count the way `.goal_seek_fee_lever()` does its own $/member
#' math. This is deliberately an approximation, not a second bisected
#' lever: it diffs the solved-vs-current revenue trajectories at the
#' target period to get the extra monthly revenue rate the growth
#' percentage implies, converts that to a member-equivalent using the
#' average membership fee, and spreads it evenly across the horizon --
#' the actual compounding shape adds members faster later on, but a flat
#' "roughly N/month" figure is the more useful practical instruction.
#'
#' @param recompute Function `function(income_pct, overhead_pct)`, same
#'   closure `.goal_seek_growth_rate()` uses -- re-run here (not reused
#'   from the bisection) since neither the baseline nor solved trajectory
#'   survives the search.
#' @param current_income_growth_pct,current_overhead_growth_pct Baseline
#'   growth rates currently in effect.
#' @param new_income_growth_pct The already-solved income-growth
#'   percentage from `.goal_seek_growth_rate()`'s income lever.
#' @param target_period Integer period index the goal is reached within.
#' @param panel_size,membership_fee Current total panel size and average
#'   membership fee ($/member/month). `NULL`/non-positive skips this
#'   entirely, same gate as `.goal_seek_fee_lever()`.
#' @param frequency `"weekly"` or `"monthly"` -- `revenue_forecast` is in
#'   per-period units, converted to a monthly rate before dividing by the
#'   (always-monthly) `membership_fee`.
#' @param fee_events Optional data frame with `start_date`/`revenue_delta`
#'   columns, as built by `mod_projections.R`'s `fee_events_df()` --
#'   scheduled future membership-fee changes. When one or more events
#'   have already taken effect by `target_date`, a second annotation is
#'   computed using the blended average fee those events imply, since
#'   the current-fee figure alone would misstate the pace needed once
#'   the change is in effect.
#' @param target_date The calendar date of `target_period`, used to
#'   decide which `fee_events` are active by then. `NULL` skips the
#'   planned-fee-change annotation even if `fee_events` is supplied.
#'
#' @return A list with `per_month` (integer, at least 1) and `fee_used`
#'   (the average fee it was computed against), plus `per_month_planned`/
#'   `fee_used_planned` when a distinct planned-fee-change figure applies.
#'   `NULL` when `panel_size`/`membership_fee` aren't both supplied, or
#'   the solved growth rate implies no positive revenue gap at the
#'   target period.
#'
#' @noRd
.income_growth_members_equiv <- function(
  recompute,
  current_income_growth_pct,
  current_overhead_growth_pct,
  new_income_growth_pct,
  target_period,
  panel_size,
  membership_fee,
  frequency,
  fee_events = NULL,
  target_date = NULL
) {
  if (!isTRUE(panel_size > 0) || !isTRUE(membership_fee > 0)) {
    return(NULL)
  }

  baseline <- recompute(current_income_growth_pct, current_overhead_growth_pct)
  goal <- recompute(new_income_growth_pct, current_overhead_growth_pct)

  extra_pp <- goal$forecast_data$revenue_forecast[target_period] -
    baseline$forecast_data$revenue_forecast[target_period]
  if (!is.finite(extra_pp) || extra_pp <= 0) {
    return(NULL)
  }

  is_weekly <- identical(frequency, "weekly")
  extra_monthly <- if (is_weekly) extra_pp * 4.33 else extra_pp

  per_month <- max(1L, round((extra_monthly / membership_fee) / target_period))
  result <- list(per_month = per_month, fee_used = round(membership_fee, 2))

  if (!is.null(fee_events) && nrow(fee_events) > 0L && !is.null(target_date)) {
    active <- fee_events[fee_events$start_date <= target_date, , drop = FALSE]
    if (nrow(active) > 0L) {
      planned_fee <- membership_fee + sum(active$revenue_delta) / panel_size
      if (
        isTRUE(planned_fee > 0) &&
          !isTRUE(round(planned_fee, 2) == round(membership_fee, 2))
      ) {
        result$per_month_planned <- max(
          1L, round((extra_monthly / planned_fee) / target_period)
        )
        result$fee_used_planned <- round(planned_fee, 2)
      }
    }
  }

  result
}

#' Find what growth-rate change would reach break-even within the horizon
#'
#' When a (growth-adjusted) break-even forecast doesn't reach break-even
#' within the forecast horizon, this finds how much higher an income growth
#' rate, or how much lower an overhead growth rate, alone would need to be
#' to reach it -- holding the other growth rate, and any configured
#' overhead/fee events, fixed at their current settings. Mirrors
#' `adj_breakeven()`'s own pipeline (`apply_growth_assumptions()` ->
#' `apply_overhead_events()` -> `apply_membership_fee_events()`) so the
#' recomputed `periods_to_breakeven` is directly comparable to what's
#' already on screen.
#'
#' @param breakeven_result Raw list from
#'   `directCareForecastR::forecast_breakeven()`, i.e. BEFORE any
#'   growth-rate or event adjustment.
#' @param current_income_growth_pct,current_overhead_growth_pct Numeric
#'   annual growth rates (%) currently in effect.
#' @param overhead_flat Passed through to `apply_growth_assumptions()`;
#'   when non-`NULL`, overhead is a flat model and the overhead lever isn't
#'   searchable.
#' @param overhead_events,fee_events Passed through to
#'   `apply_overhead_events()`/`apply_membership_fee_events()`, held fixed
#'   at the caller's current settings while searching.
#' @param panel_size,membership_fee Current total panel size and average
#'   membership fee ($/member/month) -- same values `interpret_breakeven()`
#'   already receives. When both are supplied and positive, a third "fee"
#'   lever is searched: how much higher the average membership fee alone
#'   would need to be (panel size held fixed) to reach break-even, holding
#'   income/overhead growth at their current settings. `NULL` (the
#'   default, or either one missing/non-positive) skips this lever
#'   entirely -- e.g. before the optional Membership Profile has been
#'   filled in. The same two values also gate an approximate
#'   extra-members/month annotation attached to the income lever -- see
#'   `.income_growth_members_equiv()`'s own doc.
#' @param horizon_periods Integer number of forecast periods break-even
#'   should be reached within. Defaults to
#'   `nrow(breakeven_result$forecast_data)`.
#'
#' @return A `dcAnalytics_goal_seek`-classed list with `target_period`,
#'   `income` (a list with `achievable`, and when `TRUE`, `new_growth_pct`
#'   and -- when `panel_size`/`membership_fee` are supplied -- a
#'   `members_equiv` sub-list, see `.income_growth_members_equiv()`),
#'   `overhead` (`NULL` when `overhead_flat` is set; otherwise a list with
#'   `achievable` and, when `TRUE`, `new_growth_pct`), and `fee` (`NULL`
#'   when `panel_size`/`membership_fee` aren't both supplied; otherwise a
#'   list with `achievable` and, when `TRUE`, `fee_increase` and
#'   `new_fee`, both in monthly dollars). `NULL` if `breakeven_result` is
#'   `NULL`.
#'
#' @noRd
goal_seek_breakeven <- function(
  breakeven_result,
  current_income_growth_pct = 0,
  current_overhead_growth_pct = 0,
  overhead_flat = NULL,
  overhead_events = NULL,
  fee_events = NULL,
  panel_size = NULL,
  membership_fee = NULL,
  horizon_periods = NULL
) {
  if (is.null(breakeven_result)) {
    return(NULL)
  }
  horizon_periods <- horizon_periods %||% nrow(breakeven_result$forecast_data)

  recompute <- function(income_pct, overhead_pct) {
    res <- apply_growth_assumptions(
      breakeven_result,
      income_growth_pct = income_pct,
      overhead_growth_pct = overhead_pct,
      overhead_flat = overhead_flat
    )
    res <- apply_overhead_events(res, overhead_events)
    apply_membership_fee_events(res, fee_events)
  }

  gs <- .goal_seek_growth_rate(
    recompute = recompute,
    extract_periods = function(res) res$periods_to_breakeven,
    current_income_growth_pct = current_income_growth_pct,
    current_overhead_growth_pct = current_overhead_growth_pct,
    overhead_searchable = is.null(overhead_flat),
    horizon_periods = horizon_periods
  )

  if (isTRUE(gs$income$achievable)) {
    gs$income$members_equiv <- .income_growth_members_equiv(
      recompute = recompute,
      current_income_growth_pct = current_income_growth_pct,
      current_overhead_growth_pct = current_overhead_growth_pct,
      new_income_growth_pct = gs$income$new_growth_pct,
      target_period = horizon_periods,
      panel_size = panel_size,
      membership_fee = membership_fee,
      frequency = breakeven_result$frequency,
      fee_events = fee_events,
      target_date = breakeven_result$forecast_data$period_start[horizon_periods]
    )
  }

  fee_lever <- .goal_seek_fee_lever(
    base_result = recompute(current_income_growth_pct, current_overhead_growth_pct),
    extract_periods = function(res) res$periods_to_breakeven,
    frequency = breakeven_result$frequency,
    panel_size = panel_size,
    membership_fee = membership_fee,
    horizon_periods = horizon_periods
  )

  structure(
    c(list(target_period = horizon_periods), gs, list(fee = fee_lever)),
    class = "dcAnalytics_goal_seek"
  )
}

#' Find what growth-rate change would reach the income target within the horizon
#'
#' Same idea as [goal_seek_breakeven()], applied to an income-target
#' forecast instead: how much higher an income growth rate, or how much
#' lower an overhead growth rate, alone would need to be for
#' `periods_to_target` to land within the horizon. Mirrors `adj_target()`'s
#' own pipeline, including `target_income_override` (target forecasts need
#' it to keep `required_revenue` consistent -- see
#' `apply_growth_assumptions()`).
#'
#' @param target_result Raw list from
#'   `directCareForecastR::forecast_target()`, i.e. BEFORE any growth-rate
#'   or event adjustment.
#' @param current_income_growth_pct,current_overhead_growth_pct Numeric
#'   annual growth rates (%) currently in effect.
#' @param overhead_flat,target_income_override Passed through to
#'   `apply_growth_assumptions()`; a non-`NULL` `overhead_flat` makes the
#'   overhead lever unsearchable, same as `goal_seek_breakeven()`.
#' @param overhead_events,fee_events Passed through to
#'   `apply_overhead_events()`/`apply_membership_fee_events()`, held fixed.
#' @param panel_size,membership_fee Passed through to the shared fee lever
#'   -- see `goal_seek_breakeven()`'s own doc for the full rationale.
#' @param horizon_periods Integer number of forecast periods the target
#'   should be reached within. Defaults to
#'   `nrow(target_result$forecast_data)`.
#'
#' @return A `dcAnalytics_goal_seek`-classed list, same shape as
#'   [goal_seek_breakeven()]'s return value. `NULL` if `target_result` is
#'   `NULL`.
#'
#' @noRd
goal_seek_target <- function(
  target_result,
  current_income_growth_pct = 0,
  current_overhead_growth_pct = 0,
  overhead_flat = NULL,
  target_income_override = NULL,
  overhead_events = NULL,
  fee_events = NULL,
  panel_size = NULL,
  membership_fee = NULL,
  horizon_periods = NULL
) {
  if (is.null(target_result)) {
    return(NULL)
  }
  horizon_periods <- horizon_periods %||% nrow(target_result$forecast_data)

  recompute <- function(income_pct, overhead_pct) {
    res <- apply_growth_assumptions(
      target_result,
      income_growth_pct = income_pct,
      overhead_growth_pct = overhead_pct,
      overhead_flat = overhead_flat,
      target_income_override = target_income_override
    )
    res <- apply_overhead_events(res, overhead_events)
    apply_membership_fee_events(res, fee_events)
  }

  gs <- .goal_seek_growth_rate(
    recompute = recompute,
    extract_periods = function(res) res$periods_to_target,
    current_income_growth_pct = current_income_growth_pct,
    current_overhead_growth_pct = current_overhead_growth_pct,
    overhead_searchable = is.null(overhead_flat),
    horizon_periods = horizon_periods
  )

  if (isTRUE(gs$income$achievable)) {
    gs$income$members_equiv <- .income_growth_members_equiv(
      recompute = recompute,
      current_income_growth_pct = current_income_growth_pct,
      current_overhead_growth_pct = current_overhead_growth_pct,
      new_income_growth_pct = gs$income$new_growth_pct,
      target_period = horizon_periods,
      panel_size = panel_size,
      membership_fee = membership_fee,
      frequency = target_result$frequency,
      fee_events = fee_events,
      target_date = target_result$forecast_data$period_start[horizon_periods]
    )
  }

  fee_lever <- .goal_seek_fee_lever(
    base_result = recompute(current_income_growth_pct, current_overhead_growth_pct),
    extract_periods = function(res) res$periods_to_target,
    frequency = target_result$frequency,
    panel_size = panel_size,
    membership_fee = membership_fee,
    horizon_periods = horizon_periods
  )

  structure(
    c(list(target_period = horizon_periods), gs, list(fee = fee_lever)),
    class = "dcAnalytics_goal_seek"
  )
}

#' Turn a goal-seek result into narrative HTML
#'
#' @param gs A `dcAnalytics_goal_seek` object, as returned by
#'   [goal_seek_breakeven()] or [goal_seek_target()].
#' @param pu Period-unit list, as returned by `.period_units()`.
#' @param goal_label Text naming the goal being sought, e.g. `"break-even"`
#'   or `"the income target"` -- interpolated directly into the sentence.
#' @param badge Whether to prefix the output with `.pro_badge_html()`.
#'   Defaults to `TRUE` for the live UI; the PDF report path passes
#'   `FALSE` -- Typst has no HTML renderer, so the badge `<span>` would
#'   otherwise survive `html_to_plain()`'s tag-stripping as a bare,
#'   ungrammatical leading "Pro" (the tag gets stripped, the label text
#'   inside it doesn't).
#'
#' @return An HTML string -- a single `<p>...</p>` when only one lever is
#'   achievable, or a `<p>` intro followed by a `<ul>` of every achievable
#'   lever (including "both together," see below) when two or more are.
#'   `NULL` if `gs` is `NULL` or no lever is achievable at all.
#'
#' Render an `.income_growth_members_equiv()` result as a parenthetical
#' annotation
#'
#' Split out of `.describe_goal_seek()` for readability -- one or two
#' sentences appended to the income lever's clause, giving a concrete,
#' actionable example ("about N extra members/month") alongside the
#' abstract growth percentage. See `.income_growth_members_equiv()`'s own
#' doc for why this is an approximation, not a second solved lever.
#'
#' @param me Result of `.income_growth_members_equiv()`, or `NULL`.
#'
#' @return A character string starting with `" ("` and ending with `")"`,
#'   or `""` when `me` is `NULL`.
#'
#' @noRd
.format_members_annotation <- function(me) {
  if (is.null(me)) {
    return("")
  }

  .members <- function(n) paste0(n, " extra member", if (n != 1L) "s" else "")

  current <- paste0(
    "roughly <strong>", .members(me$per_month), "/month</strong> at your ",
    "current ", fmt_dollar(me$fee_used), "/member fee"
  )

  if (is.null(me$per_month_planned)) {
    return(paste0(" (", current, ")"))
  }

  paste0(
    " (", current, ", or about <strong>",
    .members(me$per_month_planned), "/month</strong> once your planned ",
    "fee change takes effect at about ", fmt_dollar(me$fee_used_planned),
    "/member)"
  )
}

#' @noRd
.describe_goal_seek <- function(gs, pu, goal_label, badge = TRUE) {
  if (is.null(gs)) {
    return(NULL)
  }

  badge_prefix <- if (isTRUE(badge)) paste0(.pro_badge_html(), " ") else ""

  income_ok <- isTRUE(gs$income$achievable)
  overhead_ok <- !is.null(gs$overhead) && isTRUE(gs$overhead$achievable)
  fee_ok <- !is.null(gs$fee) && isTRUE(gs$fee$achievable)
  combined_ok <- !is.null(gs$combined) && isTRUE(gs$combined$achievable)

  if (!income_ok && !overhead_ok && !fee_ok) {
    return(paste0(
      "<p class='text-muted small'>", badge_prefix, "Even an aggressive ",
      "growth-rate or fee change wouldn't reach ", goal_label, " within this ",
      "forecast window on its own -- consider a longer horizon or a more ",
      "fundamental change to the revenue or cost structure.</p>"
    ))
  }

  # Lower-case, verb-first clauses -- read directly as the tail of "you'd
  # need to ___" in the single-lever case, or capitalized (below) as
  # standalone <li> items in the multi-lever case.
  income_clause <- if (income_ok) {
    paste0(
      "raise assumed income growth to about <strong>",
      gs$income$new_growth_pct, "%/yr</strong>",
      .format_members_annotation(gs$income$members_equiv)
    )
  } else {
    NULL
  }

  overhead_clause <- if (overhead_ok) {
    paste0(
      "lower assumed overhead growth to about <strong>",
      gs$overhead$new_growth_pct, "%/yr</strong>"
    )
  } else {
    NULL
  }

  # Only ever present when panel_size/membership_fee were supplied (i.e.
  # Membership Profile is filled in) -- see goal_seek_breakeven()'s own
  # doc for why this lever can be NULL even when income/overhead aren't.
  fee_clause <- if (fee_ok) {
    paste0(
      "raise your average membership fee by about <strong>",
      fmt_dollar(gs$fee$fee_increase), "/member/month</strong> (to about ",
      "<strong>", fmt_dollar(gs$fee$new_fee), "/member/month</strong>)"
    )
  } else {
    NULL
  }

  # Only ever present alongside both income_clause and overhead_clause --
  # see .solve_combined_growth_rate()'s own doc for how this blended
  # value is derived. Framed as "a smaller change to both" rather than
  # duplicating the "raise X, lower Y" phrasing of the two solo clauses,
  # so it reads as a distinct third option rather than a repeat of them.
  combined_clause <- if (combined_ok) {
    paste0(
      "make a smaller change to both together: raise income growth to ",
      "about <strong>", gs$combined$new_income_growth_pct,
      "%/yr</strong> and lower overhead growth to about <strong>",
      gs$combined$new_overhead_growth_pct, "%/yr</strong>"
    )
  } else {
    NULL
  }

  clauses <- c(income_clause, overhead_clause, fee_clause, combined_clause)

  if (length(clauses) == 1L) {
    return(paste0(
      "<p>", badge_prefix, "To reach ", goal_label, " within ", gs$target_period,
      " ", pu$plural, " on this assumption alone, you'd need to ", clauses[1],
      ".</p>"
    ))
  }

  capitalized <- vapply(clauses, function(x) {
    paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  }, character(1))

  paste0(
    "<p>", badge_prefix, "To reach ", goal_label, " within ", gs$target_period,
    " ", pu$plural, ", any one of the following would do it:</p>",
    "<ul class='mb-2'>",
    paste0("<li>", capitalized, ".</li>", collapse = ""),
    "</ul>"
  )
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


# -- Stress test: how much member loss could break-even absorb? --------------

# Uniformly reduce every forecast period's revenue by `members_lost *
# fee_pp` and the current-state scalars from the adjusted series.
# Represents a permanent panel shrinkage effective immediately (from
# period 1 onward), unlike apply_membership_fee_events()'s start-dated
# step changes.
#
# periods_to_breakeven is deliberately NOT re-derived here when the input
# was already 0L (already achieved) -- same convention
# apply_growth_assumptions() uses (see its own "already" comment): 0L
# specifically means "achieved before the forecast window even starts",
# a state a later per-period adjustment can't cleanly walk back to a
# positive crossing index. stress_test_breakeven() is only ever called
# on an already-0L result and relies entirely on breakeven_is_sustained()
# (which itself requires periods_to_breakeven == 0L) to judge whether the
# loss-adjusted series still holds up -- keeping periods_to_breakeven
# frozen at 0L is what lets that check actually run instead of always
# returning NA.
#' @noRd
.apply_member_loss <- function(result, members_lost, fee_pp) {
  if (members_lost <= 0) {
    return(result)
  }
  delta <- -(members_lost * fee_pp)
  fd <- result$forecast_data
  rev_cols <- intersect(
    c("revenue_forecast", "revenue_lower", "revenue_upper"),
    names(fd)
  )
  for (col in rev_cols) {
    fd[[col]] <- fd[[col]] + delta
  }
  result$forecast_data <- fd
  result$current_revenue <- result$current_revenue + delta
  result$current_surplus_deficit <- result$current_surplus_deficit + delta

  if (!identical(result$periods_to_breakeven, 0L) && "overhead_forecast" %in% names(fd)) {
    cross <- which(fd$revenue_forecast >= fd$overhead_forecast)
    if (length(cross) > 0L) {
      result$periods_to_breakeven <- cross[1L]
      result$breakeven_date <- fd$period_start[cross[1L]]
    } else {
      result$periods_to_breakeven <- NA_integer_
      result$breakeven_date <- as.Date(NA)
    }
  }
  result
}

#' Find how many members' worth of revenue an achieved break-even could lose
#'
#' The mirror image of [goal_seek_breakeven()]: instead of asking what
#' improvement would be needed to *reach* break-even, this asks how much
#' degradation an *already-achieved* break-even could absorb and still be
#' [breakeven_is_sustained()] through the forecast horizon -- a margin-of-
#' safety figure rather than a target-seeking one. Modeled as losing `n`
#' members' worth of `membership_fee`-equivalent revenue, uniformly across
#' every forecast period, via [.apply_member_loss()]. Monotonic in `n`
#' (losing more members can only ever hurt or leave sustainability
#' unchanged), which is what makes bisection valid here -- same approach as
#' [goal_seek_breakeven()]'s own bisection.
#'
#' @param adj_breakeven_result The fully adjusted break-even result -- i.e.
#'   after `apply_growth_assumptions()`, `apply_overhead_events()`, and
#'   `apply_membership_fee_events()` have already run (the same object
#'   `mod_projections.R`'s `adj_breakeven()` reactive holds), so the stress
#'   test reflects exactly what's on screen.
#' @param panel_size Current total panel size (all tiers combined).
#' @param membership_fee Average membership fee ($/member/month), matching
#'   the value [interpret_breakeven()] already receives.
#'
#' @return A `dcAnalytics_stress_test`-classed list with `members_loss_room`
#'   (integer, the largest number of members that can be lost while
#'   [breakeven_is_sustained()] stays `TRUE`) and `capped` (`TRUE` when
#'   losing the entire panel is still sustained -- an edge case worth
#'   flagging rather than reporting numerically, e.g. a fee-for-service-
#'   heavy practice). `NULL` when break-even isn't currently achieved,
#'   panel size/fee aren't supplied, or the achieved state isn't itself
#'   sustained (nothing to stress-test from).
#'
#' @noRd
stress_test_breakeven <- function(
  adj_breakeven_result,
  panel_size = NULL,
  membership_fee = NULL
) {
  if (is.null(adj_breakeven_result)) {
    return(NULL)
  }
  if (!identical(adj_breakeven_result$periods_to_breakeven, 0L)) {
    return(NULL)
  }
  if (is.null(panel_size) || is.na(panel_size) || panel_size <= 0) {
    return(NULL)
  }
  if (is.null(membership_fee) || is.na(membership_fee) || membership_fee <= 0) {
    return(NULL)
  }
  if (!isTRUE(breakeven_is_sustained(adj_breakeven_result))) {
    return(NULL)
  }

  fee_pp <- if (identical(adj_breakeven_result$frequency, "weekly")) {
    membership_fee / 4.33
  } else {
    membership_fee
  }

  .sustained_losing <- function(n) {
    isTRUE(breakeven_is_sustained(
      .apply_member_loss(adj_breakeven_result, n, fee_pp)
    ))
  }

  if (.sustained_losing(panel_size)) {
    return(structure(
      list(members_loss_room = panel_size, capped = TRUE),
      class = "dcAnalytics_stress_test"
    ))
  }

  lo <- 0
  hi <- panel_size
  for (i in seq_len(25)) {
    mid <- (lo + hi) / 2
    if (.sustained_losing(mid)) lo <- mid else hi <- mid
  }

  structure(
    list(members_loss_room = floor(lo), capped = FALSE),
    class = "dcAnalytics_stress_test"
  )
}

#' Turn a stress-test result into narrative HTML
#'
#' @param st A `dcAnalytics_stress_test` object, as returned by
#'   [stress_test_breakeven()].
#' @param pu Period-unit list, as returned by `.period_units()`.
#' @param badge Whether to prefix the output with `.pro_badge_html()`.
#'   See `.describe_goal_seek()`'s own doc for why the PDF report path
#'   passes `FALSE`.
#'
#' @return An HTML string (a single `<p>...</p>`), or `NULL` if `st` is
#'   `NULL`.
#'
#' @noRd
.describe_stress_test <- function(st, pu, badge = TRUE) {
  if (is.null(st)) {
    return(NULL)
  }

  badge_prefix <- if (isTRUE(badge)) paste0(.pro_badge_html(), " ") else ""

  if (isTRUE(st$capped)) {
    return(paste0(
      "<p>", badge_prefix, "Your margin is wide enough that even ",
      "losing your entire panel wouldn't put sustained break-even at risk ",
      "under current growth assumptions &mdash; fee-for-service or other ",
      "non-membership revenue is likely doing much of the work.</p>"
    ))
  }

  if (st$members_loss_room <= 0L) {
    return(paste0(
      "<p>", badge_prefix, "Your break-even margin is thin: losing ",
      "even a single member at current rates would put sustained ",
      "break-even at risk.</p>"
    ))
  }

  paste0(
    "<p>", badge_prefix, "Your current margin could absorb losing up ",
    "to <strong>", st$members_loss_room,
    " members</strong> before break-even would no longer be sustained ",
    "through the forecast horizon.</p>"
  )
}
