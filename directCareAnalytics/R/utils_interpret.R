#' Forecast interpretation helpers
#'
#' Each function generates a short narrative paragraph from a forecast result
#' list. The text is rule-based string interpolation -- no external API required.
#' Wrap the output in `shiny::HTML()` to render in the UI.
#'
#' @noRd

# Internal: proportion of forecast periods where col_a >= col_b (as a rounded %)
.coverage_pct <- function(fd, col_a, col_b) {
  a <- fd[[col_a]]
  b <- fd[[col_b]]
  n_total <- sum(!is.na(a) & !is.na(b))
  if (n_total == 0L) {
    return(NA_real_)
  }
  round(100 * sum(a >= b, na.rm = TRUE) / n_total)
}

# Internal: build a data-quality note paragraph from result$data_warnings.
# Returns an empty string when no warnings are present.
.data_warnings_html <- function(result) {
  msgs <- unique(result$data_warnings)
  if (is.null(msgs) || length(msgs) == 0L) {
    return("")
  }
  items <- paste0("<li>", msgs, "</li>", collapse = "")
  paste0(
    "<p class='text-muted small'><strong>Data quality note:</strong> ",
    "This forecast is based on limited data. Please review the following before ",
    "relying on these projections: <ul>",
    items,
    "</ul></p>"
  )
}

# Internal: return the method actually used, accounting for fallback.
# When the requested method couldn't run (e.g. ARIMA with too few obs),
# directCareForecastR falls back to linear and records it in data_warnings.
# result$method still holds the *requested* method, so we detect the fallback
# by scanning the warning text.
.actual_method <- function(result) {
  if (
    !is.null(result$data_warnings) &&
      any(grepl(
        "linear method was used instead",
        result$data_warnings,
        fixed = TRUE
      ))
  ) {
    "linear"
  } else {
    result$method
  }
}

# Internal: build a tier-context sentence for multi-tier panels.
# Returns "" for a single tier (the existing member_sentence is sufficient).
.tier_context_html <- function(membership_tiers, membership_fee) {
  if (is.null(membership_tiers) || length(membership_tiers) < 2L) {
    return("")
  }
  tier_parts <- vapply(
    membership_tiers,
    function(t) {
      lbl <- if (nzchar(t$label %||% "")) t$label else "Members"
      paste0(lbl, ": ", t$members, " @ ", fmt_dollar(t$fee), "/mo")
    },
    character(1)
  )
  paste0(
    "Your practice has <strong>",
    length(membership_tiers),
    " membership tiers</strong>: ",
    paste(tier_parts, collapse = "; "),
    if (!is.null(membership_fee) && membership_fee > 0) {
      paste0(
        " (weighted average <strong>",
        fmt_dollar(membership_fee),
        "/member/month</strong>)"
      )
    } else {
      ""
    },
    "."
  )
}

# Internal: derive period-unit strings from result$frequency.
# Returns a list: $singular ("month"/"week"), $plural ("months"/"weeks"),
# $per ("/month", "/week"), $adj ("monthly"/"weekly").
.period_units <- function(result) {
  weekly <- identical(result$frequency, "weekly")
  list(
    singular = if (weekly) "week" else "month",
    plural = if (weekly) "weeks" else "months",
    per = if (weekly) "/week" else "/month",
    adj = if (weekly) "weekly" else "monthly"
  )
}


#' @param goal_seek Optional `dcAnalytics_goal_seek` object, as returned by
#'   [goal_seek_breakeven()]. Only used when `result$breakeven_date` is
#'   `NA` (break-even not reached within the forecast horizon) -- ignored
#'   otherwise, so it's safe to always pass one in regardless of which case
#'   applies. `NULL` (the default) omits that paragraph.
#' @noRd
interpret_breakeven <- function(
  result,
  practice_name = NULL,
  sustained = NA,
  panel_size = NULL,
  membership_fee = NULL,
  membership_tiers = NULL,
  confidence_level = 0.95,
  goal_seek = NULL
) {
  name_str <- if (!is.null(practice_name) && nzchar(practice_name)) {
    paste0(htmltools::htmlEscape(practice_name), " ")
  } else {
    ""
  }

  pu <- .period_units(result)

  surplus <- result$current_surplus_deficit
  surplus_str <- fmt_dollar(abs(surplus))

  already_achieved <- identical(result$periods_to_breakeven, 0L)

  n_periods <- nrow(result$forecast_data)

  # Coverage percentage for both the "sustained" and "at risk" branches.
  bkevn_pct <- if ("overhead_forecast" %in% names(result$forecast_data)) {
    .coverage_pct(result$forecast_data, "revenue_forecast", "overhead_forecast")
  } else {
    NA_real_
  }

  coverage_sustained <- if (!is.na(bkevn_pct)) {
    paste0(
      " Based on current growth assumptions, income is projected to meet or exceed ",
      "overhead in <strong>",
      bkevn_pct,
      "%</strong> of the ",
      n_periods,
      "-",
      pu$singular,
      " forecast horizon &mdash; ",
      "this is a projection, not a guarantee."
    )
  } else {
    ""
  }

  coverage_at_risk <- if (!is.na(bkevn_pct)) {
    paste0(
      " Income meets or exceeds overhead in only <strong>",
      bkevn_pct,
      "%</strong>",
      " of the ",
      n_periods,
      "-",
      pu$singular,
      " forecast horizon."
    )
  } else {
    ""
  }

  status_sentence <- if (already_achieved && isTRUE(sustained)) {
    paste0(
      "<strong>",
      name_str,
      "is currently operating at a surplus</strong> of ",
      fmt_dollar(surplus),
      " per ",
      pu$singular,
      ".",
      coverage_sustained
    )
  } else if (already_achieved && isFALSE(sustained)) {
    paste0(
      "<strong>",
      name_str,
      "has reached break-even</strong> with a current surplus of ",
      fmt_dollar(surplus),
      ". However, the forecast projects overhead will outpace ",
      "revenue &mdash; <strong>break-even may not be sustained</strong> at current growth rates.",
      coverage_at_risk,
      " Try raising the income growth assumption or lowering overhead growth."
    )
  } else if (already_achieved) {
    paste0(
      "<strong>",
      name_str,
      "is currently operating at a surplus</strong> of ",
      fmt_dollar(surplus),
      " per ",
      pu$singular,
      ". Break-even has already been achieved."
    )
  } else if (surplus >= 0) {
    paste0(
      name_str,
      "is currently running a ",
      pu$adj,
      " surplus of ",
      surplus_str,
      "."
    )
  } else {
    paste0(
      name_str,
      "is currently running a ",
      pu$adj,
      " deficit of ",
      surplus_str,
      "."
    )
  }

  breakeven_sentence <- if (already_achieved) {
    "" # status_sentence already covers the full picture
  } else if (is.na(result$breakeven_date)) {
    paste0(
      "Based on the current revenue trend, <strong>break-even is not projected ",
      "within the ",
      n_periods,
      "-",
      pu$singular,
      " forecast window.</strong> ",
      "Consider strategies to accelerate revenue growth or reduce overhead."
    )
  } else {
    ci <- result$confidence_interval
    ci_str <- if (!is.null(ci) && !is.na(ci["lower"]) && !is.na(ci["upper"])) {
      paste0(
        " The ",
        round(confidence_level * 100),
        "% confidence interval spans ",
        format(ci["lower"], "%B %Y"),
        " to ",
        format(ci["upper"], "%B %Y"),
        ", reflecting statistical uncertainty in the revenue and overhead forecasts as well as any growth assumptions you have applied."
      )
    } else {
      ""
    }

    n_to_bkevn <- result$periods_to_breakeven
    paste0(
      "The model projects break-even in <strong>",
      format(result$breakeven_date, "%B %Y"),
      "</strong> (",
      n_to_bkevn,
      " ",
      pu$singular,
      if (!identical(n_to_bkevn, 1L)) "s" else "",
      " from now).",
      ci_str
    )
  }

  method_sentence <- paste0(
    "This projection uses the <em>",
    .actual_method(result),
    "</em> forecasting method ",
    "on ",
    result$frequency,
    " data."
  )

  # Optional member-count sentence when practice profile is provided.
  profile_has_fee <- isTRUE(
    !is.null(membership_fee) && !is.na(membership_fee) && membership_fee > 0
  )
  profile_has_both <- profile_has_fee &&
    isTRUE(!is.null(panel_size) && !is.na(panel_size) && panel_size > 0)

  # membership_fee is always $/member/month. When data are weekly, all
  # revenue/overhead quantities are in $/week, so divide by the weekly
  # equivalent of the fee for member-count arithmetic.
  fee_pp <- if (profile_has_fee) {
    if (identical(result$frequency, "weekly")) {
      membership_fee / 4.33
    } else {
      membership_fee
    }
  } else {
    NA_real_
  }

  # Use the rolling-average overhead (current_overhead_avg) when available.
  # For weekly data this is critical: monthly overhead is posted only on
  # month-start rows, so the raw single-period current_overhead is ~$0 for
  # most weeks. The average smooths across the last few overhead periods and
  # is converted to the income frequency's units inside forecast_breakeven().
  ovhd_for_members <- result$current_overhead_avg %||%
    result$current_overhead %||%
    (result$current_revenue - result$current_surplus_deficit)

  # Phrase the note differently depending on how many periods were averaged.
  n_avg <- result$overhead_avg_n %||% 1L
  ovhd_note <- if (n_avg > 1L) {
    paste0(
      " <span class='text-muted small'>(overhead estimated from the average of ",
      "the last ",
      n_avg,
      " ",
      pu$plural,
      " of data)</span>"
    )
  } else {
    ""
  }

  member_sentence <- if (profile_has_both) {
    members_bkevn <- ceiling(ovhd_for_members / fee_pp)
    pmpm <- result$current_revenue / panel_size
    paste0(
      "Your panel of <strong>",
      panel_size,
      " members</strong> is generating ",
      "<strong>",
      fmt_dollar(pmpm),
      pu$per,
      "</strong> per member on average. ",
      "At <strong>",
      fmt_dollar(membership_fee),
      "/member/month</strong>, ",
      "you need approximately <strong>",
      members_bkevn,
      " members</strong> to cover overhead",
      ovhd_note,
      "."
    )
  } else if (profile_has_fee) {
    members_bkevn <- ceiling(ovhd_for_members / fee_pp)
    paste0(
      "At <strong>",
      fmt_dollar(membership_fee),
      "/member/month</strong>, ",
      "you need approximately <strong>",
      members_bkevn,
      " members</strong> to cover overhead",
      ovhd_note,
      "."
    )
  } else {
    ""
  }

  tier_sentence <- .tier_context_html(membership_tiers, membership_fee)
  warnings_html <- .data_warnings_html(result)

  # Only meaningful when break-even hasn't been reached -- ignored
  # otherwise even if a caller passes one in, since "what would it take to
  # reach break-even" doesn't apply once it's already been achieved.
  goal_seek_html <- if (is.na(result$breakeven_date) && !is.null(goal_seek)) {
    .describe_goal_seek(goal_seek, pu, "break-even")
  } else {
    NULL
  }

  paste(
    "<p>",
    status_sentence,
    "</p>",
    if (nzchar(breakeven_sentence)) {
      paste0("<p>", breakeven_sentence, "</p>")
    } else {
      ""
    },
    if (!is.null(goal_seek_html)) goal_seek_html else "",
    if (nzchar(tier_sentence)) paste0("<p>", tier_sentence, "</p>") else "",
    if (nzchar(member_sentence)) paste0("<p>", member_sentence, "</p>") else "",
    if (nzchar(warnings_html)) warnings_html else "",
    "<p class='text-muted small'>",
    method_sentence,
    "</p>"
  )
}

#' Compare saved scenarios' break-even timing
#'
#' Pro-exclusive: names which of 2-3 saved forecast scenarios reaches
#' break-even soonest and by how much, using each scenario's own
#' snapshotted (growth-adjusted) break-even result --
#' `directCareScenarios::scenario_save_forecast()` bundles both inputs and
#' computed results at save time (see `SAVED_SCENARIOS.md`'s "Why BYTEA"
#' section), which is exactly what makes this cheap: no recomputation,
#' just reading and diffing numbers already on disk.
#'
#' @param scenarios A list of `list(label = <chr>, result =
#'   <adj_breakeven()-shaped list>)`, length 2 or 3 (the app's saved-
#'   scenario slot cap). Each `result` must be non-`NULL` -- the caller is
#'   responsible for filtering out scenarios that were saved before ever
#'   running a Break-even forecast, since those have no `adj_breakeven`
#'   snapshot to compare.
#'
#' @return An HTML string (a single `<p>...</p>`), or `NULL` if fewer than
#'   2 scenarios were supplied.
#'
#' @noRd
compare_breakeven_scenarios <- function(scenarios) {
  if (length(scenarios) < 2L) {
    return(NULL)
  }

  # Periods are only directly comparable in count when every scenario was
  # forecast at the same frequency (weekly vs. monthly periods aren't the
  # same unit) -- falls back to a generic "period" label and skips the
  # numeric gap when they differ, which in practice means a practice
  # switched data cadence between saves.
  freqs <- vapply(scenarios, function(s) s$result$frequency %||% NA_character_, character(1))
  same_freq <- length(unique(freqs)) == 1L && !anyNA(freqs)
  pu <- if (same_freq) {
    .period_units(scenarios[[1]]$result)
  } else {
    list(singular = "period", plural = "periods")
  }

  .lbl <- function(s) paste0("<strong>", htmltools::htmlEscape(s$label), "</strong>")
  .mo <- function(n) paste0(n, " ", if (n != 1L) pu$plural else pu$singular)

  reached <- Filter(function(s) !is.na(s$result$periods_to_breakeven), scenarios)
  not_reached <- Filter(function(s) is.na(s$result$periods_to_breakeven), scenarios)

  if (length(reached) == 0L) {
    labels <- paste(vapply(scenarios, .lbl, character(1)), collapse = " and ")
    return(paste0(
      "<p>None of your saved scenarios (", labels, ") reach break-even ",
      "within their forecast windows. The goal-seek breakdown above can ",
      "show what would need to change for your current one.</p>"
    ))
  }

  ord <- order(vapply(reached, function(s) s$result$periods_to_breakeven, numeric(1)))
  reached <- reached[ord]
  best <- reached[[1]]

  lead <- paste0(
    "Of your saved scenarios, ", .lbl(best), " reaches break-even soonest",
    if (same_freq) paste0(", in ", pu$singular, " ", best$result$periods_to_breakeven) else "",
    "."
  )

  follow_sentences <- if (length(reached) > 1L) {
    vapply(reached[-1], function(s) {
      if (same_freq) {
        gap <- s$result$periods_to_breakeven - best$result$periods_to_breakeven
        paste0(
          .lbl(s), " follows ", .mo(gap), " later, in ", pu$singular, " ",
          s$result$periods_to_breakeven, "."
        )
      } else {
        paste0(
          .lbl(s), " reaches break-even in ", pu$singular, " ",
          s$result$periods_to_breakeven, "."
        )
      }
    }, character(1))
  } else {
    character(0)
  }

  not_reached_sentence <- if (length(not_reached) > 0L) {
    labels <- paste(vapply(not_reached, .lbl, character(1)), collapse = " and ")
    if (length(not_reached) > 1L) {
      paste0(labels, " do not reach break-even within their forecast windows.")
    } else {
      paste0(labels, " does not reach break-even within its forecast window.")
    }
  } else {
    NULL
  }

  paste0("<p>", paste(c(lead, follow_sentences, not_reached_sentence), collapse = " "), "</p>")
}


#' @noRd
interpret_revenue <- function(
  result,
  practice_name = NULL,
  panel_size = NULL,
  membership_fee = NULL,
  membership_tiers = NULL,
  confidence_level = 0.95
) {
  name_str <- if (!is.null(practice_name) && nzchar(practice_name)) {
    paste0(htmltools::htmlEscape(practice_name), " ")
  } else {
    ""
  }

  pu <- .period_units(result)

  fd <- result$forecast_data
  current <- fmt_dollar(result$current_revenue)
  projected <- fmt_dollar(tail(fd$revenue_forecast, 1))
  horizon <- nrow(fd)
  pct_change <- round(
    (tail(fd$revenue_forecast, 1) - result$current_revenue) /
      abs(result$current_revenue) *
      100,
    1
  )
  direction <- if (pct_change >= 0) "grow" else "decline"
  pct_str <- paste0(abs(pct_change), "%")

  # Optional PMPM sentence when practice profile is provided.
  profile_has_panel <- isTRUE(
    !is.null(panel_size) && !is.na(panel_size) && panel_size > 0
  )
  profile_has_fee <- isTRUE(
    !is.null(membership_fee) && !is.na(membership_fee) && membership_fee > 0
  )

  pmpm_sentence <- if (profile_has_panel) {
    pmpm <- result$current_revenue / panel_size
    pmpm_str <- fmt_dollar(pmpm)
    fee_clause <- if (profile_has_fee) {
      # Convert monthly fee to per-period so units match the PMPM calculation.
      fee_pp_rev <- if (identical(result$frequency, "weekly")) {
        membership_fee / 4.33
      } else {
        membership_fee
      }
      diff <- pmpm - fee_pp_rev
      if (abs(diff) < fee_pp_rev * 0.05) {
        # Within 5% &mdash; consistent
        paste0(
          " This is close to your stated fee of ",
          fmt_dollar(membership_fee),
          "/member/month, suggesting your panel size and revenue data are consistent."
        )
      } else if (diff < 0) {
        # Below fee &mdash; possible new members, partial periods, or FFS offset
        paste0(
          " This is below your expected ",
          fmt_dollar(fee_pp_rev),
          pu$per,
          " (",
          fmt_dollar(membership_fee),
          "/member/month) &mdash; ",
          "your panel may include newer members or some revenue may not yet be captured."
        )
      } else {
        # Above fee &mdash; FFS or other income lifting average
        paste0(
          " This exceeds your expected ",
          fmt_dollar(fee_pp_rev),
          pu$per,
          " (",
          fmt_dollar(membership_fee),
          "/member/month), ",
          "likely reflecting fee-for-service or other income."
        )
      }
    } else {
      ""
    }
    paste0(
      "With a panel of <strong>",
      panel_size,
      " members</strong>, your practice ",
      "is generating approximately <strong>",
      pmpm_str,
      pu$per,
      "</strong> per member.",
      fee_clause
    )
  } else {
    ""
  }

  warnings_html <- .data_warnings_html(result)

  paste0(
    "<p>",
    name_str,
    "is currently generating <strong>",
    current,
    "</strong> per ",
    pu$singular,
    " in revenue. ",
    "The <em>",
    .actual_method(result),
    "</em> model projects revenue to <strong>",
    direction,
    " by ",
    pct_str,
    "</strong> over the next ",
    horizon,
    " ",
    pu$plural,
    ", ",
    "reaching approximately <strong>",
    projected,
    pu$per,
    "</strong> by ",
    format(tail(fd$period_start, 1), "%B %Y"),
    ".</p>",
    if (nzchar(pmpm_sentence)) paste0("<p>", pmpm_sentence, "</p>") else "",
    {
      ts <- .tier_context_html(membership_tiers, membership_fee)
      if (nzchar(ts)) paste0("<p>", ts, "</p>") else ""
    },
    paste0(
      "<p>The shaded region represents the ",
      round(confidence_level * 100),
      "% confidence interval &mdash; actual "
    ),
    "results may fall anywhere within that band depending on enrollment ",
    "changes, fee-for-service volume, and seasonal variation.</p>",
    if (nzchar(warnings_html)) warnings_html else "",
    "<p class='text-muted small'>Method: <em>",
    .actual_method(result),
    "</em> on ",
    result$frequency,
    " data.</p>"
  )
}


#' @param goal_seek Optional `dcAnalytics_goal_seek` object, as returned by
#'   [goal_seek_target()]. Only used when `result$target_date` is `NA`
#'   (target not reached within the forecast horizon) -- ignored
#'   otherwise, so it's safe to always pass one in regardless of which
#'   case applies. `NULL` (the default) omits that paragraph.
#' @noRd
interpret_target <- function(
  result,
  practice_name = NULL,
  target_income = NULL,
  sustained = NA,
  panel_size = NULL,
  membership_fee = NULL,
  membership_tiers = NULL,
  goal_seek = NULL
) {
  name_str <- if (!is.null(practice_name) && nzchar(practice_name)) {
    paste0(htmltools::htmlEscape(practice_name), " ")
  } else {
    ""
  }

  pu <- .period_units(result)

  target_str <- if (!is.null(target_income)) {
    fmt_dollar(target_income)
  } else {
    "the target"
  }
  req_now <- fmt_dollar(result$required_revenue_now)
  gap <- result$current_gap
  gap_str <- fmt_dollar(abs(gap))

  already_met <- isTRUE(gap >= 0)

  # Forward-looking snapshot values used in the "not yet met" branches.
  fd <- result$forecast_data
  n_periods <- nrow(fd)
  has_ovhd_col <- "overhead_forecast" %in% names(fd)
  has_req_col <- "required_revenue" %in% names(fd)

  # At-target snapshot (when the crossing period is known).
  at_tgt_idx <- result$periods_to_target
  at_tgt_ok <- !is.null(at_tgt_idx) &&
    !is.na(at_tgt_idx) &&
    at_tgt_idx >= 1L &&
    at_tgt_idx <= n_periods

  rev_at_target <- if (at_tgt_ok) fd$revenue_forecast[at_tgt_idx] else NA_real_
  ovhd_at_target <- if (at_tgt_ok && has_ovhd_col) {
    fd$overhead_forecast[at_tgt_idx]
  } else {
    NA_real_
  }
  req_at_target <- if (at_tgt_ok && has_req_col) {
    fd$required_revenue[at_tgt_idx]
  } else {
    NA_real_
  }

  # End-of-horizon snapshot (used when target is not reached within the window).
  rev_end <- if (n_periods > 0) tail(fd$revenue_forecast, 1L) else NA_real_
  ovhd_end <- if (n_periods > 0 && has_ovhd_col) {
    tail(fd$overhead_forecast, 1L)
  } else {
    NA_real_
  }
  req_end <- if (n_periods > 0 && has_req_col) {
    tail(fd$required_revenue, 1L)
  } else {
    NA_real_
  }
  end_date <- if (n_periods > 0) {
    format(tail(fd$period_start, 1L), "%B %Y")
  } else {
    NULL
  }

  # Coverage percentage for both the "sustained" and "at risk" branches.
  tgt_pct <- if (has_req_col) {
    .coverage_pct(fd, "revenue_forecast", "required_revenue")
  } else {
    NA_real_
  }

  tgt_coverage_sustained <- if (!is.na(tgt_pct)) {
    paste0(
      " Based on current growth assumptions, revenue is projected to meet or exceed ",
      "the required level in <strong>",
      tgt_pct,
      "%</strong> of the ",
      n_periods,
      "-",
      pu$singular,
      " forecast horizon &mdash; ",
      "this is a projection, not a guarantee."
    )
  } else {
    ""
  }

  tgt_coverage_at_risk <- if (!is.na(tgt_pct)) {
    paste0(
      " Revenue is projected to meet or exceed the required level in only ",
      "<strong>",
      tgt_pct,
      "%</strong>",
      " of the ",
      n_periods,
      "-",
      pu$singular,
      " forecast horizon."
    )
  } else {
    ""
  }

  gap_sentence <- if (already_met && isTRUE(sustained)) {
    paste0(
      name_str,
      "is already generating sufficient revenue to meet the ",
      target_str,
      pu$per,
      " net income target (current surplus of <strong>",
      gap_str,
      "</strong> above the required level).",
      tgt_coverage_sustained
    )
  } else if (already_met && isFALSE(sustained)) {
    paste0(
      name_str,
      "is currently meeting the ",
      target_str,
      pu$per,
      " net income target (surplus of <strong>",
      gap_str,
      "</strong>). ",
      "However, revenue growth may not keep pace with the required level &mdash; ",
      "<strong>the target position may not be sustained</strong> at current rates.",
      tgt_coverage_at_risk,
      " Consider raising the income growth assumption or reviewing overhead expectations."
    )
  } else if (already_met) {
    paste0(
      name_str,
      "is already generating sufficient revenue to meet the ",
      target_str,
      pu$per,
      " net income target. Current revenue exceeds the ",
      "required threshold by <strong>",
      gap_str,
      "</strong>."
    )
  } else {
    paste0(
      "To achieve a net income of <strong>",
      target_str,
      pu$per,
      "</strong>, ",
      name_str,
      "needs to generate at least <strong>",
      req_now,
      pu$per,
      "</strong> in revenue &mdash; <strong>",
      gap_str,
      " more</strong> than the current level."
    )
  }

  # Coverage percentage for the "not yet met" branch
  tgt_pct_not_met <- if (!already_met && !is.na(tgt_pct)) {
    paste0(
      " Under current assumptions, revenue is projected to meet or exceed ",
      "the required level in <strong>",
      tgt_pct,
      "%</strong> of the ",
      n_periods,
      "-",
      pu$singular,
      " forecast horizon."
    )
  } else {
    ""
  }

  # Only show a projection sentence when the target has not yet been met.
  target_sentence <- if (already_met) {
    "" # gap_sentence already covers the sustained/at-risk picture
  } else if (is.na(result$target_date)) {
    end_snapshot <- if (
      !is.na(rev_end) && !is.na(ovhd_end) && !is.na(req_end)
    ) {
      shortfall <- req_end - rev_end
      paste0(
        " By ",
        end_date,
        ", overhead is projected at <strong>",
        fmt_dollar(ovhd_end),
        pu$per,
        "</strong> and revenue at <strong>",
        fmt_dollar(rev_end),
        pu$per,
        "</strong> &mdash; still ",
        "<strong>",
        fmt_dollar(shortfall),
        "</strong> short of the ",
        fmt_dollar(req_end),
        " needed to cover expenses and reach the ",
        target_str,
        " net income target."
      )
    } else {
      ""
    }
    paste0(
      "<strong>The target is not reached within the ",
      n_periods,
      "-",
      pu$singular,
      " forecast window.</strong> At the current growth rate, additional ",
      "membership enrollment or fee-for-service revenue will be needed to ",
      "close the gap.",
      end_snapshot,
      tgt_pct_not_met
    )
  } else {
    n_to_tgt <- result$periods_to_target
    at_tgt_snapshot <- if (!is.na(rev_at_target) && !is.na(ovhd_at_target)) {
      paste0(
        " At that point, overhead is projected at <strong>",
        fmt_dollar(ovhd_at_target),
        pu$per,
        "</strong> and projected revenue of <strong>",
        fmt_dollar(rev_at_target),
        pu$per,
        "</strong> would cover expenses ",
        "and deliver the ",
        target_str,
        " net income target."
      )
    } else {
      ""
    }
    paste0(
      "The model projects the target will be reached in <strong>",
      format(result$target_date, "%B %Y"),
      "</strong> (",
      n_to_tgt,
      " ",
      pu$singular,
      if (!identical(n_to_tgt, 1L)) "s" else "",
      " from now).",
      at_tgt_snapshot,
      tgt_pct_not_met
    )
  }

  disclaimer <- if (!already_met) {
    paste0(
      "<p class='text-muted small'>",
      "These projections are based on the observed revenue trend and the ",
      "growth assumptions provided above. They are estimates, not guarantees &mdash; ",
      "actual results will depend on enrollment changes, fee-for-service volume, ",
      "overhead fluctuations, and other practice-specific factors.</p>"
    )
  } else {
    ""
  }

  # Optional member-count sentence when membership fee is provided.
  profile_has_fee <- isTRUE(
    !is.null(membership_fee) && !is.na(membership_fee) && membership_fee > 0
  )
  profile_has_panel <- isTRUE(
    !is.null(panel_size) && !is.na(panel_size) && panel_size > 0
  )

  member_tgt_sentence <- if (profile_has_fee) {
    # Convert monthly fee to per-period so member count uses consistent units.
    fee_pp_tgt <- if (identical(result$frequency, "weekly")) {
      membership_fee / 4.33
    } else {
      membership_fee
    }
    members_needed <- ceiling(result$required_revenue_now / fee_pp_tgt)
    gap_clause <- if (profile_has_panel) {
      members_gap <- members_needed - panel_size
      if (members_gap <= 0) {
        paste0(
          " Your current panel of <strong>",
          panel_size,
          " members</strong> already covers this."
        )
      } else {
        paste0(
          " That is <strong>",
          members_gap,
          " more</strong> than your ",
          "current panel of ",
          panel_size,
          " members."
        )
      }
    } else {
      ""
    }
    paste0(
      "At <strong>",
      fmt_dollar(membership_fee),
      "/member/month</strong>, ",
      "reaching the required revenue would take approximately ",
      "<strong>",
      members_needed,
      " members</strong>.",
      gap_clause
    )
  } else {
    ""
  }

  warnings_html <- .data_warnings_html(result)

  tier_sentence_tgt <- .tier_context_html(membership_tiers, membership_fee)

  # Only meaningful when the target hasn't been reached -- ignored
  # otherwise even if a caller passes one in, since "what would it take to
  # reach the target" doesn't apply once it's already been met.
  goal_seek_html <- if (is.na(result$target_date) && !is.null(goal_seek)) {
    .describe_goal_seek(goal_seek, pu, "the income target")
  } else {
    NULL
  }

  paste0(
    "<p>",
    gap_sentence,
    "</p>",
    if (nzchar(target_sentence)) paste0("<p>", target_sentence, "</p>") else "",
    if (!is.null(goal_seek_html)) goal_seek_html else "",
    if (nzchar(tier_sentence_tgt)) {
      paste0("<p>", tier_sentence_tgt, "</p>")
    } else {
      ""
    },
    if (nzchar(member_tgt_sentence)) {
      paste0("<p>", member_tgt_sentence, "</p>")
    } else {
      ""
    },
    disclaimer,
    if (nzchar(warnings_html)) warnings_html else "",
    "<p class='text-muted small'>Method: <em>",
    .actual_method(result),
    "</em> on ",
    result$frequency,
    " data.</p>"
  )
}
