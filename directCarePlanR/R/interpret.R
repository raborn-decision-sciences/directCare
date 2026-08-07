# A conservative-to-optimistic break-even spread wider than this many
# months is called out as "highly sensitive" in interpret_projection();
# narrower is called "relatively robust." Documented, adjustable constant
# rather than a magic number buried in prose.
.projection_sensitivity_threshold_months <- 6L

#' Format a Dollar Amount for Narrative Text
#'
#' @param x Numeric value.
#'
#' @noRd
.fmt_dollar <- function(x) {
  sign <- ifelse(x < 0, "-", "")
  paste0(sign, "$", formatC(abs(round(x)), format = "d", big.mark = ","))
}

# Known all-caps abbreviations to preserve when humanizing a snake_case
# key (e.g. cost line-item names). Extend as new ones show up.
.label_acronyms <- c(ehr = "EHR")

#' Turn a Snake-Case Key Into a Readable Label
#'
#' E.g. `"ehr_setup"` -> `"EHR Setup"`. Mirrors the `humanize()` Typst
#' function in `inst/report/report.typ`, which applies the same
#' transformation to keys rendered directly in the PDF report; this is
#' the R-side equivalent for keys that get woven into narrative text
#' instead.
#'
#' @param key Character scalar, e.g. a startup cost line-item name.
#'
#' @noRd
.humanize_label <- function(key) {
  words <- strsplit(key, "_", fixed = TRUE)[[1]]
  words <- vapply(words, function(w) {
    if (w %in% names(.label_acronyms)) {
      .label_acronyms[[w]]
    } else {
      paste0(toupper(substr(w, 1, 1)), substr(w, 2, nchar(w)))
    }
  }, character(1))
  paste(words, collapse = " ")
}

#' Interpret Revenue Projections
#'
#' Generates narrative text describing a revenue projection, framed
#' decisionally: what the fee structure requires to hit target revenue,
#' rather than a diagnostic description of the numbers alone.
#'
#' @param revenue A `dcPlanR_revenue` object, as returned by
#'   [calc_mixed_revenue()].
#'
#' @return A character string of narrative text, with paragraphs separated
#'   by `"\n\n"`.
#'
#' @export
interpret_revenue <- function(revenue) {
  has_membership <- !is.null(revenue$membership)
  has_fee <- !is.null(revenue$fee_for_service)

  membership_final <- if (has_membership) utils::tail(revenue$membership$revenue, 1) else 0
  fee_final <- if (has_fee) utils::tail(revenue$fee_for_service$revenue, 1) else 0
  total_final <- membership_final + fee_final

  composition <- if (has_membership && has_fee) {
    membership_pct <- round(100 * membership_final / total_final)
    fee_pct <- 100 - membership_pct
    paste0(
      "At full ramp, this plan generates ",
      .fmt_dollar(total_final),
      "/month in total revenue: ",
      .fmt_dollar(membership_final),
      " (",
      membership_pct,
      "%) from membership fees and ",
      .fmt_dollar(fee_final),
      " (",
      fee_pct,
      "%) from fee-for-service visits."
    )
  } else if (has_membership) {
    paste0(
      "At full ramp, this plan generates ",
      .fmt_dollar(membership_final),
      "/month in membership revenue, with no fee-for-service component."
    )
  } else {
    paste0(
      "At full ramp, this plan generates ",
      .fmt_dollar(fee_final),
      "/month in fee-for-service revenue, with no membership component."
    )
  }

  # Membership revenue is linear in fee (panel_size x fee); fee-for-service
  # revenue is linear in visit_volume (visit_volume x fee mix). A 10% shift
  # in either input scales the corresponding final-month revenue by exactly
  # 10%, with no need to re-derive the original assumption values.
  membership_shift <- membership_final * 0.10
  fee_shift <- fee_final * 0.10

  sensitivity <- if (has_membership && has_fee) {
    if (membership_shift >= fee_shift) {
      paste0(
        "The membership fee is the most sensitive lever in this plan: a 10% ",
        "change in fee shifts monthly revenue by about ",
        .fmt_dollar(membership_shift),
        ", versus about ",
        .fmt_dollar(fee_shift),
        " for an equivalent 10% change in fee-for-service visit volume."
      )
    } else {
      paste0(
        "Fee-for-service visit volume is the most sensitive lever in this ",
        "plan: a 10% change in visit volume shifts monthly revenue by about ",
        .fmt_dollar(fee_shift),
        ", versus about ",
        .fmt_dollar(membership_shift),
        " for an equivalent 10% change in the membership fee."
      )
    }
  } else if (has_membership) {
    paste0(
      "A 10% change in the membership fee shifts monthly revenue by about ",
      .fmt_dollar(membership_shift),
      "."
    )
  } else {
    paste0(
      "A 10% change in fee-for-service visit volume shifts monthly revenue ",
      "by about ",
      .fmt_dollar(fee_shift),
      "."
    )
  }

  paste(composition, sensitivity, sep = "\n\n")
}

#' Interpret a Financial Projection
#'
#' Generates narrative text describing a scenario projection, calling out
#' the most sensitive assumption and what would need to be true for the
#' practice to break even within the projection horizon.
#'
#' @param projection A tibble as returned by [project_scenarios()].
#' @param sensitivity_decomposition Optional
#'   `dcPlanR_sensitivity_decomposition` object, as returned by
#'   [decompose_projection_sensitivity()]. When supplied, an additional
#'   paragraph names which assumption (membership ramp speed or overhead)
#'   drives more of the conservative-to-optimistic spread. `NULL` (the
#'   default) omits that paragraph entirely, preserving this function's
#'   original output for callers that don't compute a decomposition.
#' @param goal_seek Optional `dcPlanR_goal_seek` object, as returned by
#'   [goal_seek_projection_recovery()]. Only used when the base scenario
#'   does not recover within the horizon -- ignored otherwise, so it's safe
#'   to always pass one in regardless of which case applies. `NULL` (the
#'   default) omits that paragraph.
#'
#' @return A character string of narrative text, with paragraphs separated
#'   by `"\n\n"`.
#'
#' @export
interpret_projection <- function(
  projection,
  sensitivity_decomposition = NULL,
  goal_seek = NULL
) {
  horizon_months <- max(projection$month)

  # First month each scenario's cumulative net income turns non-negative --
  # the point the practice recovers its ramp-period cash burn, a more
  # meaningful break-even for a not-yet-existing practice than a single
  # month's net_income crossing zero.
  recovery_month <- function(scenario_name) {
    scenario_data <- projection[projection$scenario == scenario_name, ]
    scenario_data <- scenario_data[order(scenario_data$month), ]
    idx <- which(scenario_data$cumulative_net_income >= 0)[1]
    if (is.na(idx)) NA_integer_ else scenario_data$month[idx]
  }

  base_month <- recovery_month("base")
  conservative_month <- recovery_month("conservative")
  optimistic_month <- recovery_month("optimistic")

  .describe_month <- function(m) {
    if (is.na(m)) {
      paste0("not within the ", horizon_months, "-month projection")
    } else {
      paste0("month ", m)
    }
  }

  base_sentence <- if (is.na(base_month)) {
    paste0(
      "Under base assumptions, this plan does not recover its ramp-period ",
      "costs within the ",
      horizon_months,
      "-month projection."
    )
  } else {
    paste0(
      "Under base assumptions, this plan recovers its ramp-period costs by ",
      .describe_month(base_month),
      "."
    )
  }

  scenario_sentence <- paste0(
    "Under conservative assumptions (slower panel growth, higher overhead), ",
    "that point shifts to ",
    .describe_month(conservative_month),
    "; under optimistic assumptions, it is reached by ",
    .describe_month(optimistic_month),
    "."
  )

  spread_sentence <- if (!is.na(conservative_month) && !is.na(optimistic_month)) {
    spread <- conservative_month - optimistic_month
    if (spread > .projection_sensitivity_threshold_months) {
      paste0(
        "This ",
        spread,
        "-month spread between the conservative and optimistic scenarios ",
        "indicates the plan is highly sensitive to your growth and overhead ",
        "assumptions -- validate them carefully before committing capital."
      )
    } else {
      paste0(
        "This ",
        spread,
        "-month spread between the conservative and optimistic scenarios ",
        "indicates the plan is relatively robust to variation in your growth ",
        "and overhead assumptions."
      )
    }
  } else {
    paste0(
      "One or more scenarios do not recover ramp-period costs within the ",
      horizon_months,
      "-month projection, so the conservative-to-optimistic timeline ",
      "cannot be compared directly -- consider extending the projection ",
      "horizon or revisiting the underlying assumptions."
    )
  }

  decomposition_sentence <- if (!is.null(sensitivity_decomposition)) {
    .describe_sensitivity_decomposition(sensitivity_decomposition)
  } else {
    NULL
  }

  # Only meaningful when the base scenario doesn't recover -- ignored
  # otherwise even if a caller passes one in, since "what would it take to
  # recover" doesn't apply to a plan that already does.
  goal_seek_sentence <- if (is.na(base_month) && !is.null(goal_seek)) {
    .describe_goal_seek(goal_seek)
  } else {
    NULL
  }

  # c() silently drops any NULL (no decomposition/goal-seek supplied, or
  # either had nothing computable to say), so this collapses to the
  # original 3-paragraph output when neither is passed.
  paste(
    c(
      base_sentence,
      scenario_sentence,
      spread_sentence,
      decomposition_sentence,
      goal_seek_sentence
    ),
    collapse = "\n\n"
  )
}

#' Decompose Break-Even Sensitivity by Assumption
#'
#' [interpret_projection()] reports how far apart the conservative and
#' optimistic break-even timelines are, but not which underlying assumption
#' drives that spread. This isolates the two levers [project_scenarios()]
#' varies together (membership ramp speed and monthly overhead) by
#' re-projecting with each one varied independently, holding the other at
#' its base value.
#'
#' @param assumptions Base assumptions list, as passed to
#'   [project_practice()].
#' @param horizon_months Integer horizon, matching the value used to
#'   produce the projection being interpreted. Defaults to 24.
#' @param scenario_params Optional, as passed to [project_scenarios()] --
#'   must match what actually produced the conservative/optimistic
#'   scenarios being decomposed, or the two won't correspond.
#'
#' @return A `dcPlanR_sensitivity_decomposition`-classed list with
#'   `ramp_spread_months` (integer or `NA`; `NA` when `assumptions` has no
#'   `membership_args$ramp_months` to vary, or when either isolated
#'   scenario never recovers within the horizon), `overhead_spread_months`
#'   (integer or `NA`, same rule), and `dominant_lever` (`"ramp"`,
#'   `"overhead"`, or `NA` when inconclusive -- neither lever computable,
#'   or an exact tie).
#'
#' @export
decompose_projection_sensitivity <- function(
  assumptions,
  horizon_months = 24L,
  scenario_params = NULL
) {
  default_params <- list(
    conservative = list(ramp_months_multiplier = 1.5, overhead_multiplier = 1.1),
    optimistic = list(ramp_months_multiplier = 0.75, overhead_multiplier = 0.9)
  )
  params <- utils::modifyList(default_params, rlang::`%||%`(scenario_params, list()))

  has_ramp <- !is.null(assumptions$membership_args$ramp_months)

  .recovery_month <- function(proj) {
    idx <- which(proj$cumulative_net_income >= 0)[1]
    if (is.na(idx)) NA_integer_ else proj$month[idx]
  }

  # Vary exactly one lever at a time (the other stays at its base value),
  # unlike project_scenarios()'s conservative/optimistic, which vary both
  # together -- that's what isolates each lever's individual contribution.
  .isolated_recovery <- function(lever, direction) {
    varied <- assumptions
    mult <- params[[direction]]
    if (identical(lever, "ramp")) {
      varied$membership_args$ramp_months <- max(
        1L,
        round(assumptions$membership_args$ramp_months * mult$ramp_months_multiplier)
      )
    } else {
      varied$overhead_monthly <- assumptions$overhead_monthly * mult$overhead_multiplier
    }
    .recovery_month(project_practice(varied, horizon_months))
  }

  ramp_spread <- if (has_ramp) {
    cons <- .isolated_recovery("ramp", "conservative")
    opt <- .isolated_recovery("ramp", "optimistic")
    if (is.na(cons) || is.na(opt)) NA_integer_ else cons - opt
  } else {
    NA_integer_
  }

  overhead_cons <- .isolated_recovery("overhead", "conservative")
  overhead_opt <- .isolated_recovery("overhead", "optimistic")
  overhead_spread <- if (is.na(overhead_cons) || is.na(overhead_opt)) {
    NA_integer_
  } else {
    overhead_cons - overhead_opt
  }

  dominant_lever <- if (is.na(ramp_spread) && is.na(overhead_spread)) {
    NA_character_
  } else if (is.na(ramp_spread)) {
    "overhead"
  } else if (is.na(overhead_spread)) {
    "ramp"
  } else if (abs(ramp_spread) == abs(overhead_spread)) {
    NA_character_
  } else if (abs(ramp_spread) > abs(overhead_spread)) {
    "ramp"
  } else {
    "overhead"
  }

  structure(
    list(
      ramp_spread_months = ramp_spread,
      overhead_spread_months = overhead_spread,
      dominant_lever = dominant_lever
    ),
    class = "dcPlanR_sensitivity_decomposition"
  )
}

#' Turn a Sensitivity Decomposition Into Narrative Text
#'
#' @param decomp A `dcPlanR_sensitivity_decomposition` object, as returned
#'   by [decompose_projection_sensitivity()].
#'
#' @return A character string, or `NULL` if neither lever was computable.
#'
#' @noRd
.describe_sensitivity_decomposition <- function(decomp) {
  ramp <- decomp$ramp_spread_months
  overhead <- decomp$overhead_spread_months

  # "1 months" reads as a typo, not a rounding artifact -- worth the extra
  # branch for a number that legitimately comes up (a lever with only a
  # 1-month effect on its own).
  .mo <- function(n) paste0(abs(n), " month", if (abs(n) != 1L) "s" else "")

  if (is.na(ramp) && is.na(overhead)) {
    return(NULL)
  }

  if (is.na(ramp)) {
    return(paste0(
      "This plan has no membership ramp to vary, so overhead is the only ",
      "lever behind that spread: varying it alone shifts break-even by ",
      "about ", .mo(overhead), "."
    ))
  }

  if (is.na(overhead)) {
    return(paste0(
      "Membership ramp speed is the only lever behind that spread here: ",
      "varying it alone shifts break-even by about ", .mo(ramp), "."
    ))
  }

  if (is.na(decomp$dominant_lever)) {
    return(paste0(
      "Membership ramp speed and overhead growth contribute about equally ",
      "to that spread (roughly ", .mo(ramp), " and ", .mo(overhead),
      " on their own, respectively) -- tightening either assumption alone ",
      "won't meaningfully narrow it."
    ))
  }

  if (identical(decomp$dominant_lever, "ramp")) {
    paste0(
      "Of that spread, membership ramp speed is the larger driver: varying ",
      "it alone shifts break-even by about ", .mo(ramp), ", versus about ",
      .mo(overhead), " for overhead alone. Validating your growth-rate ",
      "assumption carefully will do more to firm up this timeline than ",
      "validating overhead."
    )
  } else {
    paste0(
      "Of that spread, overhead growth is the larger driver: varying it ",
      "alone shifts break-even by about ", .mo(overhead), ", versus about ",
      .mo(ramp), " for membership ramp speed alone. Validating your ",
      "overhead assumption carefully will do more to firm up this timeline ",
      "than validating growth."
    )
  }
}

#' Find What It Would Take to Recover Within the Horizon
#'
#' When the base scenario in a projection does not recover its ramp-period
#' costs within the horizon (see [interpret_projection()]), this finds how
#' much the overhead level or the membership ramp speed alone would need to
#' improve for the base scenario to recover by `target_month`, holding the
#' other lever fixed -- the same isolation approach as
#' [decompose_projection_sensitivity()], applied as a search instead of a
#' fixed +/-10%/25% perturbation.
#'
#' @param assumptions Base assumptions list, as passed to
#'   [project_practice()].
#' @param horizon_months Integer horizon, matching the value used to
#'   produce the projection being interpreted. Defaults to 24.
#' @param target_month Integer month by which recovery should be reached.
#'   Defaults to `horizon_months` (the natural target when the base
#'   scenario doesn't recover at all within it).
#'
#' @return A `dcPlanR_goal_seek`-classed list with `target_month`, `overhead`
#'   (a list with `achievable` and, when `TRUE`, `pct_cut` and
#'   `new_overhead_monthly`), and `ramp` (`NULL` when `assumptions` has no
#'   `membership_args$ramp_months` to vary; otherwise a list with
#'   `achievable` and, when `TRUE`, `new_ramp_months` and `months_faster`).
#'   Each lever is searched independently down to an extreme floor (2% of
#'   original overhead; a 1-month ramp) -- `achievable == FALSE` means even
#'   that extreme isn't enough on its own.
#'
#' @export
goal_seek_projection_recovery <- function(
  assumptions,
  horizon_months = 24L,
  target_month = horizon_months
) {
  has_ramp <- !is.null(assumptions$membership_args$ramp_months)

  .recovery_at <- function(varied) {
    proj <- project_practice(varied, horizon_months)
    idx <- which(proj$cumulative_net_income >= 0)[1]
    if (is.na(idx)) NA_integer_ else proj$month[idx]
  }

  # Bisection for the largest multiplier m in [m_min, 1] (i.e. the smallest
  # change from today's value) such that recovery_at(apply_multiplier(m))
  # <= target_month. Monotonic in m for both levers (lower overhead and
  # faster ramp each only ever help or leave recovery unchanged), which is
  # what makes bisection valid here.
  .solve_multiplier <- function(apply_multiplier, m_min) {
    at_floor <- .recovery_at(apply_multiplier(m_min))
    if (is.na(at_floor) || at_floor > target_month) {
      return(NA_real_) # not achievable even at the extreme
    }
    lo <- m_min
    hi <- 1
    for (i in seq_len(25)) {
      mid <- (lo + hi) / 2
      r <- .recovery_at(apply_multiplier(mid))
      if (!is.na(r) && r <= target_month) lo <- mid else hi <- mid
    }
    lo
  }

  overhead_m <- .solve_multiplier(
    function(m) {
      v <- assumptions
      v$overhead_monthly <- assumptions$overhead_monthly * m
      v
    },
    m_min = 0.02
  )
  overhead_result <- if (is.na(overhead_m)) {
    list(achievable = FALSE)
  } else {
    list(
      achievable = TRUE,
      pct_cut = round(100 * (1 - overhead_m)),
      new_overhead_monthly = assumptions$overhead_monthly * overhead_m
    )
  }

  ramp_result <- if (has_ramp) {
    ramp_m <- .solve_multiplier(
      function(m) {
        v <- assumptions
        v$membership_args$ramp_months <- max(
          1L,
          round(assumptions$membership_args$ramp_months * m)
        )
        v
      },
      m_min = 1 / assumptions$membership_args$ramp_months
    )
    if (is.na(ramp_m)) {
      list(achievable = FALSE)
    } else {
      new_ramp <- max(1L, round(assumptions$membership_args$ramp_months * ramp_m))
      list(
        achievable = TRUE,
        original_ramp_months = assumptions$membership_args$ramp_months,
        new_ramp_months = new_ramp,
        months_faster = assumptions$membership_args$ramp_months - new_ramp
      )
    }
  } else {
    NULL
  }

  structure(
    list(
      target_month = target_month,
      overhead = overhead_result,
      ramp = ramp_result
    ),
    class = "dcPlanR_goal_seek"
  )
}

#' Turn a Goal-Seek Result Into Narrative Text
#'
#' @param gs A `dcPlanR_goal_seek` object, as returned by
#'   [goal_seek_projection_recovery()].
#'
#' @return A character string, or `NULL` if neither lever is achievable
#'   (nothing constructive to suggest).
#'
#' @noRd
.describe_goal_seek <- function(gs) {
  ovhd <- gs$overhead
  ramp <- gs$ramp

  ovhd_ok <- isTRUE(ovhd$achievable)
  ramp_ok <- !is.null(ramp) && isTRUE(ramp$achievable)

  if (!ovhd_ok && !ramp_ok) {
    return(paste0(
      "Even an aggressive overhead cut", if (!is.null(ramp)) " or reaching your target panel size almost immediately" else "",
      " wouldn't recover ramp-period costs by month ", gs$target_month,
      " on its own -- consider extending the horizon or revisiting the ",
      "revenue model itself, not just these assumptions."
    ))
  }

  ovhd_clause <- if (ovhd_ok) {
    paste0(
      "cut monthly overhead by about ", ovhd$pct_cut, "% (to roughly ",
      .fmt_dollar(ovhd$new_overhead_monthly), ")"
    )
  } else {
    NULL
  }

  ramp_clause <- if (ramp_ok) {
    paste0(
      "shorten your membership ramp from ", ramp$original_ramp_months,
      " to about ", ramp$new_ramp_months, " month",
      if (ramp$new_ramp_months != 1L) "s" else "",
      " (", ramp$months_faster, " month", if (ramp$months_faster != 1L) "s" else "",
      " faster)"
    )
  } else {
    NULL
  }

  clauses <- c(ovhd_clause, ramp_clause)

  intro <- if (length(clauses) > 1L) {
    paste0(
      "To recover by month ", gs$target_month, ", either change alone would ",
      "do it: you'd need to "
    )
  } else {
    paste0(
      "To recover by month ", gs$target_month, " on this lever alone, ",
      "you'd need to "
    )
  }

  paste0(
    intro,
    paste(clauses, collapse = " -- or, separately, "),
    "."
  )
}

#' Interpret Capital Requirements
#'
#' Generates narrative text describing startup capital and personal runway
#' requirements, framed around what financing decision they imply.
#'
#' @param startup_costs A list as returned by [calc_startup_costs()].
#' @param personal_runway A list as returned by [calc_personal_runway()].
#'
#' @return A character string of narrative text, with paragraphs separated
#'   by `"\n\n"`.
#'
#' @export
interpret_capital <- function(startup_costs, personal_runway) {
  line_items <- startup_costs$line_items
  top_items <- sort(unlist(line_items), decreasing = TRUE)
  top_n <- utils::head(top_items, 2L)
  top_items_str <- paste(
    paste0(
      vapply(names(top_n), .humanize_label, character(1)),
      " (",
      vapply(top_n, .fmt_dollar, character(1)),
      ")"
    ),
    collapse = " and "
  )

  startup_sentence <- paste0(
    "Startup costs total ",
    .fmt_dollar(startup_costs$total),
    ", led by ",
    top_items_str,
    "."
  )

  runway_sentence <- paste0(
    "A personal runway of ",
    personal_runway$months_coverage,
    " months at ",
    .fmt_dollar(personal_runway$monthly_expenses),
    "/month in living expenses requires a reserve of ",
    .fmt_dollar(personal_runway$total),
    "."
  )

  combined_total <- startup_costs$total + personal_runway$total
  financing_sentence <- paste0(
    "Combined, you'll need to secure at least ",
    .fmt_dollar(combined_total),
    " before launch -- whether from savings, a loan, or a combination of ",
    "both -- to cover one-time startup costs and your personal living ",
    "expenses through the ramp-up period."
  )

  paste(startup_sentence, runway_sentence, financing_sentence, sep = "\n\n")
}
