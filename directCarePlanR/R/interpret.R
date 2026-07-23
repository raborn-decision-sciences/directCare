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
      "This plan generates ",
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
#'
#' @return A character string of narrative text, with paragraphs separated
#'   by `"\n\n"`.
#'
#' @export
interpret_projection <- function(projection) {
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

  paste(base_sentence, scenario_sentence, spread_sentence, sep = "\n\n")
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
