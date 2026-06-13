# -- Report generation utilities -----------------------------------------------
# Helpers for assembling and rendering a Typst-based PDF report from the
# shared reactive state and forecast results produced by mod_projections.

# -- HTML \u2192 plain text ----------------------------------------------------------
# Strips HTML tags; converts common entities to UTF-8 equivalents.
html_to_plain <- function(html) {
  if (is.null(html) || !nzchar(html)) {
    return("")
  }
  out <- gsub("&mdash;", "\u2014", html)
  out <- gsub("&ndash;", "\u2013", out)
  out <- gsub("&nbsp;", " ", out)
  out <- gsub("&amp;", "&", out)
  out <- gsub("&lt;", "<", out)
  out <- gsub("&gt;", ">", out)
  out <- gsub("&hellip;", "\u2026", out)
  out <- gsub("<br\\s*/?>", "\n\n", out, ignore.case = TRUE, perl = TRUE)
  # </p> marks the end of a paragraph \u2192 double newline; opening <p> is discarded
  out <- gsub("</p>", "\n\n", out, ignore.case = TRUE)
  out <- gsub("<p[^>]*>", "", out, ignore.case = TRUE, perl = TRUE)
  out <- gsub("<[^>]+>", "", out, perl = TRUE)
  out <- gsub("[ \t]+", " ", out)
  out <- gsub("\n{3,}", "\n\n", out)
  trimws(out)
}

# Split HTML interpretation text into a character vector \u2014 one element per
# paragraph. This is necessary because Typst does not recognise \n\n inside a
# runtime string value as a paragraph break; the template iterates the array
# and emits explicit vertical spacing between elements.
html_to_paragraphs <- function(html) {
  plain <- html_to_plain(html)
  if (!nzchar(plain)) {
    return(character(0))
  }
  paras <- strsplit(plain, "\n\n", fixed = TRUE)[[1]]
  paras <- trimws(paras)
  paras[nzchar(paras)]
}

# -- Formatting helpers ---------------------------------------------------------
.fmt_dollar <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) {
    return("\u2014")
  }
  scales::dollar(x, accuracy = 0.01)
}

.period_label <- function(year = NULL, month = NULL, week_start = NULL) {
  if (!is.null(week_start) && !is.na(week_start)) {
    paste0("Wk ", format(as.Date(week_start), "%b %d, %Y"))
  } else if (!is.null(year) && !is.null(month)) {
    format(
      as.Date(paste(year, sprintf("%02d", month), "01", sep = "-")),
      "%b %Y"
    )
  } else {
    ""
  }
}

# -- Table builders -------------------------------------------------------------
.ovhd_table <- function(ovhd) {
  is_wkly <- "week_start" %in% names(ovhd)
  lapply(seq_len(nrow(ovhd)), function(i) {
    row <- ovhd[i, ]
    list(
      period = if (is_wkly) {
        .period_label(week_start = row$week_start)
      } else {
        .period_label(row$year, row$month)
      },
      gross_fmt = .fmt_dollar(row$gross_overhead),
      refunds_fmt = .fmt_dollar(row$total_refunds),
      net_fmt = .fmt_dollar(row$total_overhead)
    )
  })
}

.inc_table <- function(inc) {
  if (is.null(inc) || nrow(inc) == 0L) {
    return(NULL)
  }
  is_wkly <- "week_start" %in% names(inc)
  lapply(seq_len(nrow(inc)), function(i) {
    row <- inc[i, ]
    list(
      period = if (is_wkly) {
        .period_label(week_start = row$week_start)
      } else {
        .period_label(row$year, row$month)
      },
      revenue_fmt = .fmt_dollar(row$total_revenue)
    )
  })
}

.forecast_table_bkevn <- function(result, max_rows = 24L) {
  fd <- utils::head(result$forecast_data, max_rows)
  lapply(seq_len(nrow(fd)), function(i) {
    row <- fd[i, ]
    surplus <- if ("overhead_forecast" %in% names(row)) {
      row$revenue_forecast - row$overhead_forecast
    } else {
      NA_real_
    }
    list(
      period = format(row$period_start, "%b %Y"),
      revenue_fmt = .fmt_dollar(row$revenue_forecast),
      overhead_fmt = if ("overhead_forecast" %in% names(row)) {
        .fmt_dollar(row$overhead_forecast)
      } else {
        "\u2014"
      },
      surplus_fmt = .fmt_dollar(surplus),
      surplus_sign = if (!is.na(surplus) && surplus >= 0) "pos" else "neg"
    )
  })
}

.forecast_table_rev <- function(result, max_rows = 24L) {
  fd <- utils::head(result$forecast_data, max_rows)
  has_ci <- all(c("revenue_lower", "revenue_upper") %in% names(fd))
  lapply(seq_len(nrow(fd)), function(i) {
    row <- fd[i, ]
    list(
      period = format(row$period_start, "%b %Y"),
      revenue_fmt = .fmt_dollar(row$revenue_forecast),
      lo_fmt = if (has_ci) .fmt_dollar(row$revenue_lower) else "\u2014",
      hi_fmt = if (has_ci) .fmt_dollar(row$revenue_upper) else "\u2014"
    )
  })
}

.forecast_table_tgt <- function(result, max_rows = 24L) {
  fd <- utils::head(result$forecast_data, max_rows)
  has_req <- "required_revenue" %in% names(fd)
  lapply(seq_len(nrow(fd)), function(i) {
    row <- fd[i, ]
    gap <- if (has_req) {
      row$revenue_forecast - row$required_revenue
    } else {
      NA_real_
    }
    list(
      period = format(row$period_start, "%b %Y"),
      revenue_fmt = .fmt_dollar(row$revenue_forecast),
      req_fmt = if (has_req) .fmt_dollar(row$required_revenue) else "\u2014",
      gap_fmt = .fmt_dollar(gap),
      gap_sign = if (!is.na(gap) && gap >= 0) "pos" else "neg"
    )
  })
}

# -- build_report_data() --------------------------------------------------------
#' Assemble all report content into a serialisable list.
#'
#' @param r             Shared `reactiveValues` object.
#' @param inputs        Named list of current projections inputs (method, horizon,
#'                      confidence, target_income).
#' @param breakeven_res Adjusted break-even result (or NULL).
#' @param revenue_res   Adjusted revenue-forecast result (or NULL).
#' @param target_res    Adjusted income-target result (or NULL).
#' @param interpret_bkevn,interpret_rev,interpret_tgt HTML strings from
#'                      `interpret_*()` helpers (or NULL).
#' @noRd
build_report_data <- function(
  r,
  inputs,
  breakeven_res = NULL,
  revenue_res = NULL,
  target_res = NULL,
  interpret_bkevn = NULL,
  interpret_rev = NULL,
  interpret_tgt = NULL
) {
  is_weekly <- !is.null(r$overhead_monthly) &&
    "week_start" %in% names(r$overhead_monthly)

  # Detect workflow: scenario only when the Quick Estimator was used (scenario_inputs
  # is populated by mod_edit on btn_generate). Manual-entry also produces a 0-row
  # transactions tibble but leaves scenario_inputs NULL, so it falls into "upload".
  workflow <- if (!is.null(r$scenario_inputs)) "scenario" else "upload"

  has_income <- !is.null(r$income_monthly) && nrow(r$income_monthly) > 0L

  # -- Scenario inputs (stored by mod_edit on btn_generate) ------------------
  scenario_block <- if (workflow == "scenario" && !is.null(r$scenario_inputs)) {
    si <- r$scenario_inputs
    list(
      start_period = si$start_period,
      n_months = si$n_months,
      panel_size = si$panel_size,
      panel_size_fmt = as.character(si$panel_size %||% 0L),
      monthly_fee_fmt = .fmt_dollar(si$monthly_fee),
      monthly_growth = si$monthly_growth,
      overhead = list(
        rent_fmt = .fmt_dollar(si$overhead_rent),
        payroll_fmt = .fmt_dollar(si$overhead_payroll),
        ehr_fmt = .fmt_dollar(si$overhead_ehr),
        malpractice_fmt = .fmt_dollar(si$overhead_malpractice),
        supplies_fmt = .fmt_dollar(si$overhead_supplies),
        other_fmt = .fmt_dollar(si$overhead_other),
        total_fmt = .fmt_dollar(si$overhead_total)
      ),
      ffs = list(
        new_visit_fee_fmt = .fmt_dollar(si$ffs_new_visit_fee),
        new_patients_mo = si$ffs_new_patients_mo,
        followup_fee_fmt = .fmt_dollar(si$ffs_followup_fee),
        followups_mo = si$ffs_followups_mo,
        other_income_fmt = .fmt_dollar(si$ffs_other_income)
      )
    )
  } else {
    NULL
  }

  # -- Break-even block -------------------------------------------------------
  bkevn_block <- if (!is.null(breakeven_res)) {
    sustained <- breakeven_is_sustained(breakeven_res)
    already <- identical(breakeven_res$periods_to_breakeven, 0L)
    status <- if (already && isTRUE(sustained)) {
      "Achieved"
    } else if (already && isFALSE(sustained)) {
      "Achieved (at risk)"
    } else if (already) {
      "Achieved"
    } else if (is.na(breakeven_res$breakeven_date)) {
      "Not in horizon"
    } else {
      format(breakeven_res$breakeven_date, "%B %Y")
    }
    list(
      status = status,
      has_plot = TRUE,
      current_revenue_fmt = .fmt_dollar(breakeven_res$current_revenue),
      current_overhead_fmt = .fmt_dollar(
        breakeven_res$current_overhead_avg %||% breakeven_res$current_overhead
      ),
      current_surplus_fmt = .fmt_dollar(breakeven_res$current_surplus_deficit),
      surplus_sign = if (breakeven_res$current_surplus_deficit >= 0) {
        "pos"
      } else {
        "neg"
      },
      breakeven_date = if (!is.na(breakeven_res$breakeven_date)) {
        format(breakeven_res$breakeven_date, "%B %Y")
      } else {
        NULL
      },
      periods_to_breakeven = if (!is.na(breakeven_res$periods_to_breakeven)) {
        breakeven_res$periods_to_breakeven
      } else {
        NULL
      },
      interpretation = html_to_paragraphs(interpret_bkevn),
      table = .forecast_table_bkevn(breakeven_res)
    )
  } else {
    NULL
  }

  # -- Revenue block ----------------------------------------------------------
  rev_block <- if (!is.null(revenue_res)) {
    fd <- revenue_res$forecast_data
    pct_change <- round(
      (tail(fd$revenue_forecast, 1) - revenue_res$current_revenue) /
        abs(revenue_res$current_revenue) *
        100,
      1
    )
    list(
      has_plot = TRUE,
      current_revenue_fmt = .fmt_dollar(revenue_res$current_revenue),
      projected_end_fmt = .fmt_dollar(tail(fd$revenue_forecast, 1)),
      end_period = format(tail(fd$period_start, 1), "%B %Y"),
      pct_change = pct_change,
      pct_change_sign = if (pct_change >= 0) "pos" else "neg",
      interpretation = html_to_paragraphs(interpret_rev),
      table = .forecast_table_rev(revenue_res)
    )
  } else {
    NULL
  }

  # -- Income target block ----------------------------------------------------
  tgt_block <- if (!is.null(target_res)) {
    tgt_sustained <- target_is_sustained(target_res)
    tgt_already <- isTRUE(target_res$current_gap >= 0)
    tgt_status <- if (tgt_already && isFALSE(tgt_sustained)) {
      "Achieved (at risk)"
    } else if (tgt_already) {
      "Achieved"
    } else if (is.na(target_res$target_date)) {
      "Not in horizon"
    } else {
      format(target_res$target_date, "%B %Y")
    }
    list(
      has_plot = TRUE,
      target_income_fmt = .fmt_dollar(inputs$target_income),
      status = tgt_status,
      required_revenue_fmt = .fmt_dollar(target_res$required_revenue_now),
      current_gap_fmt = .fmt_dollar(target_res$current_gap),
      gap_sign = if (target_res$current_gap >= 0) "pos" else "neg",
      target_date = if (!is.na(target_res$target_date)) {
        format(target_res$target_date, "%B %Y")
      } else {
        NULL
      },
      interpretation = html_to_paragraphs(interpret_tgt),
      table = .forecast_table_tgt(target_res)
    )
  } else {
    NULL
  }

  # -- Assemble ---------------------------------------------------------------
  list(
    practice_name = r$practice_name %||% "",
    practice_id = r$practice_id %||% "",
    report_date = format(Sys.Date(), "%B %d, %Y"),
    frequency = if (is_weekly) "weekly" else "monthly",
    period_label = if (is_weekly) "Week" else "Month",
    workflow = workflow,
    has_income = has_income,
    panel_size_fmt = if (!is.null(r$panel_size)) {
      as.character(r$panel_size)
    } else {
      NULL
    },
    membership_fee_fmt = if (!is.null(r$membership_fee)) {
      .fmt_dollar(r$membership_fee)
    } else {
      NULL
    },
    forecast_method = inputs$method %||% NULL,
    forecast_horizon = inputs$horizon %||% NULL,
    ci_label = paste0(round((inputs$confidence %||% 0.95) * 100), "% CI"),
    overhead_summary = if (!is.null(r$overhead_monthly)) {
      .ovhd_table(r$overhead_monthly)
    } else {
      list()
    },
    income_summary = .inc_table(r$income_monthly),
    scenario = scenario_block,
    breakeven = bkevn_block,
    revenue = rev_block,
    target = tgt_block
  )
}

# -- render_report_pdf() --------------------------------------------------------
#' Render the Typst PDF report to `out_file`.
#'
#' Requires the `typst` binary to be on PATH (install via `brew install typst`
#' or `cargo install typst-cli`) or the `typst` R package.
#'
#' @param data_list      List produced by `build_report_data()`.
#' @param out_file       Destination file path (from `downloadHandler`).
#' @param breakeven_res  Adjusted break-even result from `adj_breakeven()`, or NULL.
#' @param revenue_res    Adjusted revenue result from `adj_revenue()`, or NULL.
#' @param target_res     Adjusted target result from `adj_target()`, or NULL.
#' @param income_monthly,overhead_monthly Raw period-summary tibbles from `r`.
#' @noRd
render_report_pdf <- function(
  data_list,
  out_file,
  breakeven_res = NULL,
  revenue_res = NULL,
  target_res = NULL,
  income_monthly = NULL,
  overhead_monthly = NULL
) {
  tmp_dir <- tempfile("dpc_report_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # -- Render forecast plots to PNG ------------------------------------------
  .save_plot <- function(p, name, w = 7, h = 3.8) {
    path <- file.path(tmp_dir, paste0(name, ".png"))
    ggplot2::ggsave(
      path,
      p +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = "white", colour = NA)
        ),
      width = w,
      height = h,
      dpi = 150,
      bg = "white"
    )
  }

  if (!is.null(breakeven_res)) {
    p <- plot_forecast_breakeven(
      breakeven_res,
      income_monthly = income_monthly,
      overhead_monthly = overhead_monthly
    )
    .save_plot(p, "breakeven")
  }
  if (!is.null(revenue_res)) {
    p <- plot_forecast_revenue(revenue_res, income_monthly = income_monthly)
    .save_plot(p, "revenue")
  }
  if (!is.null(target_res)) {
    p <- plot_forecast_target(target_res, income_monthly = income_monthly)
    .save_plot(p, "target")
  }

  # -- Write JSON sidecar next to the template --------------------------------
  json_path <- file.path(tmp_dir, "data.json")
  jsonlite::write_json(
    data_list,
    json_path,
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  )

  # Copy Typst template (Typst resolves imports relative to the .typ file)
  template_dst <- file.path(tmp_dir, "report.typ")
  file.copy(app_sys("report/report.typ"), template_dst, overwrite = TRUE)

  # -- Compile ----------------------------------------------------------------
  .compile_typst(template_dst)

  pdf_path <- file.path(tmp_dir, "report.pdf")
  if (!file.exists(pdf_path)) {
    stop("Typst compilation did not produce a PDF.")
  }
  file.copy(pdf_path, out_file, overwrite = TRUE)
  invisible(out_file)
}

# Internal: invoke the typst CLI (PATH) or the typst R package.
.compile_typst <- function(typ_file) {
  bin <- Sys.which("typst")
  if (nzchar(bin)) {
    # System binary (homebrew, cargo, etc.)
    res <- system2(
      bin,
      args = c("compile", shQuote(typ_file)),
      stdout = TRUE,
      stderr = TRUE
    )
    status <- attr(res, "status") %||% 0L
    if (!is.null(status) && identical(status, 1L)) {
      stop("Typst compile error:\n", paste(res, collapse = "\n"))
    }
    return(invisible())
  }

  # R package fallback
  if (requireNamespace("typst", quietly = TRUE)) {
    typst::typst_compile(typ_file)
    return(invisible())
  }

  stop(
    "Typst binary not found and the 'typst' R package is not installed.\n",
    "Install options:\n",
    "  macOS:  brew install typst\n",
    "  Cargo:  cargo install typst-cli\n",
    "  R pkg:  install.packages('typst')\n"
  )
}
