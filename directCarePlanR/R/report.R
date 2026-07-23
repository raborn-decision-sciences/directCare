# Per-table row cap in the assembled report data, mirroring
# directCareAnalytics's report tables -- keeps an unusually long horizon
# from producing an unbounded document.
.report_max_rows <- 24L

#' Split Narrative Text into a Paragraph Array
#'
#' `interpret_*()` functions return plain text with `"\n\n"` paragraph
#' breaks; Typst can't treat a raw `\n\n` inside a JSON string value as a
#' paragraph break, so the report template needs an array of paragraphs
#' instead.
#'
#' @param text Character string, or `NULL`.
#'
#' @noRd
.split_paragraphs <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(character(0))
  }
  paragraphs <- strsplit(text, "\n\n", fixed = TRUE)[[1]]
  paragraphs <- trimws(paragraphs)
  paragraphs[nzchar(paragraphs)]
}

#' Convert a Data Frame to a List of Row Lists
#'
#' @param df Data frame or tibble.
#' @param max_rows Integer maximum number of rows to include.
#'
#' @noRd
.rows_list <- function(df, max_rows = .report_max_rows) {
  n <- min(nrow(df), max_rows)
  lapply(seq_len(n), function(i) as.list(df[i, ]))
}

#' Assemble Report Data for the Typst Template
#'
#' Builds the `data.json` object consumed by the Planner's Typst report
#' template, from the market context, revenue, projection, capital, and
#' interpretation objects produced elsewhere in the package. This schema is
#' the shared contract between the R layer and the `.typ` template, so it
#' should stay a plain, JSON-serializable list.
#'
#' @param market_context A list as returned by [build_market_context()].
#' @param revenue A `dcPlanR_revenue` object, as returned by
#'   [calc_mixed_revenue()].
#' @param projections A tibble as returned by [project_scenarios()].
#' @param capital A list with `startup_costs` and `personal_runway`
#'   elements, as returned by [calc_startup_costs()] and
#'   [calc_personal_runway()].
#' @param interpretations A named list of narrative strings produced by the
#'   `interpret_*()` functions (e.g. `list(revenue = ..., projection = ..., capital = ...)`).
#' @param practice_name Optional character string. Defaults to a name
#'   derived from `market_context`'s geography when supplied, else
#'   `"Untitled Practice"`.
#'
#' @return A plain list, ready to be serialized to JSON with
#'   [jsonlite::write_json()]. Each of `market`, `revenue`, `projections`,
#'   `capital`, and `interpretations` is `NULL` when its corresponding
#'   input is `NULL`, so the template can render only the sections that
#'   were supplied.
#'
#' @export
build_report_data <- function(
  market_context = NULL,
  revenue = NULL,
  projections = NULL,
  capital = NULL,
  interpretations = NULL,
  practice_name = NULL
) {
  if (is.null(practice_name)) {
    practice_name <- if (!is.null(market_context)) {
      paste0(
        market_context$geography$county_name,
        ", ",
        market_context$geography$state_abb,
        " Practice"
      )
    } else {
      "Untitled Practice"
    }
  }

  market_block <- if (!is.null(market_context)) {
    list(
      county_name = market_context$geography$county_name,
      state_abb = market_context$geography$state_abb,
      metro_fips = market_context$geography$metro_fips,
      population = market_context$population_income$population,
      median_household_income = market_context$population_income$median_household_income,
      uninsured_rate_pct = round(market_context$uninsured$uninsured_rate * 100, 1),
      physician_density_per_10k = round(market_context$physician_density$physician_density_per_10k, 1),
      landscape_count = nrow(market_context$landscape)
    )
  }

  revenue_block <- if (!is.null(revenue)) {
    list(
      membership_final = if (!is.null(revenue$membership)) {
        utils::tail(revenue$membership$revenue, 1)
      },
      fee_final = if (!is.null(revenue$fee_for_service)) {
        utils::tail(revenue$fee_for_service$revenue, 1)
      },
      total_final = utils::tail(revenue$total$total_revenue, 1),
      table = .rows_list(revenue$total)
    )
  }

  projections_block <- if (!is.null(projections)) {
    scenario_names <- intersect(c("conservative", "base", "optimistic"), unique(projections$scenario))
    list(
      scenarios = lapply(scenario_names, function(scenario_name) {
        scenario_data <- projections[projections$scenario == scenario_name, ]
        list(
          scenario = scenario_name,
          table = .rows_list(scenario_data[setdiff(names(scenario_data), "scenario")])
        )
      })
    )
  }

  capital_block <- if (!is.null(capital)) {
    list(
      startup_total = capital$startup_costs$total,
      startup_line_items = as.list(capital$startup_costs$line_items),
      runway_total = capital$personal_runway$total,
      monthly_expenses = capital$personal_runway$monthly_expenses,
      months_coverage = capital$personal_runway$months_coverage,
      combined_total = capital$startup_costs$total + capital$personal_runway$total
    )
  }

  interpretations_block <- if (!is.null(interpretations)) {
    lapply(interpretations, .split_paragraphs)
  }

  list(
    report_date = format(Sys.Date(), "%B %d, %Y"),
    practice_name = practice_name,
    market = market_block,
    revenue = revenue_block,
    projections = projections_block,
    capital = capital_block,
    interpretations = interpretations_block
  )
}

#' Render the Practice Launch Plan Report
#'
#' Writes `data` to a JSON sidecar file, copies the Planner's Typst
#' template alongside it, and compiles the report to a PDF via the `typst`
#' CLI (or the `typr` R package as a fallback), following the same
#' pipeline pattern used by `directCareAnalytics`.
#'
#' @param data A list as returned by [build_report_data()].
#' @param out_file Destination PDF file path.
#'
#' @return Invisibly, `out_file`.
#'
#' @export
render_plan_report <- function(data, out_file) {
  tmp_dir <- tempfile("dcplanr_report_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  json_path <- file.path(tmp_dir, "data.json")
  jsonlite::write_json(data, json_path, auto_unbox = TRUE, null = "null", na = "null")

  template_dst <- file.path(tmp_dir, "report.typ")
  file.copy(
    system.file("report", "report.typ", package = "directCarePlanR"),
    template_dst,
    overwrite = TRUE
  )

  .compile_typst(template_dst)

  pdf_path <- file.path(tmp_dir, "report.pdf")
  if (!file.exists(pdf_path)) {
    rlang::abort(
      "Typst compilation did not produce a PDF.",
      class = "dcPlanR_typst_no_output"
    )
  }
  file.copy(pdf_path, out_file, overwrite = TRUE)
  invisible(out_file)
}

# Internal: invoke the typst CLI (PATH) or the typst R package.
.compile_typst <- function(typ_file) {
  bin <- Sys.which("typst")
  if (nzchar(bin)) {
    res <- system2(
      bin,
      args = c("compile", shQuote(typ_file)),
      stdout = TRUE,
      stderr = TRUE
    )
    status <- rlang::`%||%`(attr(res, "status"), 0L)
    if (!is.null(status) && identical(status, 1L)) {
      rlang::abort(
        paste0("Typst compile error:\n", paste(res, collapse = "\n")),
        class = "dcPlanR_typst_compile_error"
      )
    }
    return(invisible())
  }

  if (requireNamespace("typr", quietly = TRUE)) {
    typr::typr_compile(typ_file, output_format = "pdf")
    return(invisible())
  }

  rlang::abort(
    paste0(
      "Typst binary not found and the 'typr' R package is not installed.\n",
      "Install options:\n",
      "  macOS:  brew install typst\n",
      "  Cargo:  cargo install typst-cli\n",
      "  R pkg:  install.packages('typr')\n"
    ),
    class = "dcPlanR_typst_missing"
  )
}
