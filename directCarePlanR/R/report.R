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
#'   `interpret_*()` functions.
#'
#' @return A plain list, ready to be serialized to JSON with
#'   [jsonlite::write_json()].
#'
#' @export
build_report_data <- function(
  market_context = NULL,
  revenue = NULL,
  projections = NULL,
  capital = NULL,
  interpretations = NULL
) {
  rlang::abort(
    "build_report_data() is not yet implemented.",
    class = "dcPlanR_not_implemented"
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
  rlang::abort(
    "render_plan_report() is not yet implemented.",
    class = "dcPlanR_not_implemented"
  )
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
