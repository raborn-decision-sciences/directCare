#' Run a directCarePlanR call, surfacing its conditions as notifications
#'
#' directCarePlanR's error messages are already written as direct,
#' user-facing text (a deliberate package design principle), so this
#' dispatcher is simpler than it would otherwise need to be: any error
#' classed `dcPlanR_*` is shown as-is. The one informational condition
#' (`dcPlanR_zip_multi_county`, raised via `rlang::inform()` when a ZIP
#' spans multiple counties) is shown as a message notification rather than
#' an error.
#'
#' @param expr Expression to evaluate, typically a call into directCarePlanR.
#'
#' @return The value of `expr`, or `NULL` if an error was caught.
#'
#' @noRd
run_plan <- function(expr) {
  tryCatch(
    withCallingHandlers(
      expr,
      message = function(m) {
        if (inherits(m, "dcPlanR_zip_multi_county")) {
          showNotification(conditionMessage(m), type = "message", duration = 8)
        }
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) {
      if (any(grepl("^dcPlanR_", class(e)))) {
        showNotification(conditionMessage(e), type = "error", duration = 10)
      } else {
        showNotification("An unexpected error occurred.", type = "error", duration = 10)
      }
      NULL
    }
  )
}
