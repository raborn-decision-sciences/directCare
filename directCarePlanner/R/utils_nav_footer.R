# -- Shared tab-navigation footer (Back/Next + step indicator) -------------
#
# Ported from directCareAnalytics's identical helper (R/utils_nav_footer.R
# there), adapted to Planner's 2-step Plan Inputs -> Results sequence. Each
# tab module still owns its own Back/Next/Download `actionButton()`s and the
# `observeEvent()`s that respond to them -- this helper only standardizes
# the *visual* wrapper (a sticky nav bar) and adds the read-only step
# indicator alongside it. Icons and labels mirror app_ui.R's nav_panel()
# icons ("clipboard-data"/"graph-up-arrow"), so the indicator and the tabs
# it represents always agree visually.

.tour_nav_steps <- list(
  list(label = "Plan Inputs", icon = "clipboard-data"),
  list(label = "Results", icon = "graph-up-arrow")
)

#' Sticky Back/Next footer with a read-only step indicator
#'
#' @param current_step Integer 1-2, this tab's position in the Plan Inputs
#'   -> Results sequence.
#' @param back An `actionButton()` (or `NULL`) for the backward action.
#' @param forward An `actionButton()`/`downloadButton()` (or `NULL`) for the
#'   forward/primary action.
#' @param extra Additional content (e.g. `mod_scenario_slots_ui()`'s Save/
#'   Load buttons) rendered between the step indicator and Back/Next --
#'   its own visually distinct group, not mixed into the Back/Next
#'   button group itself.
#' @noRd
.tour_nav_footer <- function(current_step, back = NULL, forward = NULL, extra = NULL) {
  tags$div(
    class = "tour-nav-footer",
    tags$div(
      class = "tour-nav-footer-steps",
      lapply(seq_along(.tour_nav_steps), function(i) {
        step <- .tour_nav_steps[[i]]
        state <- if (i < current_step) {
          "complete"
        } else if (i == current_step) {
          "current"
        } else {
          "upcoming"
        }
        tags$span(
          class = paste0("tour-nav-step tour-nav-step-", state),
          bsicons::bs_icon(step$icon, title = step$label),
          tags$span(class = "tour-nav-step-label d-none d-lg-inline", step$label)
        )
      })
    ),
    if (!is.null(extra)) {
      tags$div(class = "tour-nav-footer-extra", extra)
    },
    tags$div(
      class = "tour-nav-footer-buttons",
      back,
      forward
    )
  )
}
