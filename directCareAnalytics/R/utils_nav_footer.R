# -- Shared tab-navigation footer (Back/Next + step indicator) -------------
#
# Each tab module renders its own copy of this footer -- there is no shared
# global dispatch component; every module keeps owning its own Back/Next
# `actionButton`s and the `observeEvent()`s that respond to them, exactly as
# before. This helper only standardizes the *visual* wrapper (a sticky
# footer bar) and adds the read-only step indicator alongside it. Icons and
# labels mirror the hidden navbar tabs defined in app_ui.R (`bs_icon`
# "upload"/"pencil-square"/"bar-chart-line"/"graph-up-arrow"), so the
# indicator and the tabs it represents always agree visually.
#
# `current_step` is a plain integer (1-4), not derived from any reactive
# lookup of "which tab is active" -- each module already knows its own
# position in the fixed Upload -> Edit -> Summary -> Projections sequence,
# so there's no new navigation-dispatch logic here, just a static per-module
# constant. A step is "complete" if its index is before `current_step`, kept
# deliberately index-based (not real visit-tracking) since progression
# through these tabs is already strictly sequential -- matches the decided
# design: read-only, not clickable, no new state to keep in sync.

.tour_nav_steps <- list(
  list(label = "Upload", icon = "upload"),
  list(label = "Review & Edit", icon = "pencil-square"),
  list(label = "Summary", icon = "bar-chart-line"),
  list(label = "Projections", icon = "graph-up-arrow")
)

#' Sticky Back/Next footer with a read-only step indicator
#'
#' @param current_step Integer 1-4, this tab's position in the Upload ->
#'   Edit -> Summary -> Projections sequence.
#' @param back An `actionButton()` (or `NULL`) for the backward action.
#' @param forward An `actionButton()` (or `NULL`) for the forward action.
#' @noRd
.tour_nav_footer <- function(current_step, back = NULL, forward = NULL) {
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
          bs_icon(step$icon, title = step$label),
          tags$span(class = "tour-nav-step-label d-none d-lg-inline", step$label)
        )
      })
    ),
    tags$div(
      class = "tour-nav-footer-buttons",
      back,
      forward
    )
  )
}
