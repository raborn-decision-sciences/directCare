# -- Guided walkthroughs (cicerone/driver.js) -------------------------------
#
# Three interactive, step-by-step tours -- one per Upload-tab workflow card
# (Use Real Data, Plan My Practice, Quick Calculator) -- launched from links
# added to the existing (?) help modal (see app_server.R). Each tour walks
# the user through actually performing the workflow to completion: steps
# that correspond to a real action (click a button, choose a file) hide the
# tour's own Next button and instead wait for that real Shiny input event
# before advancing (wired in app_server.R via `.tour_advance()`); steps that
# are purely informational leave the Next button so the user can read and
# move on at their own pace. This is deliberately NOT a passive tooltip
# tour -- the user does the workflow, the tour narrates it.
#
# Each tour is built from several small "chapter" guides rather than one
# long one, and a chapter always contains only steps whose elements coexist
# in the DOM at the same time. This works around a real cicerone/driver.js
# behavior: `Cicerone$init()` resolves every step's element via
# querySelector() immediately (`defineSteps()`), and any step whose element
# doesn't exist at that moment is silently *dropped* from the guide's
# internal array -- not just skipped -- which *compacts* the array. Since
# `.tour_advance()` (app_server.R) has to re-run `$init()` before jumping to
# a later step (this app's tabs render lazily, so later elements don't
# exist until earlier real actions have happened), an element that used to
# exist but has since been *replaced* -- e.g. mod_edit.R's Quick Estimator
# form is swapped out entirely for a "Scenario Ready" summary once
# "Generate" is clicked -- gets dropped too, shifting every subsequent
# step's array index out from under a hardcoded step number (confirmed
# empirically: `$start(step = 8)` intending "the 8th step I wrote in R"
# instead hit whatever the 8th *surviving* element happened to compact down
# to, throwing driver.js's own "Invalid element to highlight" warning).
# Scoping each chapter to a single DOM-stable stretch, and always starting
# a fresh chapter at its own step 1, sidesteps the compaction problem
# entirely: within one chapter nothing gets removed out from under it.
#
# Historical Data and Plan My Practice share their last two chapters (both
# reach Projections and finish the same way) -- see the `active_tour`
# dispatch in app_server.R, which routes each shared trigger to whichever
# tour is currently active.

# -- Demo-data-derived seed values for the Plan My Practice tour ------------
# Computed once from inst/extdata/demo-gnucash.csv (Jan-Mar 2026, the only
# months with non-zero activity in the trimmed demo fixture -- see
# utils_demo.R), aggregated into the Quick Estimator's 6 fixed overhead
# buckets and a single illustrative membership tier. Used to pre-fill the
# Quick Estimator form when the Plan My Practice tour starts, so the walk-
# through demonstrates real, defensible numbers instead of arbitrary
# placeholders -- exactly what a user could expect from a real, if modest,
# DPC practice. Derivation (average monthly $, Jan-Mar 2026):
#   - Rent & Utilities  -> "rent" category avg                     ~$650
#   - Staff & Payroll   -> $0 (the demo practice is modeled as a
#                          solo provider with no separate payroll line)
#   - EHR & Software    -> "software" category avg (EMR, Bamboo
#                          Health, Blaze, Spruce, Adobe)            ~$580
#   - Malpractice       -> "insurance" category avg                ~$850
#   - Supplies & Labs   -> "supplies" + "labs" categories avg       ~$360
#   - Other             -> "marketing" + "equipment" + "education"
#                          + "licenses" + "other" categories avg   ~$2,100
#     (the demo practice's real bookkeeping has far more granular
#     categories than the Quick Estimator's 6 buckets, so several
#     smaller categories -- launch marketing, one-time equipment
#     purchases, provider CE -- necessarily land in "Other" here)
# Total (~$4,540/mo) lines up with the demo's actual observed overhead
# (Jan/Feb/Mar 2026: $4,492 / $3,590 / $5,513, avg ~$4,531).
#
# The membership tier (6 members @ $150/mo, +5/mo growth) is likewise
# reverse-derived from the demo's actual Membership Fees revenue
# ($850 / $1,530 / $2,210 across Jan-Mar) assuming a $150/mo fee: implied
# panel sizes of ~6 / ~10 / ~15 members, i.e. growth of ~4-5/mo.
# Fee-for-service is a simplified illustrative split of the demo's actual
# Fee-for-Service revenue (~$215-$505/mo): 1 new-patient visit/mo at $150
# plus 2 follow-ups/mo at $75.
.tour_demo_overhead <- list(
  rent = 650,
  payroll = 0,
  ehr = 580,
  malpractice = 850,
  supplies = 360,
  other = 2100
)

.tour_demo_tier <- list(
  label = "Standard Membership",
  members = 6,
  fee = 150,
  growth = 5
)

.tour_demo_ffs <- list(
  new_visit_fee = 150,
  new_patients_mo = 1,
  followup_fee = 75,
  followups_mo = 2,
  other_income = 0
)

# Single-total equivalent, used to pre-fill the (simpler, single-field)
# Quick Calculator tour -- sum of .tour_demo_overhead.
.tour_demo_calc_overhead <- 4540

# -- Small helper: a fresh Cicerone guide with one shared default ----------
# animate stays at its cicerone default (TRUE) -- disabling it looked like
# the clean fix for the cross-chapter popover-text race documented on
# app_server.R's tour_element_ready observer, but driver.js's own bundled
# source (driver.min.js) shows animate = FALSE takes an *early-return* path
# that detaches the popover's DOM node from its parent right after
# creating it, apparently expecting something else to re-append it later.
# Confirmed empirically to make things worse, not better: with animate =
# FALSE, in-chapter Next clicks (no cross-chapter reset() involved at all,
# e.g. Quick Calculator's c2 chapter) got stuck outright, not just showing
# stale text. Left at the library default; see app_server.R for how the
# cross-chapter race is actually handled instead.
.tour_guide <- function(id) {
  cicerone::Cicerone$new(id = id, done_btn_text = "Finish")
}

# ============================================================================
# Tour 1: Historical Data (real/uploaded transactions)
# Upload -> Review & Edit -> Summary -> Projections -> Download, using a real
# GnuCash export (the bundled demo CSV is offered as a stand-in for anyone
# following along without their own file handy yet).
# ============================================================================

#' @noRd
.build_tour_h1 <- function() {
  # Upload tab, initial path-selection cards.
  guide <- .tour_guide("tour_h1")
  guide$step(
    el = "upload-btn_use_real",
    title = "Use Real Data",
    description = paste0(
      "Click <strong>Use Bookkeeping Data</strong> to upload transactions ",
      "from your accounting software."
    ),
    position = "top",
    show_btns = FALSE
  )
  guide
}

#' @noRd
.build_tour_h2 <- function() {
  # Upload tab, upload-path form (file input + Upload & Process).
  guide <- .tour_guide("tour_h2")
  guide$step(
    el = "upload-csv_file",
    title = "Choose a file",
    description = paste0(
      "Select a GnuCash export (.gnucash or .csv). Don't have one handy? ",
      "<a href='demo-data/demo-gnucash.csv' download>",
      "Download our sample GnuCash CSV</a> and select that instead -- ",
      "it's the same data behind the app's demo mode."
    ),
    position = "right",
    show_btns = FALSE
  )
  guide$step(
    el = "upload-btn_upload",
    title = "Upload & process",
    description = "Click here to parse the file and load it into the app.",
    position = "top",
    show_btns = FALSE
  )
  guide
}

#' @noRd
.build_tour_h3 <- function() {
  # Upload tab, post-upload results (mapping table + Next).
  guide <- .tour_guide("tour_h3")
  guide$step(
    el = "upload-mapping_table",
    title = "Account mapping",
    description = paste0(
      "Each GnuCash account was automatically matched to an expense ",
      "category. Anything the app couldn't confidently match is tagged ",
      "“other” -- you'll get a chance to fix that on the next tab."
    ),
    position = "top"
  )
  guide$step(
    el = "upload-btn_next_to_edit",
    title = "Continue",
    description = "Click here to move on to Review & Edit.",
    position = "top",
    show_btns = FALSE
  )
  guide
}

#' @noRd
.build_tour_h4 <- function() {
  # Review & Edit tab, CSV editing view.
  guide <- .tour_guide("tour_h4")
  guide$step(
    el = "edit-edit_table",
    title = "Review categories",
    description = paste0(
      "Click a row's Category cell to correct it, then ",
      "<strong>Apply Category Edits</strong>. Changes here flow straight ",
      "into the Summary and Projections tabs."
    ),
    position = "right"
  )
  guide$step(
    el = "edit-remap_card",
    title = "Bulk re-map (optional)",
    description = paste0(
      "If a whole account was mis-mapped, re-assign every transaction for ",
      "that account to a new category in one click here, instead of editing ",
      "rows one by one."
    ),
    position = "left"
  )
  guide$step(
    el = "edit-add_txn_card",
    title = "Add transactions by hand (optional)",
    description = paste0(
      "Missing something from your export? Add a single overhead or income ",
      "row here, or enter a whole month/week's totals at once with ",
      "<strong>Add period summary</strong>."
    ),
    position = "left"
  )
  guide$step(
    el = "edit-btn_next_to_summary",
    title = "Continue",
    description = "Once your categories look right, click here for Summary.",
    position = "top",
    show_btns = FALSE
  )
  guide
}

#' @noRd
.build_tour_h5 <- function() {
  # Summary tab.
  guide <- .tour_guide("tour_h5")
  guide$step(
    el = "summary-ovhd_plot",
    title = "Overhead trend",
    description = paste0(
      "Your monthly overhead over time, broken down by category. Toggle ",
      "between a trend line and a category breakdown above the chart."
    ),
    position = "bottom"
  )
  guide$step(
    el = "summary-inc_plot",
    title = "Revenue trend",
    description = "The same view for revenue, broken down by income source.",
    position = "bottom"
  )
  guide$step(
    el = "summary-inc_chart_type",
    title = "Bar or pie, full data or by period",
    description = paste0(
      "Both cards have matching controls: switch between a bar chart and a ",
      "pie chart, and (in pie view) between the full data range and a single ",
      "period. Toggle <strong>By category</strong> / ",
      "<strong>By source</strong> to see one line item at a time instead of ",
      "the combined trend."
    ),
    position = "bottom"
  )
  guide$step(
    el = "summary-btn_next_to_projections",
    title = "Continue",
    description = "Click here to move on to Projections.",
    position = "top",
    show_btns = FALSE
  )
  guide
}

# ============================================================================
# Tour 2: Plan My Practice (synthetic history / Quick Estimator)
# Upload -> Quick Estimator (pre-filled with demo-derived example values) ->
# Projections -> Download. Shares its last two chapters with Tour 1.
# ============================================================================

#' @noRd
.build_tour_p1 <- function() {
  # Upload tab, initial path-selection cards.
  guide <- .tour_guide("tour_p1")
  guide$step(
    el = "upload-btn_use_plan",
    title = "Plan My Practice",
    description = paste0(
      "Click <strong>Start Planning</strong> to build a scenario without ",
      "any bookkeeping export -- ideal if you're still in the planning stage."
    ),
    position = "top",
    show_btns = FALSE
  )
  guide
}

#' @noRd
.build_tour_p2 <- function() {
  # Review & Edit tab, Quick Estimator form (pre-filled).
  guide <- .tour_guide("tour_p2")
  guide$step(
    el = "edit-est_rent",
    title = "Monthly overhead",
    description = paste0(
      "We've filled in the itemized overhead fields using average monthly ",
      "figures from our own demo direct-care practice (about $4,540/mo ",
      "total -- Payroll is $0 there since it's modeled as a solo provider). ",
      "Adjust any of these to match your own numbers."
    ),
    position = "right"
  )
  guide$step(
    el = "edit-est_tier_label_1",
    title = "Membership tier",
    description = paste0(
      "We've also suggested a starting membership tier based on the demo ",
      "practice's early panel growth: 6 members at $150/mo, adding about ",
      "5 new members a month. Click <strong>Add Tier</strong> for ",
      "additional plan levels, or just adjust this one."
    ),
    position = "right"
  )
  guide$step(
    el = "edit-est_new_visit_fee",
    title = "Fee-for-service (optional)",
    description = paste0(
      "A modest fee-for-service estimate, also based on the demo practice. ",
      "Leave any of these at $0 if they don't apply to you."
    ),
    position = "right"
  )
  guide$step(
    el = "edit-est_n_months",
    title = "Synthetic history length",
    description = paste0(
      "Choose how many months of synthetic history to generate -- ",
      "12 is a solid default. More months give the forecast models more ",
      "signal, producing narrower confidence intervals."
    ),
    position = "top"
  )
  guide$step(
    el = "edit-btn_generate",
    title = "Generate your scenario",
    description = paste0(
      "Click here to build a synthetic financial history from these ",
      "estimates -- the forecast models will treat it just like real data."
    ),
    position = "top",
    show_btns = FALSE
  )
  guide
}

#' @noRd
.build_tour_p3 <- function() {
  # Review & Edit tab, "Scenario Ready" summary (form is gone, replaced).
  guide <- .tour_guide("tour_p3")
  guide$step(
    el = "edit-btn_go_projections",
    title = "Continue",
    description = paste0(
      "Review the Start vs. End comparison above, then click here to move ",
      "on to Projections. Use <strong>Revise Estimates</strong> any time to ",
      "regenerate with different numbers."
    ),
    position = "top",
    show_btns = FALSE
  )
  guide
}

# ============================================================================
# Shared tail: both Historical Data and Plan My Practice reach Projections
# and finish the same way.
# ============================================================================

#' @noRd
.build_tour_proj1 <- function() {
  # Projections tab, before running the forecast.
  #
  # Targets "projections-method" (the actual radioButtons() input rendered
  # inside method_selector's renderUI), not "projections-method_selector"
  # itself -- that's the uiOutput() wrapper div, which Shiny styles with
  # `display: contents` so it never interferes with flex/grid layouts. Per
  # the CSS spec, a `display: contents` element permanently reports a
  # zero-size getBoundingClientRect() no matter how long you wait -- it
  # never generates its own box at all. driver.js's own canHighlight()
  # check silently (no console warning) refuses to highlight a zero-size
  # element, which is what made this look like a step-transition timing bug
  # until direct inspection of the wrapper's `display` value gave it away.
  guide <- .tour_guide("tour_proj1")
  guide$step(
    el = "projections-method",
    title = "Choose a forecast method",
    description = paste0(
      "Linear is the simplest and most transparent. ETS and ARIMA can ",
      "capture trend and seasonality better with more history."
    ),
    position = "right"
  )
  # Each of the next four steps targets a real input/button, not the
  # uiOutput() wrapper div around it -- see the note above .build_tour_proj1
  # about zero-size `display: contents` wrappers silently failing to
  # highlight.
  guide$step(
    el = "projections-income_growth",
    title = "Optional: assume income growth",
    description = paste0(
      "Apply a yearly growth rate on top of the fitted trend if you expect ",
      "revenue to grow (or shrink) faster than your history alone suggests. ",
      "Leave at 0% to project the trend unchanged."
    ),
    position = "right"
  )
  guide$step(
    el = "projections-overhead_model_wrap",
    title = "Optional: project overhead differently",
    description = paste0(
      "By default overhead follows its own fitted trend. Switch this to a ",
      "flat historical/recent average -- or a custom monthly value -- if you ",
      "expect overhead to hold steady instead of continuing its trend."
    ),
    position = "right"
  )
  guide$step(
    el = "projections-proj_btn_add_tier",
    title = "Optional: membership tiers",
    description = paste0(
      "Enter your membership tiers here to unlock member-count projections ",
      "alongside the financial forecast. Skip this if member counts aren't ",
      "relevant to what you're projecting."
    ),
    position = "right"
  )
  guide$step(
    el = "projections-btn_add_overhead_event",
    title = "Optional: planned overhead changes",
    description = paste0(
      "Add a one-time planned cost increase -- hiring staff, a new office -- ",
      "that kicks in at a specific point in the forecast. Use ",
      "<strong>Add Fee Change</strong> just below for planned membership fee ",
      "changes the same way."
    ),
    position = "right"
  )
  guide$step(
    el = "projections-btn_run",
    title = "Run the forecast",
    description = "Click here to generate break-even, revenue, and target projections.",
    position = "top",
    show_btns = FALSE
  )
  guide
}

#' @noRd
.build_tour_proj2 <- function() {
  # Projections tab, after running the forecast.
  guide <- .tour_guide("tour_proj2")
  guide$step(
    el = "projections-forecast_type",
    title = "Your results",
    description = paste0(
      "Break-even, Revenue Forecast, and Income Target are shown as ",
      "separate tabs here -- each with its own chart and plain-language ",
      "interpretation below it."
    ),
    position = "bottom"
  )
  guide$step(
    el = "projections-dl_report",
    title = "Download your report",
    description = paste0(
      "Click here for a branded PDF summarizing everything you just built ",
      "-- ready to save or share. That's the full workflow!"
    ),
    position = "top"
  )
  guide
}

# ============================================================================
# Tour 3: Quick Calculator
# Entirely self-contained on the Upload tab -- no cross-tab navigation, no
# data persisted into the shared `r` state, and nothing removed from the DOM
# mid-flow, so this tour needs only two chapters.
# ============================================================================

#' @noRd
.build_tour_c1 <- function() {
  # Upload tab, initial path-selection cards.
  guide <- .tour_guide("tour_c1")
  guide$step(
    el = "upload-btn_use_calculator",
    title = "Quick Calculator",
    description = paste0(
      "Click <strong>Open Calculator</strong> for instant break-even and ",
      "income-target math -- no upload or forecast model needed."
    ),
    position = "top",
    show_btns = FALSE
  )
  guide
}

#' @noRd
.build_tour_c2 <- function() {
  # Upload tab, calculator form (pre-filled) -- persists as one piece
  # throughout, so all remaining steps fit in a single chapter.
  guide <- .tour_guide("tour_c2")
  guide$step(
    el = "upload-calculator-monthly_overhead",
    title = "Example values, filled in for you",
    description = paste0(
      "We've suggested example numbers based on our demo practice: about ",
      "$4,540/mo in total overhead here, and a 6-member panel at $150/mo ",
      "below. Change anything to match your own situation -- results ",
      "update instantly. Turn on <strong>Multiple sources</strong> to ",
      "break overhead into individual line items."
    ),
    position = "right"
  )
  guide$step(
    el = "upload-calculator-tier_label_1",
    title = "Membership panel",
    description = paste0(
      "Add tiers for different age groups or plan levels with ",
      "<strong>Add Tier</strong>."
    ),
    position = "right"
  )
  guide$step(
    el = "upload-calculator-target_income",
    title = "Set an income target",
    description = paste0(
      "Enter the monthly take-home income you want after overhead -- the ",
      "results panel will show what it takes to reach it."
    ),
    position = "right"
  )
  guide$step(
    el = "upload-calculator-results_box",
    title = "Your results",
    description = paste0(
      "Break-even and income-target scenarios update instantly as you ",
      "type -- no data is saved anywhere. Click <strong>Back</strong> when ",
      "you're done. That's the full Quick Calculator workflow!"
    ),
    position = "left"
  )
  guide
}
