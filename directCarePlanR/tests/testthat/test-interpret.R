# -- interpret_revenue -----------------------------------------------------

test_that("interpret_revenue() reports composition and names the larger sensitivity lever", {
  revenue <- calc_mixed_revenue(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 3),
    fee_args = list(visit_volume = 100, new_visit_fee = 200, follow_up_fee = 100, ramp_months = 1),
    horizon_months = 3
  )
  text <- interpret_revenue(revenue)

  expect_type(text, "character")
  expect_length(text, 1L)
  expect_true(grepl("\\$42,000/month", text))
  expect_true(grepl("\\$30,000 \\(71%\\)", text))
  expect_true(grepl("\\$12,000 \\(29%\\)", text))
  # Membership shift ($3,000) > fee shift ($1,200), so membership fee
  # should be named the more sensitive lever.
  expect_true(grepl("membership fee is the most sensitive lever", text))
  expect_true(grepl("\\$3,000", text))
  expect_true(grepl("\\$1,200", text))
})

test_that("interpret_revenue() handles a membership-only plan", {
  revenue <- calc_mixed_revenue(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 3),
    horizon_months = 3
  )
  text <- interpret_revenue(revenue)

  expect_true(grepl("\\$30,000/month in membership revenue", text))
  expect_true(grepl("no fee-for-service component", text))
  expect_false(grepl("most sensitive lever", text))
})

test_that("interpret_revenue() handles a fee-for-service-only plan", {
  revenue <- calc_mixed_revenue(
    fee_args = list(visit_volume = 100, new_visit_fee = 200, follow_up_fee = 100, ramp_months = 1),
    horizon_months = 3
  )
  text <- interpret_revenue(revenue)

  expect_true(grepl("\\$12,000/month in fee-for-service revenue", text))
  expect_true(grepl("no membership component", text))
})

# -- interpret_projection ---------------------------------------------------

test_that("interpret_projection() reports the base recovery month and scenario spread", {
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 12),
    overhead_monthly = 5000
  )
  projection <- project_scenarios(assumptions, horizon_months = 24)
  text <- interpret_projection(projection)

  expect_true(grepl("base assumptions.*recovers.*month 3", text))
  expect_true(grepl("shifts to month 6", text))
  expect_true(grepl("reached by month 2", text))
  expect_true(grepl("4-month spread", text))
  expect_true(grepl("relatively robust", text))
})

test_that("interpret_projection() flags a wide scenario spread as highly sensitive", {
  assumptions <- list(
    membership_args = list(panel_size = 200, fee = 80, ramp_months = 12),
    overhead_monthly = 8000
  )
  projection <- project_scenarios(assumptions, horizon_months = 24)
  text <- interpret_projection(projection)

  expect_true(grepl("11-month spread", text))
  expect_true(grepl("highly sensitive", text))
})

test_that("interpret_projection() handles scenarios that never recover within the horizon", {
  assumptions <- list(
    membership_args = list(panel_size = 50, fee = 50, ramp_months = 12),
    overhead_monthly = 10000
  )
  projection <- project_scenarios(assumptions, horizon_months = 12)
  text <- interpret_projection(projection)

  expect_true(grepl("does not recover its ramp-period costs", text))
  expect_true(grepl("cannot be compared directly", text))
})

# -- decompose_projection_sensitivity ----------------------------------------

test_that("decompose_projection_sensitivity() isolates ramp vs. overhead and names the dominant lever", {
  assumptions <- list(
    membership_args = list(panel_size = 200, fee = 80, ramp_months = 12),
    overhead_monthly = 8000
  )
  decomp <- decompose_projection_sensitivity(assumptions, horizon_months = 24)

  expect_s3_class(decomp, "dcPlanR_sensitivity_decomposition")
  expect_identical(decomp$ramp_spread_months, 9L)
  expect_identical(decomp$overhead_spread_months, 3L)
  expect_identical(decomp$dominant_lever, "ramp")
})

test_that("decompose_projection_sensitivity() reports NA ramp spread for a fee-for-service-only plan", {
  assumptions <- list(
    fee_args = list(visit_volume = 100, new_visit_fee = 200, follow_up_fee = 100, ramp_months = 1),
    overhead_monthly = 8000
  )
  decomp <- decompose_projection_sensitivity(assumptions, horizon_months = 24)

  expect_true(is.na(decomp$ramp_spread_months))
  expect_false(is.na(decomp$overhead_spread_months))
  expect_identical(decomp$dominant_lever, "overhead")
})

test_that("decompose_projection_sensitivity() calls an exact tie inconclusive", {
  # `identical()`, not a magnitude comparison, decides ties, so an exact
  # equality forced via mockery of the two isolated computations is more
  # reliable here than hunting for real assumptions that happen to tie.
  local_mocked_bindings(
    project_practice = function(assumptions, horizon_months) {
      tibble::tibble(month = 1:horizon_months, cumulative_net_income = seq_len(horizon_months) - 5)
    }
  )
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 12),
    overhead_monthly = 5000
  )
  decomp <- decompose_projection_sensitivity(assumptions, horizon_months = 24)

  # With project_practice() stubbed identically regardless of which
  # assumption was varied, every isolated recovery month is the same
  # (month 6, where cumulative_net_income first hits 0), so both spreads
  # collapse to 0 -- an exact tie.
  expect_identical(decomp$ramp_spread_months, 0L)
  expect_identical(decomp$overhead_spread_months, 0L)
  expect_true(is.na(decomp$dominant_lever))
})

# -- interpret_projection() + sensitivity_decomposition ----------------------

test_that("interpret_projection() omits the decomposition paragraph when none is supplied", {
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 12),
    overhead_monthly = 5000
  )
  projection <- project_scenarios(assumptions, horizon_months = 24)

  text <- interpret_projection(projection)

  expect_false(grepl("larger driver", text))
  expect_false(grepl("only lever", text))
})

test_that("interpret_projection() appends a paragraph naming the dominant lever when a decomposition is supplied", {
  assumptions <- list(
    membership_args = list(panel_size = 200, fee = 80, ramp_months = 12),
    overhead_monthly = 8000
  )
  projection <- project_scenarios(assumptions, horizon_months = 24)
  decomp <- decompose_projection_sensitivity(assumptions, horizon_months = 24)

  text <- interpret_projection(projection, decomp)

  expect_true(grepl("membership ramp speed is the larger driver", text))
  expect_true(grepl("about 9 months", text))
  expect_true(grepl("about 3 months", text))
})

test_that("interpret_projection() singularizes a 1-month spread", {
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 12),
    overhead_monthly = 5000
  )
  projection <- project_scenarios(assumptions, horizon_months = 24)
  decomp <- decompose_projection_sensitivity(assumptions, horizon_months = 24)

  text <- interpret_projection(projection, decomp)

  expect_true(grepl("about 1 month for overhead alone", text))
  expect_false(grepl("1 months", text))
})

# -- goal_seek_projection_recovery -------------------------------------------

test_that("goal_seek_projection_recovery() reports both levers when both are achievable", {
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 24),
    overhead_monthly = 15000
  )
  gs <- goal_seek_projection_recovery(assumptions, horizon_months = 12)

  expect_s3_class(gs, "dcPlanR_goal_seek")
  expect_identical(gs$target_month, 12)
  expect_true(gs$overhead$achievable)
  expect_identical(gs$overhead$pct_cut, 46)
  expect_true(gs$ramp$achievable)
  expect_identical(gs$ramp$original_ramp_months, 24)
  expect_identical(gs$ramp$new_ramp_months, 13)
  expect_identical(gs$ramp$months_faster, 11)
  expect_true(gs$combined$achievable)
  # The combined blend is a smaller change on each lever than the solo
  # solve: a smaller overhead cut and a smaller ramp shortening, since it
  # only needs to move part-way toward each solo target, not all the way.
  expect_lt(gs$combined$pct_cut, gs$overhead$pct_cut)
  expect_gt(gs$combined$new_ramp_months, gs$ramp$new_ramp_months)
})

test_that("goal_seek_projection_recovery() reports overhead-only for a fee-for-service-only plan", {
  assumptions <- list(
    membership_args = list(panel_size = 50, fee = 50, ramp_months = 12),
    overhead_monthly = 10000
  )
  gs <- goal_seek_projection_recovery(assumptions, horizon_months = 12)

  expect_true(gs$overhead$achievable)
  expect_identical(gs$overhead$pct_cut, 86)
  expect_false(gs$ramp$achievable)
  expect_false(gs$combined$achievable)
})

test_that("goal_seek_projection_recovery() reports overhead achievable = FALSE with no ramp lever at all", {
  assumptions <- list(
    fee_args = list(visit_volume = 5, new_visit_fee = 50, follow_up_fee = 30, ramp_months = 1),
    overhead_monthly = 50000
  )
  gs <- goal_seek_projection_recovery(assumptions, horizon_months = 12)

  expect_false(gs$overhead$achievable)
  expect_null(gs$ramp)
  expect_null(gs$combined)
})

# -- interpret_projection() + goal_seek --------------------------------------

test_that("interpret_projection() describes both achievable levers", {
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 24),
    overhead_monthly = 15000
  )
  projection <- project_scenarios(assumptions, horizon_months = 12)
  gs <- goal_seek_projection_recovery(assumptions, horizon_months = 12)

  text <- interpret_projection(projection, goal_seek = gs)

  expect_true(grepl("any one of the following would do it", text))
  expect_true(grepl("Cut monthly overhead by about 46%", text))
  expect_true(grepl("Shorten your membership ramp from 24 to about 13 months", text))
  expect_true(grepl("Make a smaller change to both together", text))
})

test_that("interpret_projection() falls back to a single-sentence, non-bulleted form for exactly one achievable lever", {
  # A fee-for-service-only plan has no ramp lever to search (gs$ramp is
  # NULL, see the fixture above), leaving overhead as the only lever --
  # exercises the length(clauses) == 1L branch in .describe_goal_seek(),
  # not the bulleted-list one.
  assumptions <- list(
    fee_args = list(visit_volume = 5, new_visit_fee = 50, follow_up_fee = 30, ramp_months = 1),
    overhead_monthly = 5000
  )
  projection <- project_scenarios(assumptions, horizon_months = 12)
  gs <- goal_seek_projection_recovery(assumptions, horizon_months = 12)

  text <- interpret_projection(projection, goal_seek = gs)

  expect_false(grepl("\n-", text, fixed = TRUE))
  expect_true(grepl("on this lever alone, you'd need to cut monthly overhead", text))
})

test_that("interpret_projection() reports non-achievability plainly", {
  assumptions <- list(
    fee_args = list(visit_volume = 5, new_visit_fee = 50, follow_up_fee = 30, ramp_months = 1),
    overhead_monthly = 50000
  )
  projection <- project_scenarios(assumptions, horizon_months = 12)
  gs <- goal_seek_projection_recovery(assumptions, horizon_months = 12)

  text <- interpret_projection(projection, goal_seek = gs)

  expect_true(grepl("wouldn't recover ramp-period costs", text))
})

test_that("interpret_projection() ignores goal_seek when the base scenario already recovers", {
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 3),
    overhead_monthly = 5000
  )
  projection <- project_scenarios(assumptions, horizon_months = 24)
  gs <- goal_seek_projection_recovery(assumptions, horizon_months = 24)

  text <- interpret_projection(projection, goal_seek = gs)

  expect_false(grepl("To recover by month", text))
})

# -- interpret_projection()'s "pro_paragraph" attribute ----------------------

test_that("interpret_projection() tags no paragraphs as Pro when neither extra is supplied", {
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 12),
    overhead_monthly = 5000
  )
  projection <- project_scenarios(assumptions, horizon_months = 24)

  text <- interpret_projection(projection)

  expect_identical(attr(text, "pro_paragraph"), rep(FALSE, 3))
})

test_that("interpret_projection() tags the trailing paragraph as Pro when a decomposition is supplied", {
  assumptions <- list(
    membership_args = list(panel_size = 200, fee = 80, ramp_months = 12),
    overhead_monthly = 8000
  )
  projection <- project_scenarios(assumptions, horizon_months = 24)
  decomp <- decompose_projection_sensitivity(assumptions, horizon_months = 24)

  text <- interpret_projection(projection, decomp)

  expect_identical(attr(text, "pro_paragraph"), c(FALSE, FALSE, FALSE, TRUE))
})

test_that("interpret_projection() tags the trailing paragraph as Pro when goal_seek fires", {
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 24),
    overhead_monthly = 15000
  )
  projection <- project_scenarios(assumptions, horizon_months = 12)
  gs <- goal_seek_projection_recovery(assumptions, horizon_months = 12)

  text <- interpret_projection(projection, goal_seek = gs)

  expect_identical(attr(text, "pro_paragraph"), c(FALSE, FALSE, FALSE, TRUE))
})

test_that("interpret_projection() tags both trailing paragraphs when decomposition and goal_seek both fire", {
  # Hand-built fixtures rather than real project_scenarios()/
  # decompose_projection_sensitivity() output: a non-recovering base
  # (required for goal_seek_sentence to fire) structurally forces
  # decompose_projection_sensitivity()'s own isolated-conservative variant
  # to also fail to recover (it can only make things worse than base), so
  # a real decomposition is always degenerate whenever goal_seek would
  # fire -- this exercises interpret_projection()'s own tagging logic
  # for the case directly, independent of that real-world correlation.
  projection <- data.frame(
    scenario = rep(c("base", "conservative", "optimistic"), each = 3),
    month = rep(1:3, 3),
    cumulative_net_income = c(-3000, -2500, -2000, -4000, -3500, -3000, -2000, -1500, -1000)
  )
  decomp <- structure(
    list(ramp_spread_months = 5L, overhead_spread_months = 3L, dominant_lever = "ramp"),
    class = "dcPlanR_sensitivity_decomposition"
  )
  gs <- structure(
    list(
      target_month = 12L,
      overhead = list(achievable = TRUE, pct_cut = 30, new_overhead_monthly = 7000),
      ramp = list(achievable = TRUE, original_ramp_months = 24L, new_ramp_months = 13L, months_faster = 11L)
    ),
    class = "dcPlanR_goal_seek"
  )

  text <- interpret_projection(projection, decomp, gs)

  expect_identical(attr(text, "pro_paragraph"), c(FALSE, FALSE, FALSE, TRUE, TRUE))
})

test_that("interpret_projection() doesn't tag a paragraph that a degenerate decomposition never produced", {
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 12),
    overhead_monthly = 5000
  )
  projection <- project_scenarios(assumptions, horizon_months = 24)
  # Both levers NA -- .describe_sensitivity_decomposition() returns NULL
  # for this, so no fourth paragraph should exist to tag as Pro even
  # though a (degenerate) decomposition object was passed.
  degenerate <- structure(
    list(ramp_spread_months = NA_integer_, overhead_spread_months = NA_integer_, dominant_lever = NA_character_),
    class = "dcPlanR_sensitivity_decomposition"
  )

  text <- interpret_projection(projection, degenerate)

  expect_identical(attr(text, "pro_paragraph"), rep(FALSE, 3))
})

test_that("interpret_projection() doesn't tag a goal_seek paragraph the base scenario didn't need", {
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 3),
    overhead_monthly = 5000
  )
  projection <- project_scenarios(assumptions, horizon_months = 24)
  gs <- goal_seek_projection_recovery(assumptions, horizon_months = 24)

  text <- interpret_projection(projection, goal_seek = gs)

  expect_identical(attr(text, "pro_paragraph"), rep(FALSE, 3))
})

# -- compare_plan_scenarios --------------------------------------------------

mk_scn <- function(label, recovery_month) {
  list(label = label, recovery_month = recovery_month)
}

test_that("compare_plan_scenarios() names the earlier scenario when both recover", {
  scenarios <- list(mk_scn("Scenario A", 14L), mk_scn("Scenario B", 8L))

  text <- compare_plan_scenarios(scenarios)

  expect_true(grepl("Scenario B recovers its ramp-period costs soonest, by month 8", text))
  expect_true(grepl("Scenario A follows 6 months later, by month 14", text))
})

test_that("compare_plan_scenarios() singularizes a 1-month gap", {
  scenarios <- list(mk_scn("A", 6L), mk_scn("B", 5L))

  text <- compare_plan_scenarios(scenarios)

  expect_true(grepl("follows 1 month later", text))
  expect_false(grepl("1 months", text))
})

test_that("compare_plan_scenarios() handles 3 scenarios with one not reached", {
  scenarios <- list(mk_scn("A", 14L), mk_scn("B", 8L), mk_scn("C", NA_integer_))

  text <- compare_plan_scenarios(scenarios)

  expect_true(grepl("B recovers its ramp-period costs soonest, by month 8", text))
  expect_true(grepl("A follows 6 months later, by month 14", text))
  expect_true(grepl("C does not recover within its projection horizon", text))
})

test_that("compare_plan_scenarios() reports when none recover", {
  scenarios <- list(mk_scn("A", NA_integer_), mk_scn("B", NA_integer_))

  text <- compare_plan_scenarios(scenarios)

  expect_true(grepl("None of your saved scenarios", text))
  expect_true(grepl("A and B", text))
})

test_that("compare_plan_scenarios() returns NULL for fewer than 2 scenarios", {
  expect_null(compare_plan_scenarios(list()))
  expect_null(compare_plan_scenarios(list(mk_scn("A", 5L))))
})

# -- interpret_capital -------------------------------------------------------

test_that("interpret_capital() reports totals, top line items, and a combined figure", {
  startup_costs <- calc_startup_costs(c(equipment = 5000, ehr_setup = 8000, licensing = 1000))
  personal_runway <- calc_personal_runway(monthly_expenses = 4000, months_coverage = 6)
  text <- interpret_capital(startup_costs, personal_runway)

  expect_true(grepl("\\$14,000", text))
  expect_true(grepl("EHR Setup \\(\\$8,000\\)", text))
  expect_true(grepl("Equipment \\(\\$5,000\\)", text))
  expect_false(grepl("[Ll]icensing", text)) # smallest item, not in the top 2
  expect_true(grepl("6 months at \\$4,000/month", text))
  expect_true(grepl("\\$24,000", text))
  expect_true(grepl("secure at least \\$38,000", text))
})
