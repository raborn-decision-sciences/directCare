# Unit tests for mod_calculator_server (via testServer)

empty_r <- function() {
  shiny::reactiveValues()
}

# ── total_overhead() ────────────────────────────────────────────────────────

test_that("total_overhead uses the single field when ovhd_multi is off", {
  r <- empty_r()
  testServer(mod_calculator_server, args = list(r = r), {
    session$setInputs(monthly_overhead = 3000)
    expect_equal(total_overhead(), 3000)
  })
})

test_that("total_overhead sums sources when ovhd_multi is on", {
  r <- empty_r()
  testServer(mod_calculator_server, args = list(r = r), {
    session$setInputs(ovhd_multi = TRUE, ovhd_item_amount_1 = 2000)
    session$setInputs(btn_add_ovhd_item = 1)
    session$setInputs(ovhd_item_amount_2 = 500)
    expect_equal(total_overhead(), 2500)
  })
})

# ── other_income_total() ────────────────────────────────────────────────────

test_that("other_income_total sums the default Fee-for-Service and Other Income rows", {
  r <- empty_r()
  testServer(mod_calculator_server, args = list(r = r), {
    session$setInputs(inc_item_amount_1 = 800, inc_item_amount_2 = 200)
    expect_equal(other_income_total(), 1000)
  })
})

test_that("other_income_total grows when an income source is added", {
  r <- empty_r()
  testServer(mod_calculator_server, args = list(r = r), {
    session$setInputs(inc_item_amount_1 = 800, inc_item_amount_2 = 200)
    session$setInputs(btn_add_income_item = 1)
    session$setInputs(inc_item_amount_3 = 300)
    expect_equal(other_income_total(), 1300)
  })
})

test_that("other_income_total is 0 once all income sources are removed", {
  r <- empty_r()
  testServer(mod_calculator_server, args = list(r = r), {
    session$setInputs(inc_item_amount_1 = 800, inc_item_amount_2 = 200)
    session$setInputs(btn_remove_income_item_1 = 1)
    session$setInputs(btn_remove_income_item_1 = 1)
    expect_equal(other_income_total(), 0)
  })
})

# ── tier_total_members() / tier_total_revenue() / avg_fee_per_member() ─────

test_that("tier_total_members and tier_total_revenue sum a single tier", {
  r <- empty_r()
  testServer(mod_calculator_server, args = list(r = r), {
    session$setInputs(tier_members_1 = 80, tier_fee_1 = 90)
    expect_equal(tier_total_members(), 80)
    expect_equal(tier_total_revenue(), 80 * 90)
    expect_equal(avg_fee_per_member(), 90)
  })
})

test_that("avg_fee_per_member is a weighted average across tiers", {
  r <- empty_r()
  testServer(mod_calculator_server, args = list(r = r), {
    session$setInputs(tier_members_1 = 100, tier_fee_1 = 100)
    session$setInputs(btn_add_tier = 1)
    session$setInputs(tier_members_2 = 50, tier_fee_2 = 40)
    # (100*100 + 50*40) / 150 = 12000/150 = 80
    expect_equal(avg_fee_per_member(), 80)
  })
})

# ── Break-even/target math nets out other income (results_ui) ──────────────
# results_ui itself is a renderUI closure; exercise the same reactives it
# reads to confirm the "other income covers part of overhead" arithmetic.

test_that("other income reduces the overhead that must come from membership dues", {
  r <- empty_r()
  testServer(mod_calculator_server, args = list(r = r), {
    session$setInputs(
      monthly_overhead = 5000,
      inc_item_amount_1 = 1000,
      inc_item_amount_2 = 0,
      tier_members_1 = 40,
      tier_fee_1 = 100
    )
    ovhd <- total_overhead()
    other_inc <- other_income_total()
    net_needed_breakeven <- max(0, ovhd - other_inc)
    expect_equal(net_needed_breakeven, 4000)
    expect_equal(ceiling(net_needed_breakeven / avg_fee_per_member()), 40)
  })
})

test_that("other income exceeding overhead floors the membership requirement at 0", {
  r <- empty_r()
  testServer(mod_calculator_server, args = list(r = r), {
    session$setInputs(
      monthly_overhead = 1000,
      inc_item_amount_1 = 5000,
      inc_item_amount_2 = 0
    )
    ovhd <- total_overhead()
    other_inc <- other_income_total()
    net_needed_breakeven <- max(0, ovhd - other_inc)
    expect_equal(net_needed_breakeven, 0)
  })
})
