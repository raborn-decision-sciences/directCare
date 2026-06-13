# Unit tests for mod_edit_server (via testServer)

# ── Fixture helpers ────────────────────────────────────────────────────────────

empty_r <- function(name = "Test Practice", id = "test") {
  shiny::reactiveValues(
    practice_id = id,
    practice_name = name,
    panel_size = NULL,
    membership_fee = NULL,
    transactions = tibble::tibble(
      practice_id = character(0),
      date = as.Date(character(0)),
      week_start = as.Date(character(0)),
      month = integer(0),
      year = integer(0),
      full_account_name = character(0),
      account_name = character(0),
      description = character(0),
      amount = numeric(0),
      category = character(0),
      source = character(0)
    ),
    overhead = tibble::tibble(
      practice_id = character(0),
      date = as.Date(character(0)),
      week_start = as.Date(character(0)),
      month = integer(0),
      year = integer(0),
      full_account_name = character(0),
      account_name = character(0),
      description = character(0),
      amount = numeric(0),
      category = character(0),
      source = character(0),
      is_refund = logical(0)
    ),
    income = tibble::tibble(
      practice_id = character(0),
      date = as.Date(character(0)),
      week_start = as.Date(character(0)),
      month = integer(0),
      year = integer(0),
      full_account_name = character(0),
      account_name = character(0),
      description = character(0),
      revenue = numeric(0),
      category = character(0),
      source = character(0),
      is_refund = logical(0)
    ),
    overhead_monthly = NULL,
    income_monthly = NULL,
    validation = list()
  )
}

# ── .nn() helper ───────────────────────────────────────────────────────────────

test_that(".nn() returns 0 for NULL input", {
  r <- empty_r()
  testServer(mod_edit_server, args = list(r = r), {
    expect_equal(.nn(NULL), 0)
  })
})

test_that(".nn() returns 0 for NA input", {
  r <- empty_r()
  testServer(mod_edit_server, args = list(r = r), {
    expect_equal(.nn(NA_real_), 0)
  })
})

test_that(".nn() returns 0 for Inf and NaN", {
  r <- empty_r()
  testServer(mod_edit_server, args = list(r = r), {
    expect_equal(.nn(Inf), 0)
    expect_equal(.nn(-Inf), 0)
    expect_equal(.nn(NaN), 0)
  })
})

test_that(".nn() passes through finite numeric values unchanged", {
  r <- empty_r()
  testServer(mod_edit_server, args = list(r = r), {
    expect_equal(.nn(0), 0)
    expect_equal(.nn(42), 42)
    expect_equal(.nn(-5), -5)
    expect_equal(.nn(0.75), 0.75)
  })
})

# ── membership_income_r ────────────────────────────────────────────────────────

test_that("membership_income_r is 0 when inputs are absent", {
  r <- empty_r()
  testServer(mod_edit_server, args = list(r = r), {
    expect_equal(membership_income_r(), 0)
  })
})

test_that("membership_income_r multiplies panel_size by monthly_fee", {
  r <- empty_r()
  testServer(mod_edit_server, args = list(r = r), {
    session$setInputs(est_panel_size = 80, est_monthly_fee = 99)
    expect_equal(membership_income_r(), 80 * 99)
  })
})

test_that("membership_income_r treats NA inputs as 0", {
  r <- empty_r()
  testServer(mod_edit_server, args = list(r = r), {
    session$setInputs(est_panel_size = NA_real_, est_monthly_fee = 99)
    expect_equal(membership_income_r(), 0)
  })
})

# ── ffs_income_r ───────────────────────────────────────────────────────────────

test_that("ffs_income_r is 0 when all FFS inputs are absent", {
  r <- empty_r()
  testServer(mod_edit_server, args = list(r = r), {
    expect_equal(ffs_income_r(), 0)
  })
})

test_that("ffs_income_r sums new-visit, followup, and other income", {
  r <- empty_r()
  testServer(mod_edit_server, args = list(r = r), {
    session$setInputs(
      est_new_patients_mo = 10,
      est_new_visit_fee = 150,
      est_followups_mo = 20,
      est_followup_fee = 75,
      est_other_income = 200
    )
    expect_equal(ffs_income_r(), 10 * 150 + 20 * 75 + 200)
  })
})

# ── ovhd_total_r ───────────────────────────────────────────────────────────────

test_that("ovhd_total_r is 0 when all overhead inputs are absent", {
  r <- empty_r()
  testServer(mod_edit_server, args = list(r = r), {
    expect_equal(ovhd_total_r(), 0)
  })
})

test_that("ovhd_total_r sums all six overhead categories", {
  r <- empty_r()
  testServer(mod_edit_server, args = list(r = r), {
    session$setInputs(
      est_rent = 1200,
      est_payroll = 3000,
      est_ehr = 200,
      est_malpractice = 150,
      est_supplies = 100,
      est_other_overhead = 250
    )
    expect_equal(ovhd_total_r(), 1200 + 3000 + 200 + 150 + 100 + 250)
  })
})

test_that("ovhd_total_r ignores NA entries", {
  r <- empty_r()
  testServer(mod_edit_server, args = list(r = r), {
    session$setInputs(est_rent = 1000, est_payroll = NA_real_)
    expect_equal(ovhd_total_r(), 1000)
  })
})

# ── estimator_done flag ────────────────────────────────────────────────────────

test_that("estimator_done starts FALSE", {
  r <- empty_r()
  testServer(mod_edit_server, args = list(r = r), {
    expect_false(estimator_done())
  })
})

test_that("btn_generate sets estimator_done to TRUE and populates r$overhead_monthly", {
  r <- empty_r()
  testServer(mod_edit_server, args = list(r = r), {
    session$setInputs(
      est_rent = 1000,
      est_panel_size = 50,
      est_monthly_fee = 89,
      est_n_months = 6,
      est_start_month = 1L,
      est_start_year = 2025L
    )
    session$setInputs(btn_generate = 1)

    expect_true(estimator_done())
    expect_s3_class(r$overhead_monthly, "data.frame")
    expect_equal(nrow(r$overhead_monthly), 6L)
    expect_equal(r$overhead_monthly$total_overhead, rep(1000, 6))
  })
})

test_that("btn_generate populates r$income_monthly with panel growth", {
  r <- empty_r()
  testServer(mod_edit_server, args = list(r = r), {
    session$setInputs(
      est_rent = 1000, # overhead required to pass the ovhd > 0 guard
      est_panel_size = 40,
      est_monthly_fee = 100,
      est_monthly_growth = 5,
      est_n_months = 3,
      est_start_month = 1L,
      est_start_year = 2025L
    )
    session$setInputs(btn_generate = 1)

    rev <- r$income_monthly$total_revenue
    expect_equal(length(rev), 3L)
    # panel grows: 40, 45, 50 × $100
    expect_equal(rev, c(40, 45, 50) * 100)
  })
})

test_that("btn_generate pre-populates r$panel_size and r$membership_fee", {
  r <- empty_r()
  testServer(mod_edit_server, args = list(r = r), {
    session$setInputs(
      est_rent = 1000, # overhead required to pass the ovhd > 0 guard
      est_panel_size = 60,
      est_monthly_fee = 95,
      est_n_months = 6,
      est_start_month = 1L,
      est_start_year = 2025L
    )
    session$setInputs(btn_generate = 1)

    expect_equal(r$panel_size, 60)
    expect_equal(r$membership_fee, 95)
  })
})

test_that("btn_restart_estimator resets estimator_done and clears r slots", {
  r <- empty_r()
  testServer(mod_edit_server, args = list(r = r), {
    session$setInputs(
      est_rent = 1000,
      est_panel_size = 50,
      est_monthly_fee = 89,
      est_n_months = 6,
      est_start_month = 1L,
      est_start_year = 2025L
    )
    session$setInputs(btn_generate = 1)
    expect_true(estimator_done())

    session$setInputs(btn_restart_estimator = 1)
    expect_false(estimator_done())
    expect_null(r$overhead_monthly)
    expect_null(r$income_monthly)
    expect_null(r$panel_size)
    expect_null(r$membership_fee)
  })
})

test_that("Edit tab shows manual-entry message when overhead_monthly set but no scenario_inputs", {
  r <- shiny::reactiveValues(
    practice_id = "test",
    practice_name = "Test",
    panel_size = NULL,
    membership_fee = NULL,
    scenario_inputs = NULL,
    transactions = tibble::tibble(
      practice_id = character(0),
      date = as.Date(character(0)),
      week_start = as.Date(character(0)),
      month = integer(0),
      year = integer(0),
      full_account_name = character(0),
      account_name = character(0),
      description = character(0),
      amount = numeric(0),
      category = character(0),
      source = character(0)
    ),
    overhead = NULL,
    income = NULL,
    overhead_monthly = tibble::tibble(
      practice_id = "test",
      year = 2024L,
      month = 1:6,
      total_overhead = rep(2000, 6),
      gross_overhead = rep(2000, 6),
      total_refunds = rep(0, 6)
    ),
    income_monthly = tibble::tibble(
      practice_id = "test",
      year = 2024L,
      month = 1:6,
      total_revenue = rep(5000, 6)
    ),
    validation = list()
  )

  testServer(mod_edit_server, args = list(r = r), {
    html <- as.character(output$manual_entry_ui$html)
    expect_true(grepl("No Transactions to Review", html, fixed = TRUE))
    expect_true(grepl("aggregate data manually", html, fixed = TRUE))
  })
})
