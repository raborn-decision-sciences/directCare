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
    session$setInputs(est_tier_members_1 = 80, est_tier_fee_1 = 99)
    expect_equal(membership_income_r(), 80 * 99)
  })
})

test_that("membership_income_r treats NA inputs as 0", {
  r <- empty_r()
  testServer(mod_edit_server, args = list(r = r), {
    session$setInputs(est_tier_members_1 = NA_real_, est_tier_fee_1 = 99)
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
      est_tier_members_1 = 50,
      est_tier_fee_1 = 89,
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
      est_tier_members_1 = 40,
      est_tier_fee_1 = 100,
      est_tier_growth_1 = 5,
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
      est_tier_members_1 = 60,
      est_tier_fee_1 = 95,
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
      est_tier_members_1 = 50,
      est_tier_fee_1 = 89,
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

# ── label_editor_ui (Customize Category Labels) ─────────────────────────────

# Full-schema income fixture (mirrors empty_r()'s income tibble) with real
# rows so unrelated observers (e.g. the date-range filter) don't error.
make_income_with_accounts <- function(account_names) {
  n <- length(account_names)
  tibble::tibble(
    practice_id = rep("test", n),
    date = rep(as.Date("2025-01-15"), n),
    week_start = rep(as.Date("2025-01-13"), n),
    month = rep(1L, n),
    year = rep(2025L, n),
    full_account_name = paste0("Income:", account_names),
    account_name = account_names,
    description = rep("", n),
    revenue = rep(100, n),
    category = rep("other", n),
    source = rep("gnucash_csv", n),
    is_refund = rep(FALSE, n)
  )
}

test_that("label_editor_ui renders without error for income accounts with no default label", {
  r <- empty_r()
  r$income <- make_income_with_accounts(
    c("Membership Fees", "Fee-for-Service", "Grants", "Other Income")
  )
  testServer(mod_edit_server, args = list(r = r), {
    expect_no_error(force(output$label_editor_ui))
  })
})

test_that("label_editor_ui renders without error when source_labels lacks a detected account", {
  r <- empty_r()
  r$income <- make_income_with_accounts(c("Membership Fees", "Grants"))
  r$source_labels <- c("Membership Fees" = "Panel Revenue")
  testServer(mod_edit_server, args = list(r = r), {
    expect_no_error(force(output$label_editor_ui))
  })
})

# ── Inline date editing (edit_table_cell_edit, column 0) ────────────────────

make_transactions_row <- function(
  date,
  full_account_name,
  account_name,
  amount,
  category = "other"
) {
  tibble::tibble(
    practice_id = "test",
    date = as.Date(date),
    week_start = lubridate::floor_date(as.Date(date), "week", week_start = 1),
    month = lubridate::month(as.Date(date)),
    year = lubridate::year(as.Date(date)),
    full_account_name = full_account_name,
    account_name = account_name,
    description = "",
    amount = amount,
    category = category,
    source = "gnucash_csv"
  )
}

test_that("editing the date column updates date/week_start/month/year and regenerates overhead+income", {
  r <- empty_r()
  # A mis-dated income row (the real-world bug: year typo'd as 1602).
  r$transactions <- make_transactions_row(
    "1602-02-16",
    "Income:Other Income",
    "Other Income",
    10 # ingest_gnucash_csv() already negates raw GnuCash income splits to
    # positive by the time rows land in r$transactions.
  )
  shiny::isolate({
    r$overhead <- directCareForecastR::filter_gnucash_overhead(r$transactions)
    r$income <- suppressWarnings(
      directCareForecastR::normalize_gnucash_income(r$transactions)
    )
  })

  testServer(mod_edit_server, args = list(r = r), {
    session$setInputs(
      edit_table_cell_edit = list(row = 1, col = 0, value = "2026-02-16")
    )

    expect_equal(r$transactions$date, as.Date("2026-02-16"))
    expect_equal(r$transactions$year, 2026L)
    expect_equal(r$transactions$month, 2L)
    expect_equal(
      r$transactions$week_start,
      lubridate::floor_date(as.Date("2026-02-16"), "week", week_start = 1)
    )
    expect_equal(r$income$date, as.Date("2026-02-16"))
    expect_equal(r$income$revenue, 10)
  })
})

test_that("editing the date column with an unparseable value leaves the row unchanged", {
  r <- empty_r()
  r$transactions <- make_transactions_row(
    "2026-02-16",
    "Expenses:Rent",
    "Rent",
    100
  )
  shiny::isolate({
    r$overhead <- directCareForecastR::filter_gnucash_overhead(r$transactions)
    r$income <- suppressWarnings(
      directCareForecastR::normalize_gnucash_income(r$transactions)
    )
  })

  testServer(mod_edit_server, args = list(r = r), {
    session$setInputs(
      edit_table_cell_edit = list(row = 1, col = 0, value = "not a date")
    )
    expect_equal(r$transactions$date, as.Date("2026-02-16"))
  })
})

test_that("editing the date column accepts MM/DD/YYYY as well as YYYY-MM-DD", {
  r <- empty_r()
  r$transactions <- make_transactions_row(
    "2026-02-16",
    "Expenses:Rent",
    "Rent",
    100
  )
  shiny::isolate({
    r$overhead <- directCareForecastR::filter_gnucash_overhead(r$transactions)
    r$income <- suppressWarnings(
      directCareForecastR::normalize_gnucash_income(r$transactions)
    )
  })

  testServer(mod_edit_server, args = list(r = r), {
    session$setInputs(
      edit_table_cell_edit = list(row = 1, col = 0, value = "03/01/2026")
    )
    expect_equal(r$transactions$date, as.Date("2026-03-01"))
  })
})
