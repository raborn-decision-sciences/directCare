# Unit tests for mod_summary_server (via testServer)

# ── Fixture helpers ────────────────────────────────────────────────────────────

monthly_overhead <- function(n = 3) {
  tibble::tibble(
    practice_id = rep("test", n),
    year = rep(2025L, n),
    month = seq_len(n),
    total_overhead = seq(1000, by = 200, length.out = n),
    gross_overhead = seq(1000, by = 200, length.out = n),
    total_refunds = rep(0, n)
  )
}

monthly_income <- function(n = 3) {
  tibble::tibble(
    practice_id = rep("test", n),
    year = rep(2025L, n),
    month = seq_len(n),
    total_revenue = seq(800, by = 300, length.out = n)
  )
}

weekly_overhead <- function(n = 4) {
  weeks <- seq(as.Date("2025-01-06"), by = "week", length.out = n)
  tibble::tibble(
    practice_id = rep("test", n),
    week_start = weeks,
    total_overhead = rep(250, n),
    gross_overhead = rep(250, n),
    total_refunds = rep(0, n)
  )
}

weekly_income <- function(n = 4) {
  weeks <- seq(as.Date("2025-01-06"), by = "week", length.out = n)
  tibble::tibble(
    practice_id = rep("test", n),
    week_start = weeks,
    total_revenue = seq(200, by = 50, length.out = n)
  )
}

empty_income <- function(weekly = FALSE) {
  if (weekly) {
    tibble::tibble(
      practice_id = character(0),
      week_start = as.Date(character(0)),
      total_revenue = numeric(0)
    )
  } else {
    tibble::tibble(
      practice_id = character(0),
      year = integer(0),
      month = integer(0),
      total_revenue = numeric(0)
    )
  }
}

make_r <- function(ovhd = monthly_overhead(), inc = monthly_income()) {
  shiny::reactiveValues(
    overhead_monthly = ovhd,
    income_monthly = inc,
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
    )
  )
}

# ── is_weekly ──────────────────────────────────────────────────────────────────

test_that("is_weekly returns FALSE for monthly overhead", {
  r <- make_r()
  testServer(mod_summary_server, args = list(r = r), {
    expect_false(is_weekly())
  })
})

test_that("is_weekly returns TRUE for weekly overhead", {
  r <- make_r(ovhd = weekly_overhead(), inc = weekly_income())
  testServer(mod_summary_server, args = list(r = r), {
    expect_true(is_weekly())
  })
})

# ── ovhd_overall ──────────────────────────────────────────────────────────────

test_that("ovhd_overall has period_start and total columns", {
  r <- make_r()
  testServer(mod_summary_server, args = list(r = r), {
    d <- ovhd_overall()
    expect_s3_class(d, "data.frame")
    expect_true(all(c("period_start", "total") %in% names(d)))
  })
})

test_that("ovhd_overall period_start is sorted ascending", {
  r <- make_r()
  testServer(mod_summary_server, args = list(r = r), {
    d <- ovhd_overall()
    expect_equal(d$period_start, sort(d$period_start))
  })
})

test_that("ovhd_overall constructs correct dates from year/month", {
  r <- make_r(ovhd = monthly_overhead(3))
  testServer(mod_summary_server, args = list(r = r), {
    d <- ovhd_overall()
    expect_equal(d$period_start[1], as.Date("2025-01-01"))
    expect_equal(d$period_start[3], as.Date("2025-03-01"))
  })
})

test_that("ovhd_overall renames week_start to period_start for weekly data", {
  r <- make_r(ovhd = weekly_overhead(), inc = weekly_income())
  testServer(mod_summary_server, args = list(r = r), {
    d <- ovhd_overall()
    expect_true("period_start" %in% names(d))
    expect_false("week_start" %in% names(d))
  })
})

# ── inc_overall ───────────────────────────────────────────────────────────────

test_that("inc_overall returns NULL for 0-row income (no charToDate crash)", {
  r <- make_r(inc = empty_income())
  testServer(mod_summary_server, args = list(r = r), {
    expect_null(inc_overall())
  })
})

test_that("inc_overall returns NULL for 0-row weekly income", {
  r <- make_r(ovhd = weekly_overhead(), inc = empty_income(weekly = TRUE))
  testServer(mod_summary_server, args = list(r = r), {
    expect_null(inc_overall())
  })
})

test_that("inc_overall has period_start and total columns for monthly data", {
  r <- make_r()
  testServer(mod_summary_server, args = list(r = r), {
    d <- inc_overall()
    expect_true(all(c("period_start", "total") %in% names(d)))
    expect_equal(nrow(d), 3L)
  })
})

test_that("inc_overall is sorted ascending by period_start", {
  r <- make_r()
  testServer(mod_summary_server, args = list(r = r), {
    d <- inc_overall()
    expect_equal(d$period_start, sort(d$period_start))
  })
})

# ── active_range ──────────────────────────────────────────────────────────────

test_that("active_range hi is last day of latest month for monthly data", {
  r <- make_r(ovhd = monthly_overhead(3)) # Jan–Mar 2025
  testServer(mod_summary_server, args = list(r = r), {
    ar <- active_range()
    expect_equal(ar$lo, as.Date("2025-01-01"))
    expect_equal(ar$hi, as.Date("2025-03-31"))
  })
})

test_that("active_range hi is period_start + 6 days for weekly data", {
  r <- make_r(ovhd = weekly_overhead(4), inc = weekly_income(4))
  testServer(mod_summary_server, args = list(r = r), {
    ar <- active_range()
    last <- max(weekly_overhead(4)$week_start)
    expect_equal(ar$hi, last + 6L)
  })
})

# -- Manual-entry workflow (r$overhead and r$income are NULL) ------------------

make_r_manual <- function(n = 6) {
  shiny::reactiveValues(
    overhead_monthly = monthly_overhead(n),
    income_monthly = monthly_income(n),
    overhead = NULL,
    income = NULL
  )
}

test_that("ovhd_overall works when r$overhead is NULL (manual-entry workflow)", {
  r <- make_r_manual()
  testServer(mod_summary_server, args = list(r = r), {
    d <- ovhd_overall()
    expect_s3_class(d, "data.frame")
    expect_true("period_start" %in% names(d))
    expect_true("total" %in% names(d))
    expect_equal(nrow(d), 6L)
  })
})

test_that("inc_overall works when r$income is NULL (manual-entry workflow)", {
  r <- make_r_manual()
  testServer(mod_summary_server, args = list(r = r), {
    d <- inc_overall()
    expect_s3_class(d, "data.frame")
    expect_equal(nrow(d), 6L)
  })
})

test_that("ovhd_by_cat returns nothing (req fails silently) when r$overhead is NULL", {
  r <- make_r_manual()
  testServer(mod_summary_server, args = list(r = r), {
    # req(r$overhead) inside ovhd_by_cat() raises a silent error when NULL;
    # the result is indistinguishable from NULL in the calling context.
    result <- tryCatch(ovhd_by_cat(), shiny.silent.error = function(e) NULL)
    expect_null(result)
  })
})
