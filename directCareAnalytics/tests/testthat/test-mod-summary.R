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

# ── .pretty_cat / .pretty_src / .build_palette (category label overrides) ──────

test_that(".pretty_cat returns default labels with no overrides", {
  expect_equal(.pretty_cat("rent"), "Rent")
  expect_equal(.pretty_cat("staff"), "Staff / Payroll")
})

test_that(".pretty_cat applies overrides and falls back for unmapped keys", {
  overrides <- c(rent = "Facility Costs")
  expect_equal(.pretty_cat("rent", overrides), "Facility Costs")
  expect_equal(.pretty_cat("staff", overrides), "Staff / Payroll")
  expect_equal(.pretty_cat("unknown_slug", overrides), "unknown_slug")
})

test_that(".pretty_src returns default labels with no overrides", {
  expect_equal(.pretty_src("Membership Fees"), "Membership")
  expect_equal(.pretty_src("Fee-for-Service"), "Fee-for-Service")
})

test_that(".pretty_src applies overrides including custom account names", {
  overrides <- c("Membership Fees" = "Panel Revenue", "Grants" = "Grant Income")
  expect_equal(.pretty_src("Membership Fees", overrides), "Panel Revenue")
  expect_equal(.pretty_src("Grants", overrides), "Grant Income")
  expect_equal(.pretty_src("Other Income", overrides), "Other Income")
})

test_that(".build_palette keys the palette by the current display label, not the slug", {
  pal <- .build_palette(c("rent", "staff"), .cat_palette, .pretty_cat, NULL)
  expect_equal(unname(pal["Rent"]), .cat_palette[["rent"]])
  expect_equal(unname(pal["Staff / Payroll"]), .cat_palette[["staff"]])
})

test_that(".build_palette keeps the original color after a category is renamed", {
  overrides <- c(rent = "Facility Costs")
  pal <- .build_palette(
    c("rent", "staff"),
    .cat_palette,
    .pretty_cat,
    overrides
  )
  expect_equal(unname(pal["Facility Costs"]), .cat_palette[["rent"]])
  expect_false("Rent" %in% names(pal))
})

test_that(".build_palette assigns a fallback color to unrecognised keys", {
  pal <- .build_palette("Grants", .src_palette, .pretty_src, NULL)
  expect_equal(unname(pal["Grants"]), .fallback_color)
})

# ── .grouped_totals_dt() (category/source pie-mode summary table) ──────────

transactions_overhead <- function() {
  tibble::tibble(
    practice_id = "test",
    date = as.Date(c(
      "2025-01-05",
      "2025-01-10",
      "2025-02-05",
      "2025-02-10"
    )),
    week_start = as.Date(c(
      "2025-01-05",
      "2025-01-05",
      "2025-02-02",
      "2025-02-02"
    )),
    month = c(1L, 1L, 2L, 2L),
    year = c(2025L, 2025L, 2025L, 2025L),
    full_account_name = c(
      "Expenses:Rent",
      "Expenses:Staff",
      "Expenses:Rent",
      "Expenses:Staff"
    ),
    account_name = c("Rent", "Staff", "Rent", "Staff"),
    description = "",
    amount = c(1000, 500, 1000, 700),
    category = c("rent", "staff", "rent", "staff"),
    source = "gnucash_csv",
    is_refund = FALSE
  )
}

test_that(".grouped_totals_dt aggregates across all rows and computes % of total", {
  r <- make_r()
  testServer(mod_summary_server, args = list(r = r), {
    d <- tibble::tibble(
      label = c("Rent", "Staff", "Rent"),
      total = c(1000, 500, 200)
    )
    dt <- .grouped_totals_dt(d, "Category")
    out <- dt$x$data
    expect_equal(names(out)[1], "Category")
    expect_equal(out$Category, c("Rent", "Staff"))
    expect_equal(out$Total, c("$1,200.00", "$500.00"))
    expect_equal(out[["% of Total"]], c("71%", "29%"))
  })
})

test_that("ovhd_by_cat can be filtered to a single period, matching what the by-period pie shows", {
  r <- make_r()
  r$overhead <- transactions_overhead()
  testServer(mod_summary_server, args = list(r = r), {
    d <- ovhd_by_cat()
    expect_equal(
      sort(unique(d$period_start)),
      as.Date(c("2025-01-01", "2025-02-01"))
    )

    jan <- dplyr::filter(d, period_start == as.Date("2025-01-01"))
    expect_equal(sum(jan$total[jan$category == "rent"]), 1000)
    expect_equal(sum(jan$total[jan$category == "staff"]), 500)

    feb <- dplyr::filter(d, period_start == as.Date("2025-02-01"))
    expect_equal(sum(feb$total[feb$category == "staff"]), 700)
  })
})

test_that("ovhd_table_caption reflects period-detail vs category-totals mode", {
  r <- make_r()
  r$overhead <- transactions_overhead()
  testServer(mod_summary_server, args = list(r = r), {
    session$setInputs(ovhd_by_cat = FALSE)
    html1 <- as.character(output$ovhd_table_caption$html)
    expect_true(grepl("Period detail", html1, fixed = TRUE))

    session$setInputs(
      ovhd_by_cat = TRUE,
      ovhd_chart_type = "pie",
      ovhd_pie_scope = "full"
    )
    html2 <- as.character(output$ovhd_table_caption$html)
    expect_true(grepl("Category totals \\(full range\\)", html2))

    session$setInputs(
      ovhd_pie_scope = "period",
      ovhd_pie_period = "2025-01-01"
    )
    html3 <- as.character(output$ovhd_table_caption$html)
    expect_true(grepl("Category totals", html3, fixed = TRUE))
    expect_true(grepl("Jan 2025", html3, fixed = TRUE))
  })
})
