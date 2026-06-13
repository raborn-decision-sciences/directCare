# Unit tests for mod_projections_server (via testServer)

# ── Fixture helpers ────────────────────────────────────────────────────────────

make_monthly_r <- function(n = 6) {
  shiny::reactiveValues(
    panel_size = NULL,
    membership_fee = NULL,
    overhead_monthly = tibble::tibble(
      practice_id = rep("test", n),
      year = rep(2025L, n),
      month = seq_len(n),
      total_overhead = rep(2000, n),
      gross_overhead = rep(2000, n),
      total_refunds = rep(0, n)
    ),
    income_monthly = tibble::tibble(
      practice_id = rep("test", n),
      year = rep(2025L, n),
      month = seq_len(n),
      total_revenue = seq(1000, by = 200, length.out = n)
    )
  )
}

make_weekly_r <- function(n = 12) {
  weeks <- seq(as.Date("2025-01-06"), by = "week", length.out = n)
  shiny::reactiveValues(
    panel_size = NULL,
    membership_fee = NULL,
    overhead_monthly = tibble::tibble(
      practice_id = rep("test", n),
      week_start = weeks,
      total_overhead = rep(500, n),
      gross_overhead = rep(500, n),
      total_refunds = rep(0, n)
    ),
    income_monthly = tibble::tibble(
      practice_id = rep("test", n),
      week_start = weeks,
      total_revenue = seq(400, by = 30, length.out = n)
    )
  )
}

# ── is_weekly ──────────────────────────────────────────────────────────────────

test_that("is_weekly is FALSE for monthly overhead", {
  r <- make_monthly_r()
  testServer(mod_projections_server, args = list(r = r), {
    expect_false(is_weekly())
  })
})

test_that("is_weekly is TRUE for weekly overhead", {
  r <- make_weekly_r()
  testServer(mod_projections_server, args = list(r = r), {
    expect_true(is_weekly())
  })
})

test_that("is_weekly is FALSE when overhead_monthly is NULL", {
  r <- shiny::reactiveValues(
    panel_size = NULL,
    membership_fee = NULL,
    overhead_monthly = NULL,
    income_monthly = NULL
  )
  testServer(mod_projections_server, args = list(r = r), {
    expect_false(is_weekly())
  })
})

# ── fee_per_period ─────────────────────────────────────────────────────────────

test_that("fee_per_period returns NA when membership_fee is not set", {
  r <- make_monthly_r()
  testServer(mod_projections_server, args = list(r = r), {
    session$setInputs(membership_fee = NULL)
    expect_true(is.na(fee_per_period()))
  })
})

test_that("fee_per_period returns NA when membership_fee is 0", {
  r <- make_monthly_r()
  testServer(mod_projections_server, args = list(r = r), {
    session$setInputs(membership_fee = 0)
    expect_true(is.na(fee_per_period()))
  })
})

test_that("fee_per_period returns fee unchanged for monthly data", {
  r <- make_monthly_r()
  testServer(mod_projections_server, args = list(r = r), {
    session$setInputs(membership_fee = 100)
    expect_equal(fee_per_period(), 100)
  })
})

test_that("fee_per_period divides by 4.33 for weekly data", {
  r <- make_weekly_r()
  testServer(mod_projections_server, args = list(r = r), {
    session$setInputs(membership_fee = 100)
    expect_equal(fee_per_period(), 100 / 4.33)
  })
})

# ── profile_ok ─────────────────────────────────────────────────────────────────

test_that("profile_ok is FALSE when inputs are absent", {
  r <- make_monthly_r()
  testServer(mod_projections_server, args = list(r = r), {
    expect_false(profile_ok())
  })
})

test_that("profile_ok is FALSE when either input is 0 or NA", {
  r <- make_monthly_r()
  testServer(mod_projections_server, args = list(r = r), {
    session$setInputs(panel_size = 50, membership_fee = 0)
    expect_false(profile_ok())

    session$setInputs(panel_size = 0, membership_fee = 100)
    expect_false(profile_ok())
  })
})

test_that("profile_ok is TRUE when both inputs are positive", {
  r <- make_monthly_r()
  testServer(mod_projections_server, args = list(r = r), {
    session$setInputs(panel_size = 50, membership_fee = 100)
    expect_true(profile_ok())
  })
})

# ── r$panel_size / r$membership_fee synced from inputs ────────────────────────

test_that("positive panel_size and membership_fee are synced into r", {
  r <- make_monthly_r()
  testServer(mod_projections_server, args = list(r = r), {
    session$setInputs(panel_size = 60, membership_fee = 89)
    expect_equal(r$panel_size, 60)
    expect_equal(r$membership_fee, 89)
  })
})

test_that("zero or NA inputs clear r$panel_size and r$membership_fee", {
  r <- make_monthly_r()
  testServer(mod_projections_server, args = list(r = r), {
    session$setInputs(panel_size = 0, membership_fee = NA)
    expect_null(r$panel_size)
    expect_null(r$membership_fee)
  })
})

# ── income_summary fallback ────────────────────────────────────────────────────

test_that("income_summary uses r$income_monthly when available", {
  r <- make_monthly_r()
  testServer(mod_projections_server, args = list(r = r), {
    d <- income_summary()
    expect_equal(nrow(d), 6L)
    expect_true("total_revenue" %in% names(d))
    # values should match fixture, not the 0.8 proxy
    expect_equal(d$total_revenue, seq(1000, by = 200, length.out = 6))
  })
})

test_that("income_summary falls back to 80% of overhead when income is absent", {
  r <- make_monthly_r()
  r$income_monthly <- NULL
  testServer(mod_projections_server, args = list(r = r), {
    d <- income_summary()
    expect_equal(d$total_revenue, rep(2000 * 0.8, 6))
  })
})
