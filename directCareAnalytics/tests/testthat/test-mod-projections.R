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
    expect_true(is.na(fee_per_period()))
  })
})

test_that("fee_per_period returns NA when membership_fee is 0", {
  r <- make_monthly_r()
  testServer(mod_projections_server, args = list(r = r), {
    session$setInputs(proj_tier_members_1 = 10, proj_tier_fee_1 = 0)
    expect_true(is.na(fee_per_period()))
  })
})

test_that("fee_per_period returns fee unchanged for monthly data", {
  r <- make_monthly_r()
  testServer(mod_projections_server, args = list(r = r), {
    session$setInputs(proj_tier_members_1 = 10, proj_tier_fee_1 = 100)
    expect_equal(fee_per_period(), 100)
  })
})

test_that("fee_per_period divides by 4.33 for weekly data", {
  r <- make_weekly_r()
  testServer(mod_projections_server, args = list(r = r), {
    session$setInputs(proj_tier_members_1 = 10, proj_tier_fee_1 = 100)
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
    session$setInputs(proj_tier_members_1 = 50, proj_tier_fee_1 = 0)
    expect_false(profile_ok())

    session$setInputs(proj_tier_members_1 = 0, proj_tier_fee_1 = 100)
    expect_false(profile_ok())
  })
})

test_that("profile_ok is TRUE when both inputs are positive", {
  r <- make_monthly_r()
  testServer(mod_projections_server, args = list(r = r), {
    session$setInputs(proj_tier_members_1 = 50, proj_tier_fee_1 = 100)
    expect_true(profile_ok())
  })
})

# ── r$panel_size / r$membership_fee synced from inputs ────────────────────────

test_that("positive panel_size and membership_fee are synced into r", {
  r <- make_monthly_r()
  testServer(mod_projections_server, args = list(r = r), {
    session$setInputs(proj_tier_members_1 = 60, proj_tier_fee_1 = 89)
    expect_equal(r$panel_size, 60)
    expect_equal(r$membership_fee, 89)
  })
})

test_that("zero or NA inputs clear r$panel_size and r$membership_fee", {
  r <- make_monthly_r()
  testServer(mod_projections_server, args = list(r = r), {
    session$setInputs(proj_tier_members_1 = 0, proj_tier_fee_1 = NA)
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

# ── Partial-report download (.safe_result / download_ui) ───────────────────
# Regression tests for a real bug: target_result() (and, before the user
# switches to that sub-tab and re-runs, revenue/breakeven the same way) is
# an eventReactive() gated on input$btn_run. Before it has ever fired,
# calling it throws a shiny.silent.error -- and is.null(x) does NOT catch
# that, it just propagates. The old download handler called
# is.null(adj_target()) directly, so downloading a report after running
# only Break-even/Revenue (never switching to the Income Target sub-tab)
# crashed the downloadHandler's content function, which Shiny then served
# as an HTML error page instead of the PDF.

test_that(".safe_result resolves an un-run forecast to NULL instead of throwing", {
  r <- make_monthly_r(6)
  testServer(mod_projections_server, args = list(r = r), {
    session$setInputs(
      method = "linear",
      horizon = 3,
      confidence = 0.95,
      forecast_type = "breakeven"
    )
    session$setInputs(btn_run = 1)
    wait_for_task(forecast_task)

    expect_false(is.null(.safe_result(adj_breakeven())))
    expect_false(is.null(.safe_result(adj_revenue())))
    # Never switched to the Income Target sub-tab, so target_result() was
    # never triggered.
    expect_true(is.null(.safe_result(adj_target())))
  })
})

test_that("download_ui offers the button and lists missing sections for a partial report", {
  r <- make_monthly_r(6)
  testServer(mod_projections_server, args = list(r = r), {
    session$setInputs(
      method = "linear",
      horizon = 3,
      confidence = 0.95,
      forecast_type = "breakeven"
    )
    session$setInputs(btn_run = 1)
    wait_for_task(forecast_task)

    html <- as.character(output$download_ui$html)
    expect_true(grepl("Download Report", html, fixed = TRUE))
    expect_true(grepl("will not include", html, fixed = TRUE))
    expect_true(grepl("Income Target", html, fixed = TRUE))
  })
})

test_that("download_ui shows no missing-sections note once all three forecasts have run", {
  r <- make_monthly_r(6)
  testServer(mod_projections_server, args = list(r = r), {
    session$setInputs(
      method = "linear",
      horizon = 3,
      confidence = 0.95,
      target_income = 5000,
      forecast_type = "target"
    )
    session$setInputs(btn_run = 1)
    wait_for_task(forecast_task)

    expect_false(is.null(.safe_result(adj_breakeven())))
    expect_false(is.null(.safe_result(adj_revenue())))
    expect_false(is.null(.safe_result(adj_target())))

    html <- as.character(output$download_ui$html)
    expect_true(grepl("Download Report", html, fixed = TRUE))
    expect_false(grepl("will not include", html, fixed = TRUE))
  })
})

test_that("dl_report passes goal_seek/stress_test and badge = FALSE into interpret_breakeven()/interpret_target()", {
  # Regression coverage for the PDF report feature gap: before this, the
  # download handler never passed goal_seek/stress_test at all, so Pro
  # subscribers never saw that content in their report even though the
  # live UI already computed it for them (breakeven_goal_seek() etc. are
  # already Pro-gated internally, returning NULL for non-Pro, so reusing
  # them here needs no separate tier check). badge = FALSE because a "Pro"
  # badge makes no sense inside a document the paying user already owns.
  r <- make_monthly_r(6)
  r$plan_tier <- "pro"
  r$practice_name <- "Test Practice"

  captured <- new.env()

  testServer(mod_projections_server, args = list(r = r), {
    session$setInputs(
      method = "linear",
      horizon = 3,
      confidence = 0.95,
      target_income = 5000,
      forecast_type = "target"
    )
    session$setInputs(btn_run = 1)
    wait_for_task(forecast_task)

    local_mocked_bindings(
      interpret_breakeven = function(...) {
        captured$bkevn_args <- list(...)
        "<p>mock</p>"
      },
      interpret_target = function(...) {
        captured$tgt_args <- list(...)
        "<p>mock</p>"
      },
      render_report_pdf = function(data, file, ...) {
        writeLines("mock pdf", file)
        invisible(file)
      },
      .package = "directCareAnalytics"
    )

    output$dl_report
  })

  expect_identical(captured$bkevn_args$badge, FALSE)
  expect_true("goal_seek" %in% names(captured$bkevn_args))
  expect_true("stress_test" %in% names(captured$bkevn_args))

  expect_identical(captured$tgt_args$badge, FALSE)
  expect_true("goal_seek" %in% names(captured$tgt_args))
})
