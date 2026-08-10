# Unit tests for mod_results_server (via testServer)

# Plain list (not wrapped in reactiveValues) so it can also be used
# directly by pure-function tests (e.g. .market_context_rows()) without
# tripping Shiny's "reactive value accessed outside a reactive consumer"
# guard.
fixture_market_context <- function() {
  list(
    geography = list(county_name = "Fulton County", state_abb = "GA", metro_fips = "12060"),
    population_income = list(population = 1000000L, median_household_income = 75000),
    uninsured = list(uninsured_rate = 0.1),
    physician_density = list(physician_density_per_10k = 40),
    landscape = data.frame(county_fips = character(0))
  )
}

fixture_populated_r <- function() {
  revenue <- directCarePlanR::calc_mixed_revenue(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 3),
    horizon_months = 3
  )
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 3),
    overhead_monthly = 5000
  )
  projections <- directCarePlanR::project_scenarios(assumptions, horizon_months = 3)
  startup_costs <- directCarePlanR::calc_startup_costs(c(equipment = 5000, ehr_setup = 8000))
  personal_runway <- directCarePlanR::calc_personal_runway(monthly_expenses = 4000, months_coverage = 6)

  reactiveValues(
    practice_name = "Test Practice",
    # Represents a normal, fully-functional practice for tests that
    # aren't specifically exercising the paywall gate itself (see
    # "gates Market Context and Download Report for a free-tier
    # practice" below for that case).
    plan_tier = "pro",
    horizon_months = 3,
    market_context = fixture_market_context(),
    revenue = revenue,
    projections = projections,
    capital = list(startup_costs = startup_costs, personal_runway = personal_runway),
    interpretations = list(
      revenue = directCarePlanR::interpret_revenue(revenue),
      projection = directCarePlanR::interpret_projection(projections),
      capital = directCarePlanR::interpret_capital(startup_costs, personal_runway)
    )
  )
}

test_that("shows an empty-state prompt when no plan has been built", {
  r <- reactiveValues(projections = NULL)

  testServer(mod_results_server, args = list(r = r), {
    html <- paste(as.character(output$content), collapse = "")
    expect_true(grepl("Build a plan", html))
  })
})

test_that("renders results content once a plan has been built", {
  r <- fixture_populated_r()

  testServer(mod_results_server, args = list(r = r), {
    html <- paste(as.character(output$content), collapse = "")
    expect_true(grepl("Market Context", html))
    expect_true(grepl("Fulton County", html))
    expect_true(grepl("Scenario Projections", html))
    expect_true(grepl("Capital Requirements", html))
    expect_true(grepl("Interpretation", html))
  })
})

test_that("gates Market Context and Download Report for a free-tier practice", {
  r <- fixture_populated_r()
  r$plan_tier <- "free"

  testServer(mod_results_server, args = list(r = r), {
    html <- paste(as.character(output$content), collapse = "")
    expect_false(grepl("Fulton County", html))
    expect_true(grepl("Starter or Pro plan required", html))

    footer_html <- paste(as.character(nav_footer()), collapse = "")
    # "Unlock Download Report" itself contains "Download Report", so check
    # the actual element rendered rather than that substring: the real
    # download link's id ("...-dl_report") should be absent, and the
    # gated trigger's should be present.
    expect_false(grepl("dl_report", footer_html, fixed = TRUE))
    expect_true(grepl("btn_see_plans_report", footer_html, fixed = TRUE))
    # Same swap for the Save/Load scenario-slots widget.
    expect_false(grepl("scenario-save_click", footer_html, fixed = TRUE))
    expect_true(grepl("btn_see_plans_scenario", footer_html, fixed = TRUE))
  })
})

# -- Location comparison (Pro) -------------------------------------------

fixture_compare_context <- function() {
  list(
    geography = list(county_name = "San Francisco County", state_abb = "CA", metro_fips = "41860"),
    population_income = list(population = 800000L, median_household_income = 120000),
    uninsured = list(uninsured_rate = 0.05),
    physician_density = list(physician_density_per_10k = 60),
    landscape = data.frame(county_fips = character(0))
  )
}

test_that("renders nothing for the comparison section when no compare ZIPs were entered", {
  r <- fixture_populated_r()

  testServer(mod_results_server, args = list(r = r), {
    html <- paste(as.character(output$content), collapse = "")
    expect_false(grepl("Location Comparison", html))
  })
})

test_that("Pro tier with computed compare contexts renders the comparison table", {
  r <- fixture_populated_r()
  r$market_context_compare_requested <- 1L
  r$market_context_compare <- list(fixture_compare_context())

  testServer(mod_results_server, args = list(r = r), {
    html <- paste(as.character(output$content), collapse = "")
    expect_true(grepl("Location Comparison", html))
    expect_true(grepl("Fulton County", html))
    expect_true(grepl("San Francisco County", html))
    expect_false(grepl("See Pro plan", html))
  })
})

test_that("non-Pro tier with a requested comparison shows the upsell teaser, not the table", {
  r <- fixture_populated_r()
  r$plan_tier <- "starter"
  r$market_context_compare_requested <- 1L
  r$market_context_compare <- NULL

  testServer(mod_results_server, args = list(r = r), {
    html <- paste(as.character(output$content), collapse = "")
    expect_true(grepl("Location Comparison", html))
    expect_false(grepl("San Francisco County", html))
    expect_true(grepl("See Pro plan", html))
    expect_true(grepl("btn_see_plans_pro_location_compare", html, fixed = TRUE))
  })
})

test_that(".market_context_rows() returns the 6 expected fields in order", {
  rows <- .market_context_rows(fixture_market_context())

  expect_equal(
    vapply(rows, function(x) x$label, character(1)),
    c(
      "Location", "Population", "Median household income",
      "Uninsured rate", "Physician density", "Known nearby direct care practices"
    )
  )
  expect_equal(rows[[1]]$value, "Fulton County, GA")
  expect_equal(rows[[2]]$value, "1,000,000")
})

test_that("btn_generate_report passes r$plan_tier into directCarePlanR::build_report_data()", {
  # build_report_data() still runs in the main process (inside the
  # btn_generate_report invoke observer), so mocking it here still works --
  # unlike render_plan_report(), which now runs inside report_task's mirai
  # worker and can no longer be intercepted by local_mocked_bindings() (a
  # mirai worker loads the real installed package fresh in a separate
  # process). See the "Generate Report..." test below for real, unmocked
  # coverage of that call.
  r <- fixture_populated_r()
  r$plan_tier <- "pro"

  captured <- new.env()

  # Mocking doesn't stop this test's btn_generate_report click from firing a
  # real, unwaited-for report_task$invoke() in the background (the mock only
  # intercepts build_report_data() in *this* process; render_plan_report()
  # still runs for real inside the worker) -- so the mock's return value
  # still needs to be something Typst can actually compile. An empty list()
  # was found to leave a stray Typst error running in the background that
  # corrupted a *later* test's real compile in the same session; calling
  # through to build_report_data()'s own all-defaults case (confirmed to
  # compile cleanly) avoids that while still capturing the real args passed
  # in for the assertion below.
  real_build_report_data <- directCarePlanR::build_report_data

  testServer(mod_results_server, args = list(r = r), {
    local_mocked_bindings(
      build_report_data = function(...) {
        captured$args <- list(...)
        real_build_report_data()
      },
      .package = "directCarePlanR"
    )

    session$setInputs(btn_generate_report = 1)
    wait_for_task(report_task)
  })

  expect_identical(captured$args$plan_tier, "pro")
})

test_that("Generate Report resolves to a real file and reveals the download button", {
  # End-to-end coverage of the two-step/three-state async flow (report_task):
  # click Generate Report, wait for the background mirai worker to finish,
  # confirm report_path() now points at a real PDF-ish file, that nav_footer()
  # now renders the real download button, and that dl_report's now-trivial
  # content() (file.copy() from report_path()) actually produces output.
  # Unlike directCareAnalytics's own render_report_pdf() (which lives in the
  # app package itself and is only dev-loaded, not installed, under
  # devtools::test()), render_plan_report() is exported from directCarePlanR,
  # a genuinely installed dependency package -- so this runs for real here,
  # no install-time skip needed. It does still need an actual typst
  # toolchain (CLI binary or the typr R fallback) to compile the PDF,
  # which directCarePlanner.yaml's CI doesn't install (unlike
  # directCarePlanR.yaml's own suite, which does) -- skip rather than fail
  # there, matching directCareAnalytics/tests/testthat/test-utils-report.R's
  # identical guard for its own real-typst-compile test.
  skip_if(
    nzchar(Sys.which("typst")) == FALSE && !requireNamespace("typr", quietly = TRUE),
    "no typst toolchain available"
  )

  r <- fixture_populated_r()
  r$plan_tier <- "pro"

  testServer(mod_results_server, args = list(r = r), {
    session$setInputs(btn_generate_report = 1)
    wait_for_task(report_task)

    expect_false(is.null(report_path()))
    expect_true(file.exists(report_path()))

    footer_html <- paste(as.character(nav_footer()), collapse = "")
    expect_true(grepl("dl_report", footer_html, fixed = TRUE))

    expect_no_error(force(output$dl_report))
  })
})

test_that("nav_footer shows Generate Report (not Download) before a report has been generated", {
  r <- fixture_populated_r()
  r$plan_tier <- "pro"

  testServer(mod_results_server, args = list(r = r), {
    footer_html <- paste(as.character(nav_footer()), collapse = "")
    expect_true(grepl("btn_generate_report", footer_html, fixed = TRUE))
    expect_false(grepl("dl_report", footer_html, fixed = TRUE))
  })
})

test_that(".paragraphs_to_html() splits on blank lines and wraps each in <p>", {
  html <- paste(as.character(.paragraphs_to_html("First paragraph.\n\nSecond paragraph.")), collapse = "")
  expect_true(grepl("<p>First paragraph.</p>", html, fixed = TRUE))
  expect_true(grepl("<p>Second paragraph.</p>", html, fixed = TRUE))
})

test_that(".paragraphs_to_html() returns NULL for empty input", {
  expect_null(.paragraphs_to_html(NULL))
  expect_null(.paragraphs_to_html(""))
})

test_that(".paragraphs_to_html() badges paragraphs flagged by the pro_paragraph attribute", {
  text <- "First paragraph.\n\nSecond paragraph."
  attr(text, "pro_paragraph") <- c(FALSE, TRUE)

  html <- paste(as.character(.paragraphs_to_html(text)), collapse = "")

  expect_true(grepl("<p>First paragraph.</p>", html, fixed = TRUE))
  expect_false(grepl("badge.*First paragraph", html))
  expect_true(grepl("badge", html))
  expect_true(grepl("Second paragraph", html))
})

test_that(".paragraphs_to_html() badges nothing when the pro_paragraph attribute is absent", {
  html <- paste(as.character(.paragraphs_to_html("Plain paragraph.")), collapse = "")

  expect_false(grepl("badge", html))
})

test_that(".paragraphs_to_html() renders a '\\n- ' paragraph as an intro <p> plus a real <ul>", {
  text <- "Intro sentence:\n- First item.\n- Second item."

  html <- paste(as.character(.paragraphs_to_html(text)), collapse = "")

  expect_true(grepl("<p>Intro sentence:</p>", html, fixed = TRUE))
  expect_true(grepl("<ul>", html, fixed = TRUE))
  expect_true(grepl("<li>First item.</li>", html, fixed = TRUE))
  expect_true(grepl("<li>Second item.</li>", html, fixed = TRUE))
  expect_false(grepl("- First item", html, fixed = TRUE))
})

test_that(".paragraphs_to_html() puts the Pro badge on the intro <p>, not inside the <ul>", {
  text <- "Intro sentence:\n- First item.\n- Second item."
  attr(text, "pro_paragraph") <- TRUE

  html <- paste(as.character(.paragraphs_to_html(text)), collapse = "")
  intro_p <- sub("<ul>.*", "", html)

  expect_true(grepl("badge", intro_p))
  expect_true(grepl("<li>First item.</li>", html, fixed = TRUE))
})

# ── .humanize_cost_items() (startup-cost category label overrides) ──────

test_that(".humanize_cost_items returns default labels with no overrides", {
  expect_equal(.humanize_cost_items("ehr_setup"), "EHR setup")
  expect_equal(.humanize_cost_items("equipment"), "Equipment")
})

test_that(".humanize_cost_items applies overrides and falls back for unmapped keys", {
  overrides <- c(ehr_setup = "Practice Software")
  expect_equal(.humanize_cost_items("ehr_setup", overrides), "Practice Software")
  expect_equal(.humanize_cost_items("equipment", overrides), "Equipment")
  expect_equal(.humanize_cost_items("unknown_slug", overrides), "unknown_slug")
})

