# -- build_report_data -------------------------------------------------------

fixture_market_context <- list(
  geography = list(county_name = "Fulton County", state_abb = "GA", metro_fips = "12060"),
  population_income = list(population = 1000000L, median_household_income = 75000),
  uninsured = list(uninsured_rate = 0.1),
  physician_density = list(physician_density_per_10k = 40),
  landscape = data.frame(county_fips = character(0))
)

test_that("build_report_data() returns NULL sections for omitted inputs", {
  data <- build_report_data()

  expect_named(
    data,
    c("report_date", "practice_name", "market", "revenue", "projections", "capital", "interpretations")
  )
  expect_equal(data$practice_name, "Untitled Practice")
  expect_null(data$market)
  expect_null(data$revenue)
  expect_null(data$projections)
  expect_null(data$capital)
  expect_null(data$interpretations)
})

test_that("build_report_data() derives practice_name from market_context when not supplied", {
  data <- build_report_data(market_context = fixture_market_context)
  expect_equal(data$practice_name, "Fulton County, GA Practice")
})

test_that("build_report_data() lets an explicit practice_name override the derived one", {
  data <- build_report_data(market_context = fixture_market_context, practice_name = "My Practice")
  expect_equal(data$practice_name, "My Practice")
})

test_that("build_report_data() assembles the revenue block correctly", {
  revenue <- calc_mixed_revenue(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 3),
    fee_args = list(visit_volume = 100, new_visit_fee = 200, follow_up_fee = 100),
    horizon_months = 3
  )
  data <- build_report_data(revenue = revenue)

  expect_equal(data$revenue$membership_final, 30000)
  expect_equal(data$revenue$fee_final, 12000)
  expect_equal(data$revenue$total_final, 42000)
  expect_equal(length(data$revenue$table), 3L)
  expect_equal(data$revenue$table[[1]]$total_revenue, 22000)
})

test_that("build_report_data() assembles the projections block with one entry per scenario", {
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 3),
    overhead_monthly = 5000
  )
  projections <- project_scenarios(assumptions, horizon_months = 3)
  data <- build_report_data(projections = projections)

  expect_equal(length(data$projections$scenarios), 3L)
  scenario_names <- vapply(data$projections$scenarios, function(x) x$scenario, character(1))
  expect_setequal(scenario_names, c("conservative", "base", "optimistic"))
  base <- data$projections$scenarios[[which(scenario_names == "base")]]
  expect_equal(length(base$table), 3L)
})

test_that("build_report_data() assembles the capital block and combines totals", {
  startup_costs <- calc_startup_costs(c(equipment = 5000, ehr_setup = 8000))
  personal_runway <- calc_personal_runway(monthly_expenses = 4000, months_coverage = 6)
  data <- build_report_data(capital = list(startup_costs = startup_costs, personal_runway = personal_runway))

  expect_equal(data$capital$startup_total, 13000)
  expect_equal(data$capital$runway_total, 24000)
  expect_equal(data$capital$combined_total, 37000)
})

test_that("build_report_data() splits interpretation text into paragraph arrays", {
  interpretations <- list(
    revenue = "Paragraph one.\n\nParagraph two.",
    capital = "Only paragraph."
  )
  data <- build_report_data(interpretations = interpretations)

  expect_equal(data$interpretations$revenue, c("Paragraph one.", "Paragraph two."))
  expect_equal(data$interpretations$capital, "Only paragraph.")
})

# -- render_plan_report (real Typst compile) ---------------------------------

test_that("render_plan_report() compiles a real, non-empty PDF from full report data", {
  skip_if(nzchar(Sys.which("typst")) == FALSE && !requireNamespace("typr", quietly = TRUE), "no typst toolchain available")

  market_context <- fixture_market_context
  revenue <- calc_mixed_revenue(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 3),
    fee_args = list(visit_volume = 100, new_visit_fee = 200, follow_up_fee = 100),
    horizon_months = 3
  )
  assumptions <- list(
    membership_args = list(panel_size = 300, fee = 100, ramp_months = 3),
    overhead_monthly = 5000
  )
  projections <- project_scenarios(assumptions, horizon_months = 3)
  startup_costs <- calc_startup_costs(c(equipment = 5000, ehr_setup = 8000))
  personal_runway <- calc_personal_runway(monthly_expenses = 4000, months_coverage = 6)

  data <- build_report_data(
    market_context = market_context,
    revenue = revenue,
    projections = projections,
    capital = list(startup_costs = startup_costs, personal_runway = personal_runway),
    interpretations = list(
      revenue = interpret_revenue(revenue),
      projection = interpret_projection(projections),
      capital = interpret_capital(startup_costs, personal_runway)
    )
  )

  out_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(out_file))
  result <- render_plan_report(data, out_file)

  expect_equal(result, out_file)
  expect_true(file.exists(out_file))
  expect_gt(file.size(out_file), 0)
})

test_that("render_plan_report() works with only a subset of sections supplied", {
  skip_if(nzchar(Sys.which("typst")) == FALSE && !requireNamespace("typr", quietly = TRUE), "no typst toolchain available")

  data <- build_report_data(
    revenue = calc_mixed_revenue(
      membership_args = list(panel_size = 100, fee = 50, ramp_months = 2),
      horizon_months = 2
    )
  )

  out_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(out_file))
  render_plan_report(data, out_file)
  expect_true(file.exists(out_file))
  expect_gt(file.size(out_file), 0)
})
