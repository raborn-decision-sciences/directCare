# Unit tests for build_report_data() workflow detection

make_r <- function(scenario_inputs = NULL, transactions = NULL) {
  n <- 6L
  shiny::reactiveValues(
    practice_id = "test",
    practice_name = "Test Practice",
    panel_size = 50,
    membership_fee = 100,
    transactions = transactions,
    overhead = NULL,
    income = NULL,
    scenario_inputs = scenario_inputs,
    validation = list(),
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
      total_revenue = rep(5000, n)
    )
  )
}

test_that("build_report_data sets workflow = 'scenario' when scenario_inputs is set", {
  shiny::testServer(
    function(input, output, session) {
      r <- make_r(
        scenario_inputs = list(
          est_rent = 1000,
          est_panel_size = 50,
          est_monthly_fee = 100,
          est_n_months = 6L,
          est_start_month = 1L,
          est_start_year = 2025L
        ),
        transactions = tibble::tibble(practice_id = character(0))
      )
      inputs <- list(confidence = 0.95)
      d <- build_report_data(r, inputs)
      expect_equal(d$workflow, "scenario")
    },
    {}
  )
})

test_that("build_report_data sets workflow = 'upload' for CSV upload (non-empty transactions)", {
  shiny::testServer(
    function(input, output, session) {
      r <- make_r(
        scenario_inputs = NULL,
        transactions = tibble::tibble(
          practice_id = "test",
          date = Sys.Date(),
          amount = 100
        )
      )
      inputs <- list(confidence = 0.95)
      d <- build_report_data(r, inputs)
      expect_equal(d$workflow, "upload")
    },
    {}
  )
})

test_that("build_report_data sets workflow = 'upload' for manual entry (0-row transactions, no scenario_inputs)", {
  shiny::testServer(
    function(input, output, session) {
      r <- make_r(
        scenario_inputs = NULL,
        transactions = tibble::tibble(practice_id = character(0))
      )
      inputs <- list(confidence = 0.95)
      d <- build_report_data(r, inputs)
      expect_equal(d$workflow, "upload")
    },
    {}
  )
})

# -- .forecast_table_bkevn() overhead CI columns --------------------------------

make_bkevn_fd <- function(with_overhead_ci = TRUE) {
  fd <- tibble::tibble(
    period_start = seq(as.Date("2026-01-01"), by = "month", length.out = 3),
    revenue_forecast = c(4000, 4200, 4400),
    revenue_lower = c(3800, 4000, 4200),
    revenue_upper = c(4200, 4400, 4600),
    overhead_forecast = rep(5000, 3)
  )
  if (with_overhead_ci) {
    fd$overhead_lower <- c(4900, 4900, 4900)
    fd$overhead_upper <- c(5100, 5100, 5100)
  }
  fd
}

test_that(".forecast_table_bkevn includes overhead CI columns when present in forecast_data", {
  result <- list(forecast_data = make_bkevn_fd(with_overhead_ci = TRUE))
  tbl <- .forecast_table_bkevn(result)
  expect_equal(tbl[[1]]$ovhd_lo_fmt, "$4,900.00")
  expect_equal(tbl[[1]]$ovhd_hi_fmt, "$5,100.00")
})

test_that(".forecast_table_bkevn falls back to em-dash when overhead CI is absent", {
  result <- list(forecast_data = make_bkevn_fd(with_overhead_ci = FALSE))
  tbl <- .forecast_table_bkevn(result)
  expect_equal(tbl[[1]]$ovhd_lo_fmt, "—")
  expect_equal(tbl[[1]]$ovhd_hi_fmt, "—")
})

test_that("build_report_data's breakeven block sets has_overhead_ci from forecast_data columns", {
  shiny::testServer(
    function(input, output, session) {
      r <- make_r(scenario_inputs = NULL, transactions = NULL)
      bkevn_with_ci <- list(
        breakeven_date = as.Date("2026-03-01"),
        periods_to_breakeven = 3L,
        current_surplus_deficit = -500,
        current_revenue = 4000,
        current_overhead = 5000,
        current_overhead_avg = 5000,
        overhead_avg_n = 3L,
        confidence_interval = c(lower = as.Date(NA), upper = as.Date(NA)),
        forecast_data = make_bkevn_fd(with_overhead_ci = TRUE),
        method = "linear",
        frequency = "monthly",
        data_warnings = NULL
      )
      d <- build_report_data(
        r,
        list(confidence = 0.95),
        breakeven_res = bkevn_with_ci,
        interpret_bkevn = "<p>test</p>"
      )
      expect_true(d$breakeven$has_overhead_ci)

      bkevn_no_ci <- bkevn_with_ci
      bkevn_no_ci$forecast_data <- make_bkevn_fd(with_overhead_ci = FALSE)
      d2 <- build_report_data(
        r,
        list(confidence = 0.95),
        breakeven_res = bkevn_no_ci,
        interpret_bkevn = "<p>test</p>"
      )
      expect_false(d2$breakeven$has_overhead_ci)
    },
    {}
  )
})

test_that("build_report_data's plan_tier_label reflects r$plan_tier", {
  shiny::testServer(
    function(input, output, session) {
      r <- make_r(scenario_inputs = NULL, transactions = NULL)
      inputs <- list(confidence = 0.95)

      r$plan_tier <- "pro"
      expect_equal(build_report_data(r, inputs)$plan_tier_label, "Pro")

      r$plan_tier <- "starter"
      expect_equal(build_report_data(r, inputs)$plan_tier_label, "Starter")

      r$plan_tier <- "free"
      expect_equal(build_report_data(r, inputs)$plan_tier_label, "Free")
    },
    {}
  )
})

test_that("build_report_data's plan_tier_label defaults to Free when r$plan_tier is unset", {
  shiny::testServer(
    function(input, output, session) {
      r <- make_r(scenario_inputs = NULL, transactions = NULL)
      inputs <- list(confidence = 0.95)

      expect_equal(build_report_data(r, inputs)$plan_tier_label, "Free")
    },
    {}
  )
})
