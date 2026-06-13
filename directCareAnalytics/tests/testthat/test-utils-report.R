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
