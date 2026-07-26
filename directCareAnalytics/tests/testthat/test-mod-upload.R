# Unit tests for mod_upload_server (via testServer)

# Sample CSV paths from the backend package
sample_overhead_csv <- system.file(
  "extdata",
  "sample_overhead.csv",
  package = "directCareForecastR"
)
sample_income_csv <- system.file(
  "extdata",
  "sample_income.csv",
  package = "directCareForecastR"
)
sample_transactions_csv <- system.file(
  "extdata",
  "sample_transactions.csv",
  package = "directCareForecastR"
)

# Minimal fileInput list that mimics what Shiny provides after the user picks a
# file — only `datapath` and `name` are used by the upload handlers.
make_file_input <- function(path) {
  list(datapath = path, name = basename(path))
}

# Shared empty-r factory. practice_id/practice_name are pre-seeded (not NULL)
# since app_server.R -- not this module -- is what sources them from
# res_auth now; this module only *reads* them (e.g. tagging ingested rows
# with practice_id).
empty_r <- function() {
  shiny::reactiveValues(
    practice_id = "test-practice",
    practice_name = "Test Practice",
    panel_size = NULL,
    membership_fee = NULL,
    transactions = NULL,
    overhead = NULL,
    income = NULL,
    overhead_monthly = NULL,
    income_monthly = NULL,
    validation = list()
  )
}

test_that("btn_use_real sets path_chosen to 'upload'", {
  r <- empty_r()
  testServer(mod_upload_server, args = list(r = r), {
    session$setInputs(btn_use_real = 1)
    expect_equal(path_chosen(), "upload")
  })
})

test_that("btn_back resets path_chosen to NULL", {
  r <- empty_r()
  testServer(mod_upload_server, args = list(r = r), {
    session$setInputs(btn_use_real = 1)
    expect_equal(path_chosen(), "upload")

    session$setInputs(btn_back = 1)
    expect_null(path_chosen())
  })
})

test_that("go_to_manual_entry initialises all r slots with 0-row tibbles", {
  r <- empty_r()
  testServer(mod_upload_server, args = list(r = r), {
    session$setInputs(btn_use_plan = 1)

    expect_s3_class(r$transactions, "data.frame")
    expect_s3_class(r$overhead, "data.frame")
    expect_s3_class(r$income, "data.frame")
    expect_equal(nrow(r$transactions), 0L)
    expect_equal(nrow(r$overhead), 0L)
    expect_equal(nrow(r$income), 0L)
    expect_equal(r$validation, list())
  })
})

test_that("go_to_manual_entry tibbles have correct column names", {
  r <- empty_r()
  testServer(mod_upload_server, args = list(r = r), {
    session$setInputs(btn_use_plan = 1)

    expect_true(all(
      c(
        "practice_id",
        "date",
        "week_start",
        "month",
        "year",
        "full_account_name",
        "account_name",
        "description",
        "amount",
        "category",
        "source"
      ) %in%
        names(r$transactions)
    ))
    expect_true("is_refund" %in% names(r$overhead))
    expect_true("revenue" %in% names(r$income))
    expect_false("amount" %in% names(r$income))
  })
})

test_that("btn_manual_entry shows the entry form without immediately modifying r", {
  r <- empty_r()
  testServer(mod_upload_server, args = list(r = r), {
    session$setInputs(btn_use_real = 1)
    session$setInputs(btn_manual_entry = 1)
    # r$transactions remains NULL until the user submits the manual entry form
    expect_null(r$transactions)
    expect_null(r$overhead_monthly)
  })
})

test_that("mod_manual_entry_server populates r on monthly submit", {
  r <- empty_r()
  testServer(mod_manual_entry_server, args = list(r = r), {
    session$setInputs(
      manual_freq = "monthly",
      manual_start_month = 1L,
      manual_start_year = 2024L,
      manual_n_periods = 3L,
      btn_gen_manual = 1
    )
    session$setInputs(btn_submit_manual = 1)

    expect_s3_class(r$overhead_monthly, "tbl_df")
    expect_equal(nrow(r$overhead_monthly), 3L)
    expect_true(all(
      c(
        "year",
        "month",
        "total_overhead",
        "gross_overhead",
        "total_refunds"
      ) %in%
        names(r$overhead_monthly)
    ))
    expect_s3_class(r$income_monthly, "tbl_df")
    expect_equal(nrow(r$income_monthly), 3L)
    expect_true("total_revenue" %in% names(r$income_monthly))
    # 0-row transactions tibble signals non-upload workflow
    expect_equal(nrow(r$transactions), 0L)
  })
})

test_that("mod_manual_entry_server populates r on weekly submit", {
  r <- empty_r()
  testServer(mod_manual_entry_server, args = list(r = r), {
    session$setInputs(
      manual_freq = "weekly",
      manual_start_date = as.Date("2024-01-01"),
      manual_n_periods = 4L,
      btn_gen_manual = 1
    )
    session$setInputs(btn_submit_manual = 1)

    expect_s3_class(r$overhead_monthly, "tbl_df")
    expect_equal(nrow(r$overhead_monthly), 4L)
    expect_true("week_start" %in% names(r$overhead_monthly))
    expect_false("month" %in% names(r$overhead_monthly))
    expect_s3_class(r$income_monthly, "tbl_df")
    expect_true("week_start" %in% names(r$income_monthly))
  })
})

# ---------------------------------------------------------------------------
# r$reset_signal (Start Over, triggered globally from app_server)
# ---------------------------------------------------------------------------

test_that("r$reset_signal clears path_chosen and loaded flags", {
  r <- empty_r()
  r$reset_signal <- 0L
  testServer(mod_upload_server, args = list(r = r), {
    session$setInputs(btn_use_real = 1)

    r$reset_signal <- r$reset_signal + 1L
    session$flushReact()

    expect_null(path_chosen())
    expect_false(loaded$overhead)
    expect_false(loaded$income)
  })
})

# ---------------------------------------------------------------------------
# Generic CSV — combined file (btn_upload with software = "other")
# ---------------------------------------------------------------------------

test_that("btn_upload with software=other populates r from a combined CSV", {
  r <- empty_r()
  testServer(mod_upload_server, args = list(r = r), {
    session$setInputs(btn_use_real = 1)
    session$setInputs(
      software = "other",
      file_arrangement = "combined",
      csv_file = make_file_input(sample_transactions_csv),
      col_date = "date",
      col_amount = "amount",
      col_type = "type",
      overhead_pattern = "expense",
      income_pattern = "income"
    )
    session$setInputs(btn_upload = 1)

    expect_s3_class(r$overhead_monthly, "tbl_df")
    expect_s3_class(r$income_monthly, "tbl_df")
    expect_true(nrow(r$overhead_monthly) > 0L)
    expect_true(nrow(r$income_monthly) > 0L)
    expect_true(loaded$overhead)
    expect_true(loaded$income)
  })
})

test_that("btn_upload with software=other initialises a 0-row transactions tibble", {
  r <- empty_r()
  testServer(mod_upload_server, args = list(r = r), {
    session$setInputs(btn_use_real = 1)
    session$setInputs(
      software = "other",
      file_arrangement = "combined",
      csv_file = make_file_input(sample_transactions_csv),
      col_date = "date",
      col_amount = "amount",
      col_type = "type",
      overhead_pattern = "expense",
      income_pattern = "income"
    )
    session$setInputs(btn_upload = 1)

    expect_s3_class(r$transactions, "data.frame")
    expect_equal(nrow(r$transactions), 0L)
  })
})

test_that("btn_upload with a bad file leaves r unchanged and does not throw", {
  r <- empty_r()
  tmp <- tempfile(fileext = ".csv")
  writeLines("not,a,valid,csv\nfor,this,schema", tmp)
  on.exit(unlink(tmp))

  testServer(mod_upload_server, args = list(r = r), {
    session$setInputs(btn_use_real = 1)
    session$setInputs(
      software = "other",
      file_arrangement = "combined",
      csv_file = make_file_input(tmp),
      col_date = "date",
      col_amount = "amount",
      col_type = "type",
      overhead_pattern = "expense",
      income_pattern = "income"
    )
    expect_no_error(session$setInputs(btn_upload = 1))

    expect_null(r$overhead_monthly)
    expect_null(r$income_monthly)
    expect_false(loaded$overhead)
    expect_false(loaded$income)
  })
})

# ---------------------------------------------------------------------------
# Separate file loading — btn_load_overhead
# ---------------------------------------------------------------------------

test_that("btn_load_overhead populates r$overhead_monthly and sets loaded$overhead", {
  r <- empty_r()
  testServer(mod_upload_server, args = list(r = r), {
    session$setInputs(btn_use_real = 1)
    session$setInputs(
      software = "other",
      file_arrangement = "separate",
      overhead_file = make_file_input(sample_overhead_csv),
      ovhd_col_date = "date",
      ovhd_col_amount = "amount"
    )
    session$setInputs(btn_load_overhead = 1)

    expect_s3_class(r$overhead_monthly, "tbl_df")
    expect_true(nrow(r$overhead_monthly) > 0L)
    expect_true(loaded$overhead)
    expect_false(loaded$income)
  })
})

test_that("btn_load_overhead does not touch r$income_monthly", {
  r <- empty_r()
  testServer(mod_upload_server, args = list(r = r), {
    session$setInputs(btn_use_real = 1)
    session$setInputs(
      software = "other",
      file_arrangement = "separate",
      overhead_file = make_file_input(sample_overhead_csv),
      ovhd_col_date = "date",
      ovhd_col_amount = "amount"
    )
    session$setInputs(btn_load_overhead = 1)

    expect_null(r$income_monthly)
  })
})

test_that("btn_load_overhead initialises a 0-row transactions tibble", {
  r <- empty_r()
  testServer(mod_upload_server, args = list(r = r), {
    session$setInputs(btn_use_real = 1)
    session$setInputs(
      software = "other",
      file_arrangement = "separate",
      overhead_file = make_file_input(sample_overhead_csv),
      ovhd_col_date = "date",
      ovhd_col_amount = "amount"
    )
    session$setInputs(btn_load_overhead = 1)

    expect_s3_class(r$transactions, "data.frame")
    expect_equal(nrow(r$transactions), 0L)
  })
})

test_that("btn_load_overhead with a bad file leaves r unchanged and does not throw", {
  r <- empty_r()
  tmp <- tempfile(fileext = ".csv")
  writeLines("x,y\n1,2", tmp)
  on.exit(unlink(tmp))

  testServer(mod_upload_server, args = list(r = r), {
    session$setInputs(btn_use_real = 1)
    session$setInputs(
      software = "other",
      file_arrangement = "separate",
      overhead_file = make_file_input(tmp),
      ovhd_col_date = "date",
      ovhd_col_amount = "amount"
    )
    expect_no_error(session$setInputs(btn_load_overhead = 1))

    expect_null(r$overhead_monthly)
    expect_false(loaded$overhead)
  })
})

# ---------------------------------------------------------------------------
# Separate file loading — btn_load_income
# ---------------------------------------------------------------------------

test_that("btn_load_income populates r$income_monthly and sets loaded$income", {
  r <- empty_r()
  testServer(mod_upload_server, args = list(r = r), {
    session$setInputs(btn_use_real = 1)
    session$setInputs(
      software = "other",
      file_arrangement = "separate",
      income_file = make_file_input(sample_income_csv),
      inc_col_date = "date",
      inc_col_amount = "amount"
    )
    session$setInputs(btn_load_income = 1)

    expect_s3_class(r$income_monthly, "tbl_df")
    expect_true(nrow(r$income_monthly) > 0L)
    expect_true(loaded$income)
    expect_false(loaded$overhead)
  })
})

test_that("btn_load_income does not touch r$overhead_monthly", {
  r <- empty_r()
  testServer(mod_upload_server, args = list(r = r), {
    session$setInputs(btn_use_real = 1)
    session$setInputs(
      software = "other",
      file_arrangement = "separate",
      income_file = make_file_input(sample_income_csv),
      inc_col_date = "date",
      inc_col_amount = "amount"
    )
    session$setInputs(btn_load_income = 1)

    expect_null(r$overhead_monthly)
  })
})

test_that("btn_load_income initialises a 0-row transactions tibble", {
  r <- empty_r()
  testServer(mod_upload_server, args = list(r = r), {
    session$setInputs(btn_use_real = 1)
    session$setInputs(
      software = "other",
      file_arrangement = "separate",
      income_file = make_file_input(sample_income_csv),
      inc_col_date = "date",
      inc_col_amount = "amount"
    )
    session$setInputs(btn_load_income = 1)

    expect_s3_class(r$transactions, "data.frame")
    expect_equal(nrow(r$transactions), 0L)
  })
})

test_that("btn_load_income with a bad file leaves r unchanged and does not throw", {
  r <- empty_r()
  tmp <- tempfile(fileext = ".csv")
  writeLines("x,y\n1,2", tmp)
  on.exit(unlink(tmp))

  testServer(mod_upload_server, args = list(r = r), {
    session$setInputs(btn_use_real = 1)
    session$setInputs(
      software = "other",
      file_arrangement = "separate",
      income_file = make_file_input(tmp),
      inc_col_date = "date",
      inc_col_amount = "amount"
    )
    expect_no_error(session$setInputs(btn_load_income = 1))

    expect_null(r$income_monthly)
    expect_false(loaded$income)
  })
})

# ---------------------------------------------------------------------------
# Separate-file workflow: loading both files independently
# ---------------------------------------------------------------------------

test_that("loading overhead then income yields both summaries without either overwriting the other", {
  r <- empty_r()
  testServer(mod_upload_server, args = list(r = r), {
    session$setInputs(btn_use_real = 1)
    session$setInputs(
      software = "other",
      file_arrangement = "separate",
      overhead_file = make_file_input(sample_overhead_csv),
      ovhd_col_date = "date",
      ovhd_col_amount = "amount"
    )
    session$setInputs(btn_load_overhead = 1)

    overhead_rows <- nrow(r$overhead_monthly)

    session$setInputs(
      income_file = make_file_input(sample_income_csv),
      inc_col_date = "date",
      inc_col_amount = "amount"
    )
    session$setInputs(btn_load_income = 1)

    expect_equal(nrow(r$overhead_monthly), overhead_rows)
    expect_true(nrow(r$income_monthly) > 0L)
    expect_true(loaded$overhead)
    expect_true(loaded$income)
  })
})

test_that("init_generic_state does not overwrite an already-initialised transactions tibble", {
  r <- empty_r()
  testServer(mod_upload_server, args = list(r = r), {
    session$setInputs(btn_use_real = 1)

    # Load overhead first — triggers init_generic_state, sets r$transactions
    session$setInputs(
      software = "other",
      file_arrangement = "separate",
      overhead_file = make_file_input(sample_overhead_csv),
      ovhd_col_date = "date",
      ovhd_col_amount = "amount"
    )
    session$setInputs(btn_load_overhead = 1)
    first_ptr <- r$transactions

    # Load income second — should not replace r$transactions
    session$setInputs(
      income_file = make_file_input(sample_income_csv),
      inc_col_date = "date",
      inc_col_amount = "amount"
    )
    session$setInputs(btn_load_income = 1)

    expect_identical(r$transactions, first_ptr)
  })
})
