overhead_sample <- system.file("extdata", "sample_overhead.csv",
                               package = "directCareForecastR")
income_sample   <- system.file("extdata", "sample_income.csv",
                               package = "directCareForecastR")
txn_sample      <- system.file("extdata", "sample_transactions.csv",
                               package = "directCareForecastR")

# =============================================================================
# type = "both" (default) — mixed file
# =============================================================================

test_that("ingest_csv_generic reads the bundled mixed transactions file", {
  result <- ingest_csv_generic(
    path            = txn_sample,
    practice_id     = 1,
    col_date        = "date",
    col_amount      = "amount",
    col_type        = "type",
    col_category    = "category",
    col_description = "description"
  )
  expect_type(result, "list")
  expect_named(result, c("overhead", "income"))
})

test_that("ingest_csv_generic type='both' returns correct row counts", {
  result <- ingest_csv_generic(
    path         = txn_sample,
    practice_id  = 1,
    col_date     = "date",
    col_amount   = "amount",
    col_type     = "type",
    col_category = "category"
  )
  # sample_transactions.csv has 8 expense rows and 10 income rows
  expect_equal(nrow(result$overhead), 8)
  expect_equal(nrow(result$income),  10)
})

test_that("ingest_csv_generic type='both' overhead tibble has amount column", {
  result <- ingest_csv_generic(
    path        = txn_sample,
    practice_id = 1,
    col_date    = "date",
    col_amount  = "amount",
    col_type    = "type"
  )
  expect_true("amount" %in% names(result$overhead))
  expect_false("revenue" %in% names(result$overhead))
})

test_that("ingest_csv_generic type='both' income tibble has revenue column", {
  result <- ingest_csv_generic(
    path        = txn_sample,
    practice_id = 1,
    col_date    = "date",
    col_amount  = "amount",
    col_type    = "type"
  )
  expect_true("revenue" %in% names(result$income))
  expect_false("amount" %in% names(result$income))
})

test_that("ingest_csv_generic type='both' both tibbles have is_refund column", {
  result <- ingest_csv_generic(
    path        = txn_sample,
    practice_id = 1,
    col_date    = "date",
    col_amount  = "amount",
    col_type    = "type"
  )
  expect_true("is_refund" %in% names(result$overhead))
  expect_true("is_refund" %in% names(result$income))
})

test_that("ingest_csv_generic type='both' sets source = 'generic_csv' on both tibbles", {
  result <- ingest_csv_generic(
    path        = txn_sample,
    practice_id = 1,
    col_date    = "date",
    col_amount  = "amount",
    col_type    = "type"
  )
  expect_true(all(result$overhead$source == "generic_csv"))
  expect_true(all(result$income$source   == "generic_csv"))
})

test_that("ingest_csv_generic type='both' maps col_category for overhead rows", {
  result <- ingest_csv_generic(
    path         = txn_sample,
    practice_id  = 1,
    col_date     = "date",
    col_amount   = "amount",
    col_type     = "type",
    col_category = "category"
  )
  expect_true("rent" %in% result$overhead$category)
  expect_true("software" %in% result$overhead$category)
})

test_that("ingest_csv_generic type='both' income rows with blank category default to 'other'", {
  result <- ingest_csv_generic(
    path         = txn_sample,
    practice_id  = 1,
    col_date     = "date",
    col_amount   = "amount",
    col_type     = "type",
    col_category = "category"
  )
  expect_true(all(result$income$category == "other"))
})

test_that("ingest_csv_generic type='both' default patterns are case-insensitive", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      dt   = c("01/01/2025", "01/15/2025"),
      amt  = c(500, 200),
      kind = c("INCOME", "EXPENSE")
    ),
    tmp
  )
  result <- ingest_csv_generic(
    tmp, 1, "dt", "amt", col_type = "kind"
  )
  expect_equal(nrow(result$income),   1)
  expect_equal(nrow(result$overhead), 1)
  unlink(tmp)
})

test_that("ingest_csv_generic type='both' respects custom overhead_pattern and income_pattern", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      dt   = c("01/01/2025", "01/15/2025", "02/01/2025"),
      amt  = c(500, 1200, 300),
      kind = c("credit", "debit", "credit")
    ),
    tmp
  )
  result <- ingest_csv_generic(
    tmp, 1, "dt", "amt",
    col_type         = "kind",
    overhead_pattern = "debit",
    income_pattern   = "credit"
  )
  expect_equal(nrow(result$income),   2)
  expect_equal(nrow(result$overhead), 1)
  unlink(tmp)
})

test_that("ingest_csv_generic type='both' overhead takes priority when row matches both patterns", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      dt   = "01/01/2025",
      amt  = 100,
      kind = "expense income"  # matches both default patterns
    ),
    tmp
  )
  result <- ingest_csv_generic(
    tmp, 1, "dt", "amt", col_type = "kind"
  )
  expect_equal(nrow(result$overhead), 1)
  expect_equal(nrow(result$income),   0)
  unlink(tmp)
})

test_that("ingest_csv_generic type='both' warns on rows that match neither pattern", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      dt   = c("01/01/2025", "01/15/2025"),
      amt  = c(500, 200),
      kind = c("income", "transfer")  # "transfer" matches neither
    ),
    tmp
  )
  expect_warning(
    ingest_csv_generic(tmp, 1, "dt", "amt", col_type = "kind"),
    class = "dcForecastR_unclassified_rows"
  )
  unlink(tmp)
})

test_that("ingest_csv_generic type='both' unclassified warning attaches count and values", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      dt   = c("01/01/2025", "01/15/2025"),
      amt  = c(500, 200),
      kind = c("income", "transfer")
    ),
    tmp
  )
  w <- tryCatch(
    ingest_csv_generic(tmp, 1, "dt", "amt", col_type = "kind"),
    warning = \(w) w
  )
  expect_equal(w$n_unmatched, 1)
  expect_equal(w$unmatched_values, "transfer")
  unlink(tmp)
})

test_that("ingest_csv_generic type='both' drops unmatched rows silently after warning", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      dt   = c("01/01/2025", "01/15/2025", "02/01/2025"),
      amt  = c(150, 200, 999),
      kind = c("income", "expense", "transfer")
    ),
    tmp
  )
  result <- suppressWarnings(
    ingest_csv_generic(tmp, 1, "dt", "amt", col_type = "kind")
  )
  expect_equal(nrow(result$income),   1)
  expect_equal(nrow(result$overhead), 1)
  unlink(tmp)
})

# ── type = "both" errors ───────────────────────────────────────────────────────

test_that("ingest_csv_generic type='both' errors when col_type is NULL", {
  expect_snapshot(
    ingest_csv_generic(
      path        = txn_sample,
      practice_id = 1,
      col_date    = "date",
      col_amount  = "amount",
      type        = "both"
      # col_type intentionally omitted
    ),
    error = TRUE
  )
})

test_that("ingest_csv_generic type='both' error for missing col_type has dcForecastR_missing_columns class", {
  err <- tryCatch(
    ingest_csv_generic(
      path        = txn_sample,
      practice_id = 1,
      col_date    = "date",
      col_amount  = "amount",
      type        = "both"
    ),
    error = \(e) e
  )
  expect_true(inherits(err, "dcForecastR_missing_columns"))
})

test_that("ingest_csv_generic type='both' errors when col_type column does not exist", {
  expect_snapshot(
    ingest_csv_generic(
      path        = txn_sample,
      practice_id = 1,
      col_date    = "date",
      col_amount  = "amount",
      col_type    = "nonexistent_column"
    ),
    error = TRUE
  )
})

# ── type = "both" downstream integration ─────────────────────────────────────

test_that("ingest_csv_generic type='both' overhead flows into summarize_overhead_monthly", {
  result  <- ingest_csv_generic(
    path         = txn_sample,
    practice_id  = 1,
    col_date     = "date",
    col_amount   = "amount",
    col_type     = "type",
    col_category = "category"
  )
  summary <- summarize_overhead_monthly(result$overhead)

  expect_s3_class(summary, "data.frame")
  expect_equal(nrow(summary), 3)  # Jan, Feb, Mar 2025
  expect_true(all(summary$total_overhead > 0))
})

test_that("ingest_csv_generic type='both' income flows into summarize_income_monthly", {
  result  <- ingest_csv_generic(
    path        = txn_sample,
    practice_id = 1,
    col_date    = "date",
    col_amount  = "amount",
    col_type    = "type"
  )
  summary <- summarize_income_monthly(result$income)

  expect_s3_class(summary, "data.frame")
  expect_equal(nrow(summary), 3)  # Jan, Feb, Mar 2025
  expect_true(all(summary$total_revenue > 0))
})

# =============================================================================
# type = "overhead" — single-type file
# =============================================================================

test_that("ingest_csv_generic type='overhead' reads the bundled overhead sample file", {
  result <- ingest_csv_generic(
    path            = overhead_sample,
    practice_id     = 1,
    col_date        = "date",
    col_amount      = "amount",
    type            = "overhead",
    col_category    = "category",
    col_description = "description"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 10)
})

test_that("ingest_csv_generic type='overhead' output has correct columns", {
  result <- ingest_csv_generic(
    path        = overhead_sample,
    practice_id = 1,
    col_date    = "date",
    col_amount  = "amount",
    type        = "overhead"
  )
  expected <- c("practice_id", "date", "week_start", "month", "year",
                "full_account_name", "account_name", "description",
                "amount", "category", "source", "is_refund")
  expect_true(all(expected %in% names(result)))
})

test_that("ingest_csv_generic type='income' output has revenue column, not amount", {
  result <- ingest_csv_generic(
    path        = income_sample,
    practice_id = 1,
    col_date    = "date",
    col_amount  = "amount",
    type        = "income"
  )
  expect_true("revenue" %in% names(result))
  expect_false("amount" %in% names(result))
})

test_that("ingest_csv_generic sets source to 'generic_csv'", {
  result <- ingest_csv_generic(
    path        = overhead_sample,
    practice_id = 1,
    col_date    = "date",
    col_amount  = "amount",
    type        = "overhead"
  )
  expect_true(all(result$source == "generic_csv"))
})

test_that("ingest_csv_generic attaches practice_id to every row", {
  result <- ingest_csv_generic(
    path        = overhead_sample,
    practice_id = "p42",
    col_date    = "date",
    col_amount  = "amount",
    type        = "overhead"
  )
  expect_true(all(result$practice_id == "p42"))
})

test_that("ingest_csv_generic adds is_refund column", {
  result <- ingest_csv_generic(
    path        = overhead_sample,
    practice_id = 1,
    col_date    = "date",
    col_amount  = "amount",
    type        = "overhead"
  )
  expect_true("is_refund" %in% names(result))
  expect_true(all(!result$is_refund))
})

# ── Date parsing ──────────────────────────────────────────────────────────────

test_that("ingest_csv_generic parses US date format (m/d/Y) automatically", {
  result <- ingest_csv_generic(
    path        = overhead_sample,
    practice_id = 1,
    col_date    = "date",
    col_amount  = "amount",
    type        = "overhead"
  )
  expect_s3_class(result$date, "Date")
  expect_equal(result$date[1], as.Date("2025-01-05"))
})

test_that("ingest_csv_generic parses ISO date format (Y-m-d) automatically", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(txn_date = "2025-03-15", spend = 500), tmp)
  result <- ingest_csv_generic(
    tmp, 1, "txn_date", "spend", type = "overhead"
  )
  expect_equal(result$date, as.Date("2025-03-15"))
  unlink(tmp)
})

test_that("ingest_csv_generic respects an explicit date_format argument", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(dt = "15-03-2025", amt = 300), tmp)
  result <- ingest_csv_generic(
    tmp, 1, "dt", "amt",
    type        = "overhead",
    date_format = "%d-%m-%Y"
  )
  expect_equal(result$date, as.Date("2025-03-15"))
  unlink(tmp)
})

test_that("ingest_csv_generic derives week_start, month, and year from date", {
  result <- ingest_csv_generic(
    path        = overhead_sample,
    practice_id = 1,
    col_date    = "date",
    col_amount  = "amount",
    type        = "overhead"
  )
  expect_s3_class(result$week_start, "Date")
  expect_equal(result$month[1], 1L)
  expect_equal(result$year[1],  2025L)
})

# ── Non-standard column names ─────────────────────────────────────────────────

test_that("ingest_csv_generic works with non-standard column names", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(`Transaction Date` = "01/15/2025", `Credit Amount` = 500, Notes = "Fee"),
    tmp
  )
  result <- ingest_csv_generic(
    path            = tmp,
    practice_id     = 1,
    col_date        = "Transaction Date",
    col_amount      = "Credit Amount",
    col_description = "Notes",
    type            = "income"
  )
  expect_equal(result$revenue, 500)
  expect_equal(result$description, "Fee")
  unlink(tmp)
})

# ── Optional columns ──────────────────────────────────────────────────────────

test_that("ingest_csv_generic defaults category to 'other' when col_category is not given", {
  result <- ingest_csv_generic(
    path        = overhead_sample,
    practice_id = 1,
    col_date    = "date",
    col_amount  = "amount",
    type        = "overhead"
  )
  expect_true(all(result$category == "other"))
})

test_that("ingest_csv_generic maps col_category when provided", {
  result <- ingest_csv_generic(
    path         = overhead_sample,
    practice_id  = 1,
    col_date     = "date",
    col_amount   = "amount",
    col_category = "category",
    type         = "overhead"
  )
  expect_false(all(result$category == "other"))
  expect_true("rent" %in% result$category)
})

test_that("ingest_csv_generic sets description to NA when col_description is not given", {
  result <- ingest_csv_generic(
    path        = overhead_sample,
    practice_id = 1,
    col_date    = "date",
    col_amount  = "amount",
    type        = "overhead"
  )
  expect_true(all(is.na(result$description)))
})

test_that("ingest_csv_generic warns when an optional column name does not exist", {
  expect_warning(
    ingest_csv_generic(
      path            = overhead_sample,
      practice_id     = 1,
      col_date        = "date",
      col_amount      = "amount",
      col_description = "nonexistent_col",
      type            = "overhead"
    ),
    class = "dcForecastR_missing_optional_columns"
  )
})

test_that("ingest_csv_generic falls back gracefully when optional column is absent", {
  result <- suppressWarnings(
    ingest_csv_generic(
      path            = overhead_sample,
      practice_id     = 1,
      col_date        = "date",
      col_amount      = "amount",
      col_description = "nonexistent_col",
      type            = "overhead"
    )
  )
  expect_true(all(is.na(result$description)))
})

# ── Error handling ────────────────────────────────────────────────────────────

test_that("ingest_csv_generic errors on missing required col_date", {
  expect_snapshot(
    ingest_csv_generic(
      path        = overhead_sample,
      practice_id = 1,
      col_date    = "does_not_exist",
      col_amount  = "amount",
      type        = "overhead"
    ),
    error = TRUE
  )
})

test_that("ingest_csv_generic error for missing column has dcForecastR_missing_columns class", {
  err <- tryCatch(
    ingest_csv_generic(
      path        = overhead_sample,
      practice_id = 1,
      col_date    = "does_not_exist",
      col_amount  = "amount",
      type        = "overhead"
    ),
    error = \(e) e
  )
  expect_true(inherits(err, "dcForecastR_missing_columns"))
})

test_that("ingest_csv_generic errors when dates cannot be parsed", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(dt = "not-a-date", amt = 100), tmp)
  expect_snapshot(
    ingest_csv_generic(tmp, 1, "dt", "amt", type = "overhead"),
    error = TRUE
  )
  unlink(tmp)
})

test_that("ingest_csv_generic date parse error has dcForecastR_invalid_dates class", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(dt = "not-a-date", amt = 100), tmp)
  err <- tryCatch(
    ingest_csv_generic(tmp, 1, "dt", "amt", "overhead"),
    error = \(e) e
  )
  expect_true(inherits(err, "dcForecastR_invalid_dates"))
  unlink(tmp)
})

test_that("ingest_csv_generic errors on a nonexistent file", {
  expect_error(
    ingest_csv_generic("no_such_file.csv", 1, "date", "amount", "overhead")
  )
})

# ── Refund handling ───────────────────────────────────────────────────────────

test_that("ingest_csv_generic flags negative amounts as refunds", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(dt = c("01/01/2025", "01/15/2025"), amt = c(500, -100)),
    tmp
  )
  result <- suppressWarnings(
    ingest_csv_generic(tmp, 1, "dt", "amt", "overhead")
  )
  expect_true(result$is_refund[2])
  expect_false(result$is_refund[1])
  unlink(tmp)
})

test_that("ingest_csv_generic warns when negative revenue rows are present", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(dt = c("01/01/2025", "01/15/2025"), amt = c(500, -100)),
    tmp
  )
  expect_warning(
    ingest_csv_generic(tmp, 1, "dt", "amt", "income"),
    class = "dcForecastR_refunds_detected"
  )
  unlink(tmp)
})

# ── Single-type downstream integration ───────────────────────────────────────

test_that("ingest_csv_generic type='overhead' flows into summarize_overhead_monthly", {
  result  <- ingest_csv_generic(
    path         = overhead_sample,
    practice_id  = 1,
    col_date     = "date",
    col_amount   = "amount",
    col_category = "category",
    type         = "overhead"
  )
  summary <- summarize_overhead_monthly(result)
  expect_s3_class(summary, "data.frame")
  expect_equal(nrow(summary), 3)
  expect_true(all(summary$total_overhead > 0))
})

test_that("ingest_csv_generic type='income' flows into summarize_income_monthly", {
  result  <- ingest_csv_generic(
    path        = income_sample,
    practice_id = 1,
    col_date    = "date",
    col_amount  = "amount",
    type        = "income"
  )
  summary <- summarize_income_monthly(result)
  expect_s3_class(summary, "data.frame")
  expect_equal(nrow(summary), 3)
  expect_true(all(summary$total_revenue > 0))
})
