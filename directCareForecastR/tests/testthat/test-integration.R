# Integration tests against the real GnuCash CSV export.
#
# The CSV contains only expense and asset/imbalance transactions — no Income
# entries are present at this time. Tests cover the full overhead pipeline
# (ingest → filter → summarize → forecast) and confirm that the income path
# returns an empty tibble gracefully rather than erroring.
#
# Shared pipeline objects are computed once at file scope so each test_that
# block can reference them without repeating the suppressWarnings() boilerplate.
# The real CSV legitimately contains refunds (negative amounts), zero-amount
# rows, and a future-dated entry; those produce expected validate_overhead()
# warnings that we suppress at the file level rather than treating as failures.

csv_path     <- testthat::test_path("test-gnucash.csv")
transactions <- suppressWarnings(ingest_gnucash_csv(csv_path, practice_id = 1))
overhead     <- suppressWarnings(filter_gnucash_overhead(transactions))
income       <- suppressWarnings(normalize_gnucash_income(transactions))
monthly      <- summarize_overhead_monthly(overhead)

# =============================================================================
# Ingest
# =============================================================================

test_that("ingest_gnucash_csv reads the real export without error", {
  expect_no_error(
    suppressWarnings(ingest_gnucash_csv(csv_path, practice_id = 1))
  )
})

test_that("ingest_gnucash_csv returns the normalized schema", {
  expected_cols <- c(
    "practice_id", "date", "week_start", "month", "year",
    "full_account_name", "account_name", "description",
    "amount", "category", "source"
  )
  expect_true(all(expected_cols %in% names(transactions)))
  expect_false("is_refund" %in% names(transactions))
})

test_that("ingest_gnucash_csv parses dates correctly", {
  expect_s3_class(transactions$date, "Date")
  expect_false(anyNA(transactions$date))
})

test_that("ingest_gnucash_csv assigns categories from the default account map", {
  known_categories <- c(
    "rent", "staff", "supplies", "software", "insurance",
    "marketing", "labs", "equipment", "licenses", "education", "other"
  )
  expect_true(all(transactions$category %in% known_categories))
  expect_false(anyNA(transactions$category))
})

test_that("ingest_gnucash_csv maps known DPC account names correctly", {
  get_category <- function(acct) {
    unique(transactions$category[transactions$account_name == acct])
  }

  expect_equal(get_category("Rent"),                "rent")
  expect_equal(get_category("Malpractice Insurance"), "insurance")
  expect_equal(get_category("Payroll Expenses"),    "staff")
  expect_equal(get_category("Advertisement"),       "marketing")
  expect_equal(get_category("Labs"),                "labs")
  expect_equal(get_category("EMR"),                 "software")
  expect_equal(get_category("Equipment Rental"),    "equipment")
  expect_equal(get_category("Licenses and Permits"), "licenses")
  expect_equal(get_category("Education"),           "education")
})

# =============================================================================
# Filter: overhead
# =============================================================================

test_that("filter_gnucash_overhead returns only Expenses rows", {
  expect_true(all(grepl("Expenses", overhead$full_account_name)))
  expect_true("is_refund" %in% names(overhead))
})

test_that("filter_gnucash_overhead removes Imbalance-USD and asset rows", {
  expect_false(any(grepl("Imbalance", overhead$full_account_name)))
  expect_false(any(grepl("Assets",    overhead$full_account_name)))
})

test_that("filter_gnucash_overhead contains no NA amounts", {
  expect_false(anyNA(overhead$amount))
})

# =============================================================================
# Filter: income (empty — no Income rows in this export)
# =============================================================================

test_that("normalize_gnucash_income returns empty tibble gracefully when no income rows exist", {
  expect_s3_class(income, "data.frame")
  expect_equal(nrow(income), 0)
  expect_true("revenue"   %in% names(income))
  expect_true("is_refund" %in% names(income))
})

# =============================================================================
# Summarize
# =============================================================================

test_that("summarize_overhead_monthly produces one row per month", {
  expect_s3_class(monthly, "data.frame")
  expect_true(nrow(monthly) > 0)

  # One row per practice_id/year/month combination
  combos <- paste(monthly$practice_id, monthly$year, monthly$month)
  expect_equal(length(combos), length(unique(combos)))
})

test_that("summarize_overhead_monthly totals are non-negative after refund netting", {
  expect_true(all(monthly$gross_overhead >= 0))
  expect_true(all(monthly$total_refunds  >= 0))
})

test_that("summarize_overhead_monthly gross_overhead >= total_overhead always", {
  expect_true(all(monthly$gross_overhead >= monthly$total_overhead))
})

# =============================================================================
# Forecast (linear only — sparse real data, no income rows)
# =============================================================================

test_that("forecast_revenue runs on real monthly overhead data without error", {
  # Use overhead totals as a stand-in revenue series to exercise the
  # forecasting engine with real data magnitudes.
  mock_income <- tibble::tibble(
    practice_id   = monthly$practice_id,
    year          = monthly$year,
    month         = monthly$month,
    total_revenue = monthly$total_overhead
  )

  expect_no_error(
    result <- forecast_revenue(mock_income, method = "linear", horizon = 6)
  )
  expect_equal(nrow(result$forecast_data), 6)
})

test_that("forecast_breakeven runs on real overhead data without error", {
  mock_income <- tibble::tibble(
    practice_id   = monthly$practice_id,
    year          = monthly$year,
    month         = monthly$month,
    total_revenue = monthly$total_overhead * 0.5  # below overhead — will show deficit
  )

  expect_no_error(
    suppressWarnings(
      result <- forecast_breakeven(mock_income, monthly, method = "linear", horizon = 24)
    )
  )
  expect_equal(result$frequency, "monthly")
  expect_equal(nrow(result$forecast_data), 24)
})
