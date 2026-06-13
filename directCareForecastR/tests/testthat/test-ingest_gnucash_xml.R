sample_xml <- system.file(
  "extdata",
  "sample_gnucash.gnucash",
  package = "directCareForecastR"
)

skip_if_no_xml2 <- function() {
  skip_if_not_installed("xml2")
}

# ---------------------------------------------------------------------------
# Output schema
# ---------------------------------------------------------------------------

test_that("ingest_gnucash_xml returns a tibble with the expected columns", {
  skip_if_no_xml2()
  result <- suppressWarnings(ingest_gnucash_xml(
    sample_xml,
    practice_id = "test"
  ))
  expected_cols <- c(
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
  )
  expect_s3_class(result, "tbl_df")
  expect_true(all(expected_cols %in% names(result)))
  expect_false("is_refund" %in% names(result))
})

test_that("ingest_gnucash_xml sets source to 'gnucash_xml'", {
  skip_if_no_xml2()
  result <- suppressWarnings(ingest_gnucash_xml(
    sample_xml,
    practice_id = "test"
  ))
  expect_true(all(result$source == "gnucash_xml"))
})

test_that("ingest_gnucash_xml propagates practice_id to every row", {
  skip_if_no_xml2()
  result <- suppressWarnings(ingest_gnucash_xml(
    sample_xml,
    practice_id = "river-dpc"
  ))
  expect_true(all(result$practice_id == "river-dpc"))
})

test_that("ingest_gnucash_xml parses dates as Date class", {
  skip_if_no_xml2()
  result <- suppressWarnings(ingest_gnucash_xml(
    sample_xml,
    practice_id = "test"
  ))
  expect_s3_class(result$date, "Date")
  expect_s3_class(result$week_start, "Date")
})

test_that("ingest_gnucash_xml derives month and year from date", {
  skip_if_no_xml2()
  result <- suppressWarnings(ingest_gnucash_xml(
    sample_xml,
    practice_id = "test"
  ))
  expect_equal(result$month, lubridate::month(result$date))
  expect_equal(result$year, lubridate::year(result$date))
})

test_that("ingest_gnucash_xml derives week_start as Monday of transaction week", {
  skip_if_no_xml2()
  result <- suppressWarnings(ingest_gnucash_xml(
    sample_xml,
    practice_id = "test"
  ))
  expected <- lubridate::floor_date(result$date, "week", week_start = 1)
  expect_equal(result$week_start, expected)
})

# ---------------------------------------------------------------------------
# Account filtering -- only EXPENSE and INCOME splits are kept
# ---------------------------------------------------------------------------

test_that("ingest_gnucash_xml drops BANK/ASSET splits from output", {
  skip_if_no_xml2()
  result <- suppressWarnings(ingest_gnucash_xml(
    sample_xml,
    practice_id = "test"
  ))
  # The fixture has one BANK account (Checking). No row should reference it.
  expect_false(any(grepl("Checking", result$full_account_name)))
  expect_false(any(result$account_name == "Checking"))
})

test_that("ingest_gnucash_xml returns one row per EXPENSE or INCOME split", {
  skip_if_no_xml2()
  # Fixture: 3 transactions, each with 1 EXPENSE/INCOME split and 1 BANK split
  result <- suppressWarnings(ingest_gnucash_xml(
    sample_xml,
    practice_id = "test"
  ))
  expect_equal(nrow(result), 3L)
})

# ---------------------------------------------------------------------------
# Full account name construction
# ---------------------------------------------------------------------------

test_that("ingest_gnucash_xml builds full account names from parent chain", {
  skip_if_no_xml2()
  result <- suppressWarnings(ingest_gnucash_xml(
    sample_xml,
    practice_id = "test"
  ))
  expect_true("Expenses:Rent" %in% result$full_account_name)
  expect_true("Income:Membership" %in% result$full_account_name)
})

test_that("ingest_gnucash_xml excludes ROOT account name from the path", {
  skip_if_no_xml2()
  result <- suppressWarnings(ingest_gnucash_xml(
    sample_xml,
    practice_id = "test"
  ))
  expect_false(any(grepl("Root Account", result$full_account_name)))
})

test_that("account_name holds the leaf account name only", {
  skip_if_no_xml2()
  result <- suppressWarnings(ingest_gnucash_xml(
    sample_xml,
    practice_id = "test"
  ))
  expect_true("Rent" %in% result$account_name)
  expect_true("Membership" %in% result$account_name)
  expect_false(any(grepl(":", result$account_name)))
})

# ---------------------------------------------------------------------------
# Amount parsing and sign convention
# ---------------------------------------------------------------------------

test_that("ingest_gnucash_xml parses rational amounts correctly", {
  skip_if_no_xml2()
  result <- suppressWarnings(ingest_gnucash_xml(
    sample_xml,
    practice_id = "test"
  ))
  rent_row <- result[result$account_name == "Rent" & result$amount > 0, ]
  expect_equal(rent_row$amount, 1200.00)
})

test_that("ingest_gnucash_xml negates INCOME split amounts to positive revenue", {
  skip_if_no_xml2()
  result <- suppressWarnings(ingest_gnucash_xml(
    sample_xml,
    practice_id = "test"
  ))
  income_row <- result[result$account_name == "Membership", ]
  expect_equal(income_row$amount, 150.00)
})

test_that("ingest_gnucash_xml preserves negative EXPENSE amounts as refunds", {
  skip_if_no_xml2()
  result <- suppressWarnings(ingest_gnucash_xml(
    sample_xml,
    practice_id = "test"
  ))
  refund_row <- result[result$account_name == "Rent" & result$amount < 0, ]
  expect_equal(nrow(refund_row), 1L)
  expect_equal(refund_row$amount, -100.00)
})

# ---------------------------------------------------------------------------
# Account mapping integration
# ---------------------------------------------------------------------------

test_that("ingest_gnucash_xml applies the default account map", {
  skip_if_no_xml2()
  result <- suppressWarnings(ingest_gnucash_xml(
    sample_xml,
    practice_id = "test"
  ))
  rent_rows <- result[result$account_name == "Rent", ]
  expect_true(all(rent_rows$category == "rent"))
})

test_that("ingest_gnucash_xml accepts a custom account map", {
  skip_if_no_xml2()
  custom_map <- tibble::tibble(
    pattern = "Rent",
    category = "occupancy",
    match_type = "contains",
    ignore_case = TRUE
  )
  result <- suppressWarnings(
    ingest_gnucash_xml(
      sample_xml,
      practice_id = "test",
      account_map = custom_map
    )
  )
  rent_rows <- result[result$account_name == "Rent", ]
  expect_true(all(rent_rows$category == "occupancy"))
})

# ---------------------------------------------------------------------------
# Downstream compatibility
# ---------------------------------------------------------------------------

test_that("filter_gnucash_overhead works on XML output", {
  skip_if_no_xml2()
  result <- suppressWarnings(ingest_gnucash_xml(
    sample_xml,
    practice_id = "test"
  ))
  overhead <- filter_gnucash_overhead(result)
  expect_s3_class(overhead, "tbl_df")
  expect_true(all(grepl("Expenses", overhead$full_account_name)))
  expect_true("is_refund" %in% names(overhead))
})

test_that("normalize_gnucash_income works on XML output", {
  skip_if_no_xml2()
  result <- suppressWarnings(ingest_gnucash_xml(
    sample_xml,
    practice_id = "test"
  ))
  income <- normalize_gnucash_income(result)
  expect_s3_class(income, "tbl_df")
  expect_true(all(grepl("Income", income$full_account_name)))
  expect_true("revenue" %in% names(income))
  expect_false("amount" %in% names(income))
})

test_that("summarize_overhead_monthly works on XML-derived overhead", {
  skip_if_no_xml2()
  result <- suppressWarnings(ingest_gnucash_xml(
    sample_xml,
    practice_id = "test"
  ))
  overhead <- filter_gnucash_overhead(result) |> validate_overhead()
  monthly <- summarize_overhead_monthly(overhead)
  expect_s3_class(monthly, "tbl_df")
  expect_true(nrow(monthly) > 0L)
  expect_true("total_overhead" %in% names(monthly))
})

# ---------------------------------------------------------------------------
# Error cases
# ---------------------------------------------------------------------------

test_that("ingest_gnucash_xml errors on a missing file", {
  skip_if_no_xml2()
  expect_error(ingest_gnucash_xml("no_such_file.gnucash", practice_id = "test"))
})

test_that("ingest_gnucash_xml errors with dcForecastR_no_data on a file with no transactions", {
  skip_if_no_xml2()
  empty_xml <- tempfile(fileext = ".gnucash")
  writeLines(
    c(
      '<?xml version="1.0" encoding="utf-8" ?>',
      '<gnc-v2 xmlns:gnc="http://www.gnucash.org/XML/gnc"',
      '        xmlns:book="http://www.gnucash.org/XML/book"',
      '        xmlns:cd="http://www.gnucash.org/XML/cd">',
      '  <gnc:count-data cd:type="book">1</gnc:count-data>',
      '  <gnc:book version="2.0.0">',
      '    <book:id type="guid">empty-book</book:id>',
      '  </gnc:book>',
      '</gnc-v2>'
    ),
    empty_xml
  )
  on.exit(unlink(empty_xml))
  expect_error(
    ingest_gnucash_xml(empty_xml, practice_id = "test"),
    class = "dcForecastR_no_data"
  )
})
