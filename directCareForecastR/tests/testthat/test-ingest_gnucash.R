test_that("ingest_gnucash_csv reads and normalizes CSV files correctly", {
  temp_csv <- tempfile(fileext = ".csv")

  test_data <- data.frame(
    Date = c("01/15/2025", "02/20/2025"),
    `Full Account Name` = c("Expenses:Rent", "Expenses:Utilities"),
    `Account Name` = c("Rent", "Utilities"),
    Description = c("Office rent", "Electric bill"),
    `Amount Num.` = c(1000, 200),
    check.names = FALSE
  )

  readr::write_csv(test_data, temp_csv)

  result <- ingest_gnucash_csv(temp_csv, practice_id = 1)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)

  # is_refund is NOT added at this stage; it is added by filter_gnucash_overhead()
  # and normalize_gnucash_income() after the rows are split
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
  expect_true(all(expected_cols %in% names(result)))
  expect_false("is_refund" %in% names(result))

  expect_equal(result$practice_id, c(1, 1))
  expect_equal(result$source, c("gnucash_csv", "gnucash_csv"))
  expect_equal(result$month, c(1L, 2L))
  expect_equal(result$year, c(2025L, 2025L))

  unlink(temp_csv)
})

test_that("ingest_gnucash_csv handles missing files gracefully", {
  expect_error(ingest_gnucash_csv("nonexistent_file.csv", practice_id = 1))
})

test_that("ingest_gnucash_csv requires practice_id", {
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    Date = "01/15/2025",
    `Full Account Name` = "Expenses:Rent",
    `Account Name` = "Rent",
    Description = "Office rent",
    `Amount Num.` = 1000,
    check.names = FALSE
  )
  readr::write_csv(test_data, temp_csv)

  expect_error(ingest_gnucash_csv(temp_csv))

  unlink(temp_csv)
})

test_that("ingest_gnucash_csv negates INCOME split amounts to positive revenue", {
  # Real GnuCash CSV exports use the same signed-split ledger convention as
  # the native XML: INCOME splits are recorded as negative (a credit), while
  # EXPENSE splits are positive. Confirmed against an actual practice export
  # where every Income:* row carried a negative Amount Num.
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    Date = c("01/15/2025", "01/16/2025"),
    `Full Account Name` = c("Income:Membership Fees", "Expenses:Rent"),
    `Account Name` = c("Membership Fees", "Rent"),
    Description = c("Membership payment", "Office rent"),
    `Amount Num.` = c(-150.00, 1000.00),
    check.names = FALSE
  )
  readr::write_csv(test_data, temp_csv)

  result <- ingest_gnucash_csv(temp_csv, practice_id = 1)

  income_row <- result[result$account_name == "Membership Fees", ]
  expect_equal(income_row$amount, 150.00)

  expense_row <- result[result$account_name == "Rent", ]
  expect_equal(expense_row$amount, 1000.00)

  unlink(temp_csv)
})

test_that("ingest_gnucash_csv preserves negative EXPENSE amounts as refunds", {
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    Date = "01/15/2025",
    `Full Account Name` = "Expenses:Rent",
    `Account Name` = "Rent",
    Description = "Rent refund",
    `Amount Num.` = -100.00,
    check.names = FALSE
  )
  readr::write_csv(test_data, temp_csv)

  result <- ingest_gnucash_csv(temp_csv, practice_id = 1)
  expect_equal(result$amount, -100.00)

  unlink(temp_csv)
})
