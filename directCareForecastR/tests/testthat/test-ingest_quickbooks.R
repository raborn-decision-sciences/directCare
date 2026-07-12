test_that("ingest_quickbooks_csv reads and normalizes a Transaction List by Date export", {
  temp_csv <- tempfile(fileext = ".csv")

  test_data <- data.frame(
    Date = c("01/05/2025", "01/10/2025", "01/12/2025"),
    `Transaction Type` = c("Deposit", "Check", "Bill Payment"),
    Name = c("Jane Doe", "ABC Realty", "Acme Supplies"),
    `Memo/Description` = c(
      "January membership",
      "Office rent",
      "Medical supplies"
    ),
    Account = c("Membership Fees", "Rent Expense", "Medical Supplies"),
    Split = c("Undeposited Funds", "Checking", "Checking"),
    Amount = c(89, -2000, -350.5),
    check.names = FALSE
  )
  readr::write_csv(test_data, temp_csv)

  result <- ingest_quickbooks_csv(temp_csv, practice_id = 1)

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
  expect_equal(nrow(result), 3L)
  expect_equal(result$source, rep("quickbooks_csv", 3L))
  expect_equal(result$practice_id, rep(1, 3L))

  unlink(temp_csv)
})

test_that("ingest_quickbooks_csv classifies rows by amount sign", {
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    Date = c("03/01/2025", "03/02/2025"),
    Account = c("Membership Fees", "Rent Expense"),
    Amount = c(500, -1200),
    check.names = FALSE
  )
  readr::write_csv(test_data, temp_csv)

  result <- ingest_quickbooks_csv(temp_csv, practice_id = "test")

  expect_true(grepl("^Income:", result$full_account_name[result$amount == 500]))
  expect_true(
    grepl("^Expenses:", result$full_account_name[result$amount == 1200])
  )
  # Amounts are stored as positive magnitudes regardless of sign in the source.
  expect_equal(sort(result$amount), c(500, 1200))

  unlink(temp_csv)
})

test_that("ingest_quickbooks_csv maps expense accounts to internal categories", {
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    Date = c("04/01/2025", "04/02/2025", "04/03/2025"),
    Account = c("Rent Expense", "Payroll Expenses", "Medical Supplies"),
    Amount = c(-2000, -4200, -350),
    check.names = FALSE
  )
  readr::write_csv(test_data, temp_csv)

  result <- ingest_quickbooks_csv(temp_csv, practice_id = "test")
  cats <- setNames(result$category, result$account_name)

  expect_equal(cats[["Rent Expense"]], "rent")
  expect_equal(cats[["Payroll Expenses"]], "staff")
  expect_equal(cats[["Medical Supplies"]], "supplies")

  unlink(temp_csv)
})

test_that("ingest_quickbooks_csv is compatible with filter_gnucash_overhead / normalize_gnucash_income", {
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    Date = c("05/01/2025", "05/02/2025"),
    Account = c("Membership Fees", "Rent Expense"),
    Amount = c(89, -2000),
    check.names = FALSE
  )
  readr::write_csv(test_data, temp_csv)

  tx <- ingest_quickbooks_csv(temp_csv, practice_id = "test")
  overhead <- filter_gnucash_overhead(tx)
  income <- normalize_gnucash_income(tx)

  expect_equal(nrow(overhead), 1L)
  expect_equal(overhead$amount, 2000)
  expect_equal(nrow(income), 1L)
  expect_equal(income$revenue, 89)

  unlink(temp_csv)
})

test_that("ingest_quickbooks_csv errors when required columns are missing", {
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    Date = "01/15/2025",
    Description = "Office rent",
    check.names = FALSE
  )
  readr::write_csv(test_data, temp_csv)

  expect_error(
    ingest_quickbooks_csv(temp_csv, practice_id = 1),
    class = "dcForecastR_missing_columns"
  )

  unlink(temp_csv)
})

test_that("ingest_quickbooks_csv handles missing files gracefully", {
  expect_error(ingest_quickbooks_csv("nonexistent_file.csv", practice_id = 1))
})

test_that("ingest_quickbooks_csv accepts 'Account Name' as an alias for 'Account'", {
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    Date = "06/01/2025",
    `Account Name` = "Rent Expense",
    Amount = -1500,
    check.names = FALSE
  )
  readr::write_csv(test_data, temp_csv)

  result <- ingest_quickbooks_csv(temp_csv, practice_id = "test")
  expect_equal(result$account_name, "Rent Expense")
  expect_equal(result$amount, 1500)

  unlink(temp_csv)
})
