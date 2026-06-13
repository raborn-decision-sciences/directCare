test_that("normalize_gnucash_csv produces correct normalized schema", {
  temp_csv <- tempfile(fileext = ".csv")

  test_data <- data.frame(
    Date = c("01/15/2025", "02/20/2025", "03/10/2025"),
    `Full Account Name` = c(
      "Expenses:Rent",
      "Expenses:Utilities",
      "Income:Sales"
    ),
    `Account Name` = c("Rent", "Utilities", "Sales"),
    Description = c("Office rent", "Electric", "Payment"),
    `Amount Num.` = c(1000, 200, 500),
    ExtraColumn = c("A", "B", "C"),
    check.names = FALSE
  )

  readr::write_csv(test_data, temp_csv)

  result <- suppressWarnings(ingest_gnucash_csv(temp_csv, practice_id = "p1"))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)

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
  expect_false("Amount Num." %in% names(result))
  expect_false("ExtraColumn" %in% names(result))
  # is_refund is added downstream by filter/normalize, not by ingest
  expect_false("is_refund" %in% names(result))

  expect_s3_class(result$date, "Date")
  expect_s3_class(result$week_start, "Date")
  expect_equal(result$month, c(1L, 2L, 3L))
  expect_equal(result$year, c(2025L, 2025L, 2025L))
  expect_true(all(result$source == "gnucash_csv"))

  unlink(temp_csv)
})

test_that("normalize_gnucash_csv errors on missing required columns", {
  temp_csv <- tempfile(fileext = ".csv")
  bad_data <- data.frame(Col1 = 1:2, Col2 = c("a", "b"))
  readr::write_csv(bad_data, temp_csv)

  expect_error(ingest_gnucash_csv(temp_csv, practice_id = 1))

  unlink(temp_csv)
})

test_that("normalize_gnucash_income filters to income rows, renames amount to revenue, and adds is_refund", {
  normalized <- tibble::tibble(
    practice_id = 1,
    date = as.Date(c("2025-01-15", "2025-02-20", "2025-03-10")),
    week_start = as.Date(c("2025-01-13", "2025-02-17", "2025-03-10")),
    month = c(1L, 2L, 3L),
    year = c(2025L, 2025L, 2025L),
    full_account_name = c("Income:Sales", "Expenses:Rent", "Income:Consulting"),
    account_name = c("Sales", "Rent", "Consulting"),
    description = c("Payment", "Rent", "Project A"),
    amount = c(1000, 1000, 2000),
    category = c("other", "rent", "other"),
    source = "gnucash_csv"
  )

  result <- normalize_gnucash_income(normalized)

  expect_equal(nrow(result), 2)
  expect_true(all(grepl("Income", result$full_account_name)))

  expect_true("revenue" %in% names(result))
  expect_false("amount" %in% names(result))
  expect_true("is_refund" %in% names(result))

  expect_equal(result$revenue, c(1000, 2000))
  expect_true(all(!result$is_refund))
})
