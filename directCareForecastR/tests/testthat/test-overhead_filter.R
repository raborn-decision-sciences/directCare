test_that("filter_gnucash_overhead filters to expense rows only", {
  normalized <- tibble::tibble(
    practice_id = 1,
    date = as.Date(c("2025-01-15", "2025-02-20", "2025-03-10")),
    week_start = as.Date(c("2025-01-13", "2025-02-17", "2025-03-10")),
    month = c(1L, 2L, 3L),
    year = c(2025L, 2025L, 2025L),
    full_account_name = c("Expenses:Rent", "Income:Sales", "Expenses:Utilities"),
    account_name = c("Rent", "Sales", "Utilities"),
    description = c("Office rent", "Payment", "Electric bill"),
    amount = c(1000, 500, 200),
    category = c("rent", "other", "other"),
    source = "gnucash_csv",
    is_refund = c(FALSE, FALSE, FALSE)
  )

  result <- filter_gnucash_overhead(normalized)

  expect_equal(nrow(result), 2)
  expect_true(all(grepl("Expenses", result$full_account_name)))
  expect_equal(result$amount, c(1000, 200))
  expect_equal(result$account_name, c("Rent", "Utilities"))
})

test_that("filter_gnucash_overhead returns empty data frame when no expenses", {
  normalized <- tibble::tibble(
    practice_id = 1,
    date = as.Date(c("2025-01-15", "2025-02-20")),
    week_start = as.Date(c("2025-01-13", "2025-02-17")),
    month = c(1L, 2L),
    year = c(2025L, 2025L),
    full_account_name = c("Income:Sales", "Assets:Bank"),
    account_name = c("Sales", "Bank"),
    description = c("Payment", "Deposit"),
    amount = c(500, 1000),
    category = c("other", "other"),
    source = "gnucash_csv",
    is_refund = c(FALSE, FALSE)
  )

  result <- filter_gnucash_overhead(normalized)

  expect_equal(nrow(result), 0)
  expect_s3_class(result, "data.frame")
})
