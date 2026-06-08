test_that("summarize_overhead_monthly aggregates correctly", {
  # Expenses are positive amounts; is_refund = FALSE
  test_data <- tibble::tibble(
    practice_id = c(1, 1, 1, 1),
    date = as.Date(c("2025-01-15", "2025-01-20", "2025-02-10", "2025-02-15")),
    week_start = as.Date(c("2025-01-13", "2025-01-20", "2025-02-10", "2025-02-10")),
    full_account_name = c(
      "Expenses:Rent", "Expenses:Utilities",
      "Expenses:Rent", "Expenses:Supplies"
    ),
    account_name = c("Rent", "Utilities", "Rent", "Supplies"),
    description = c("Office rent", "Electric", "Office rent", "Paper"),
    amount = c(1000, 200, 1000, 150),
    month = c(1L, 1L, 2L, 2L),
    year = c(2025L, 2025L, 2025L, 2025L),
    category = c("rent", "other", "rent", "supplies"),
    source = "gnucash_csv",
    is_refund = c(FALSE, FALSE, FALSE, FALSE)
  )

  result <- summarize_overhead_monthly(test_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)

  expected_cols <- c("practice_id", "year", "month", "total_overhead",
                     "gross_overhead", "total_refunds")
  expect_true(all(expected_cols %in% names(result)))

  jan_row <- result |> dplyr::filter(month == 1)
  feb_row <- result |> dplyr::filter(month == 2)

  expect_equal(jan_row$total_overhead, 1200)
  expect_equal(feb_row$total_overhead, 1150)
  expect_equal(result$total_refunds, c(0, 0))
  expect_equal(result$gross_overhead, result$total_overhead)
})

test_that("summarize_overhead_monthly handles refunds correctly", {
  # Refunds are negative amounts; is_refund = TRUE
  test_data <- tibble::tibble(
    practice_id = c(1, 1, 1),
    date = as.Date(c("2025-01-15", "2025-01-20", "2025-01-25")),
    week_start = as.Date(c("2025-01-13", "2025-01-20", "2025-01-20")),
    full_account_name = c("Expenses:Rent", "Expenses:Utilities", "Expenses:Refund"),
    account_name = c("Rent", "Utilities", "Refund"),
    description = c("Office rent", "Electric", "Deposit refund"),
    amount = c(1000, 200, -100),  # negative = refund
    month = c(1L, 1L, 1L),
    year = c(2025L, 2025L, 2025L),
    category = c("rent", "other", "other"),
    source = "gnucash_csv",
    is_refund = c(FALSE, FALSE, TRUE)
  )

  result <- summarize_overhead_monthly(test_data)

  expect_equal(nrow(result), 1)
  expect_equal(result$gross_overhead, 1200)  # 1000 + 200
  expect_equal(result$total_refunds, 100)    # magnitude of refund
  expect_equal(result$total_overhead, 1100)  # 1200 - 100
})

test_that("summarize_overhead_monthly respects include_refunds = FALSE", {
  test_data <- tibble::tibble(
    practice_id = c(1, 1, 1),
    date = as.Date(c("2025-01-15", "2025-01-20", "2025-01-25")),
    week_start = as.Date(c("2025-01-13", "2025-01-20", "2025-01-20")),
    full_account_name = c("Expenses:Rent", "Expenses:Utilities", "Expenses:Refund"),
    account_name = c("Rent", "Utilities", "Refund"),
    description = c("Office rent", "Electric", "Deposit refund"),
    amount = c(1000, 200, -100),
    month = c(1L, 1L, 1L),
    year = c(2025L, 2025L, 2025L),
    category = c("rent", "other", "other"),
    source = "gnucash_csv",
    is_refund = c(FALSE, FALSE, TRUE)
  )

  result <- summarize_overhead_monthly(test_data, include_refunds = FALSE)

  expect_equal(nrow(result), 1)
  expect_equal(result$total_overhead, 1200)  # refund row excluded
})
