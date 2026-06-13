# Helpers ----------------------------------------------------------------------

make_monthly_overhead_tbl <- function() {
  tibble::tibble(
    practice_id = c(1, 1, 1, 1),
    date = as.Date(c("2025-01-15", "2025-01-20", "2025-02-10", "2025-02-15")),
    week_start = as.Date(c(
      "2025-01-13",
      "2025-01-20",
      "2025-02-10",
      "2025-02-10"
    )),
    full_account_name = c(
      "Expenses:Rent",
      "Expenses:Utilities",
      "Expenses:Rent",
      "Expenses:Supplies"
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
}

make_weekly_overhead_tbl <- function() {
  weeks <- seq(as.Date("2025-01-06"), by = "week", length.out = 4)
  tibble::tibble(
    practice_id = rep(1, 4),
    date = weeks,
    week_start = weeks,
    full_account_name = rep("Expenses:Rent", 4),
    account_name = rep("Rent", 4),
    description = rep("Weekly overhead", 4),
    amount = c(250, 250, 300, 300),
    month = c(1L, 1L, 1L, 2L),
    year = c(2025L, 2025L, 2025L, 2025L),
    category = rep("rent", 4),
    source = "gnucash_csv",
    is_refund = c(FALSE, FALSE, FALSE, FALSE)
  )
}

# =============================================================================
# summarize_overhead_monthly
# =============================================================================

test_that("summarize_overhead_monthly returns correct structure", {
  result <- summarize_overhead_monthly(make_monthly_overhead_tbl())

  expect_s3_class(result, "data.frame")
  expected_cols <- c(
    "practice_id",
    "year",
    "month",
    "total_overhead",
    "gross_overhead",
    "total_refunds"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("summarize_overhead_monthly aggregates to one row per month", {
  result <- summarize_overhead_monthly(make_monthly_overhead_tbl())
  expect_equal(nrow(result), 2)
})

test_that("summarize_overhead_monthly sums overhead correctly", {
  result <- summarize_overhead_monthly(make_monthly_overhead_tbl())

  jan <- dplyr::filter(result, month == 1)
  feb <- dplyr::filter(result, month == 2)

  expect_equal(jan$total_overhead, 1200)
  expect_equal(feb$total_overhead, 1150)
})

test_that("summarize_overhead_monthly gross_overhead equals total_overhead when no refunds", {
  result <- summarize_overhead_monthly(make_monthly_overhead_tbl())
  expect_equal(result$gross_overhead, result$total_overhead)
  expect_equal(result$total_refunds, c(0, 0))
})

test_that("summarize_overhead_monthly handles refunds correctly", {
  overhead <- make_monthly_overhead_tbl()
  refund_row <- tibble::tibble(
    practice_id = 1,
    date = as.Date("2025-01-25"),
    week_start = as.Date("2025-01-20"),
    full_account_name = "Expenses:Rent",
    account_name = "Rent",
    description = "Deposit refund",
    amount = -100,
    month = 1L,
    year = 2025L,
    category = "rent",
    source = "gnucash_csv",
    is_refund = TRUE
  )
  overhead <- dplyr::bind_rows(overhead, refund_row)

  result <- summarize_overhead_monthly(overhead)
  jan <- dplyr::filter(result, month == 1)

  expect_equal(jan$gross_overhead, 1200) # 1000 + 200, ignoring refund
  expect_equal(jan$total_refunds, 100) # magnitude of refund
  expect_equal(jan$total_overhead, 1100) # 1200 - 100
})

test_that("summarize_overhead_monthly derives is_refund when column is absent", {
  overhead <- make_monthly_overhead_tbl() |>
    dplyr::bind_rows(tibble::tibble(
      practice_id = 1,
      date = as.Date("2025-01-25"),
      week_start = as.Date("2025-01-20"),
      full_account_name = "Expenses:Rent",
      account_name = "Rent",
      description = "Refund",
      amount = -50,
      month = 1L,
      year = 2025L,
      category = "rent",
      source = "gnucash_csv",
      is_refund = TRUE
    )) |>
    dplyr::select(-is_refund)

  result <- summarize_overhead_monthly(overhead)
  jan <- dplyr::filter(result, month == 1)

  expect_equal(jan$gross_overhead, 1200)
  expect_equal(jan$total_refunds, 50)
  expect_equal(jan$total_overhead, 1150)
})

test_that("summarize_overhead_monthly include_refunds = FALSE excludes refund rows", {
  overhead <- make_monthly_overhead_tbl()
  refund_row <- tibble::tibble(
    practice_id = 1,
    date = as.Date("2025-01-25"),
    week_start = as.Date("2025-01-20"),
    full_account_name = "Expenses:Rent",
    account_name = "Rent",
    description = "Refund",
    amount = -200,
    month = 1L,
    year = 2025L,
    category = "rent",
    source = "gnucash_csv",
    is_refund = TRUE
  )
  overhead <- dplyr::bind_rows(overhead, refund_row)

  result <- summarize_overhead_monthly(overhead, include_refunds = FALSE)
  jan <- dplyr::filter(result, month == 1)

  expect_equal(jan$total_overhead, 1200) # refund row excluded entirely
})

test_that("summarize_overhead_monthly groups by practice_id", {
  overhead <- dplyr::bind_rows(
    make_monthly_overhead_tbl(),
    dplyr::mutate(make_monthly_overhead_tbl(), practice_id = 2)
  )

  result <- summarize_overhead_monthly(overhead)
  expect_equal(nrow(result), 4) # 2 practices × 2 months
  expect_equal(sort(unique(result$practice_id)), c(1, 2))
})

test_that("summarize_overhead_monthly errors on missing required columns", {
  bad <- tibble::tibble(practice_id = 1, year = 2025, amount = 1000)

  expect_error(
    summarize_overhead_monthly(bad),
    class = "dcForecastR_missing_columns"
  )
})

test_that("summarize_overhead_monthly total_refunds is always non-negative", {
  overhead <- make_monthly_overhead_tbl()
  refund_row <- tibble::tibble(
    practice_id = 1,
    date = as.Date("2025-01-25"),
    week_start = as.Date("2025-01-20"),
    full_account_name = "Expenses:Rent",
    account_name = "Rent",
    description = "Refund",
    amount = -800,
    month = 1L,
    year = 2025L,
    category = "rent",
    source = "gnucash_csv",
    is_refund = TRUE
  )
  overhead <- dplyr::bind_rows(overhead, refund_row)

  result <- summarize_overhead_monthly(overhead)
  expect_true(all(result$total_refunds >= 0))
})

# =============================================================================
# summarize_overhead_weekly
# =============================================================================

test_that("summarize_overhead_weekly returns correct structure", {
  result <- summarize_overhead_weekly(make_weekly_overhead_tbl())

  expect_s3_class(result, "data.frame")
  expected_cols <- c(
    "practice_id",
    "week_start",
    "total_overhead",
    "gross_overhead",
    "total_refunds"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("summarize_overhead_weekly aggregates to one row per week", {
  result <- summarize_overhead_weekly(make_weekly_overhead_tbl())
  expect_equal(nrow(result), 4) # 4 distinct weeks
})

test_that("summarize_overhead_weekly sums overhead within a week correctly", {
  overhead <- tibble::tibble(
    practice_id = c(1, 1),
    date = as.Date(c("2025-01-06", "2025-01-08")),
    week_start = as.Date(c("2025-01-06", "2025-01-06")),
    full_account_name = rep("Expenses:Rent", 2),
    account_name = rep("Rent", 2),
    description = rep("Rent", 2),
    amount = c(600, 400),
    month = c(1L, 1L),
    year = c(2025L, 2025L),
    category = rep("rent", 2),
    source = "gnucash_csv",
    is_refund = c(FALSE, FALSE)
  )

  result <- summarize_overhead_weekly(overhead)
  expect_equal(nrow(result), 1)
  expect_equal(result$total_overhead, 1000)
})

test_that("summarize_overhead_weekly handles refunds correctly", {
  overhead <- make_weekly_overhead_tbl()
  refund_row <- tibble::tibble(
    practice_id = 1,
    date = as.Date("2025-01-06"),
    week_start = as.Date("2025-01-06"),
    full_account_name = "Expenses:Rent",
    account_name = "Rent",
    description = "Deposit refund",
    amount = -50,
    month = 1L,
    year = 2025L,
    category = "rent",
    source = "gnucash_csv",
    is_refund = TRUE
  )
  overhead <- dplyr::bind_rows(overhead, refund_row)

  result <- summarize_overhead_weekly(overhead)
  week1 <- dplyr::filter(result, week_start == as.Date("2025-01-06"))

  expect_equal(week1$gross_overhead, 250) # original row only
  expect_equal(week1$total_refunds, 50)
  expect_equal(week1$total_overhead, 200)
})

test_that("summarize_overhead_weekly derives is_refund when column is absent", {
  overhead <- make_weekly_overhead_tbl() |>
    dplyr::bind_rows(tibble::tibble(
      practice_id = 1,
      date = as.Date("2025-01-06"),
      week_start = as.Date("2025-01-06"),
      full_account_name = "Expenses:Rent",
      account_name = "Rent",
      description = "Refund",
      amount = -30,
      month = 1L,
      year = 2025L,
      category = "rent",
      source = "gnucash_csv",
      is_refund = TRUE
    )) |>
    dplyr::select(-is_refund)

  result <- summarize_overhead_weekly(overhead)
  week1 <- dplyr::filter(result, week_start == as.Date("2025-01-06"))

  expect_equal(week1$gross_overhead, 250)
  expect_equal(week1$total_refunds, 30)
  expect_equal(week1$total_overhead, 220)
})

test_that("summarize_overhead_weekly include_refunds = FALSE excludes refund rows", {
  overhead <- make_weekly_overhead_tbl()
  refund_row <- tibble::tibble(
    practice_id = 1,
    date = as.Date("2025-01-06"),
    week_start = as.Date("2025-01-06"),
    full_account_name = "Expenses:Rent",
    account_name = "Rent",
    description = "Refund",
    amount = -100,
    month = 1L,
    year = 2025L,
    category = "rent",
    source = "gnucash_csv",
    is_refund = TRUE
  )
  overhead <- dplyr::bind_rows(overhead, refund_row)

  result <- summarize_overhead_weekly(overhead, include_refunds = FALSE)
  week1 <- dplyr::filter(result, week_start == as.Date("2025-01-06"))

  expect_equal(week1$total_overhead, 250) # refund row excluded
})

test_that("summarize_overhead_weekly week_start column is Date class", {
  result <- summarize_overhead_weekly(make_weekly_overhead_tbl())
  expect_s3_class(result$week_start, "Date")
})

test_that("summarize_overhead_weekly groups by practice_id", {
  overhead <- dplyr::bind_rows(
    make_weekly_overhead_tbl(),
    dplyr::mutate(make_weekly_overhead_tbl(), practice_id = 2)
  )

  result <- summarize_overhead_weekly(overhead)
  expect_equal(nrow(result), 8) # 2 practices × 4 weeks
  expect_equal(sort(unique(result$practice_id)), c(1, 2))
})

test_that("summarize_overhead_weekly errors when week_start column is missing", {
  bad <- tibble::tibble(practice_id = 1, amount = 500)

  expect_error(
    summarize_overhead_weekly(bad),
    class = "dcForecastR_missing_columns"
  )
})

test_that("summarize_overhead_weekly total_refunds is always non-negative", {
  overhead <- make_weekly_overhead_tbl()
  refund_row <- tibble::tibble(
    practice_id = 1,
    date = as.Date("2025-01-06"),
    week_start = as.Date("2025-01-06"),
    full_account_name = "Expenses:Rent",
    account_name = "Rent",
    description = "Refund",
    amount = -500,
    month = 1L,
    year = 2025L,
    category = "rent",
    source = "gnucash_csv",
    is_refund = TRUE
  )
  overhead <- dplyr::bind_rows(overhead, refund_row)

  result <- summarize_overhead_weekly(overhead)
  expect_true(all(result$total_refunds >= 0))
})
