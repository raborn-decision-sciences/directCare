# Helpers ----------------------------------------------------------------------

make_monthly_income_tbl <- function() {
  tibble::tibble(
    practice_id = c(1, 1, 1, 1),
    date = as.Date(c("2025-01-10", "2025-01-20", "2025-02-05", "2025-02-15")),
    week_start = as.Date(c(
      "2025-01-06",
      "2025-01-20",
      "2025-02-03",
      "2025-02-10"
    )),
    full_account_name = rep("Income:Membership", 4),
    account_name = rep("Membership", 4),
    description = rep("Monthly fee", 4),
    revenue = c(1000, 500, 1000, 500),
    month = c(1L, 1L, 2L, 2L),
    year = c(2025L, 2025L, 2025L, 2025L),
    category = rep("other", 4),
    source = "gnucash_csv",
    is_refund = c(FALSE, FALSE, FALSE, FALSE)
  )
}

make_weekly_income_tbl <- function() {
  weeks <- seq(as.Date("2025-01-06"), by = "week", length.out = 4)
  tibble::tibble(
    practice_id = rep(1, 4),
    date = weeks,
    week_start = weeks,
    full_account_name = rep("Income:Membership", 4),
    account_name = rep("Membership", 4),
    description = rep("Monthly fee", 4),
    revenue = c(500, 600, 700, 800),
    month = c(1L, 1L, 1L, 2L),
    year = c(2025L, 2025L, 2025L, 2025L),
    category = rep("other", 4),
    source = "gnucash_csv",
    is_refund = c(FALSE, FALSE, FALSE, FALSE)
  )
}

# =============================================================================
# summarize_income_monthly
# =============================================================================

test_that("summarize_income_monthly returns correct structure", {
  result <- summarize_income_monthly(make_monthly_income_tbl())

  expect_s3_class(result, "data.frame")
  expected_cols <- c(
    "practice_id",
    "year",
    "month",
    "total_revenue",
    "gross_revenue",
    "total_refunds"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("summarize_income_monthly aggregates to one row per month", {
  result <- summarize_income_monthly(make_monthly_income_tbl())
  expect_equal(nrow(result), 2)
})

test_that("summarize_income_monthly sums revenue correctly", {
  result <- summarize_income_monthly(make_monthly_income_tbl())

  jan <- dplyr::filter(result, month == 1)
  feb <- dplyr::filter(result, month == 2)

  expect_equal(jan$total_revenue, 1500)
  expect_equal(feb$total_revenue, 1500)
})

test_that("summarize_income_monthly gross_revenue equals total_revenue when no refunds", {
  result <- summarize_income_monthly(make_monthly_income_tbl())
  expect_equal(result$gross_revenue, result$total_revenue)
  expect_equal(result$total_refunds, c(0, 0))
})

test_that("summarize_income_monthly handles refunds correctly", {
  income <- make_monthly_income_tbl()
  # Add a refund row in January
  refund_row <- tibble::tibble(
    practice_id = 1,
    date = as.Date("2025-01-25"),
    week_start = as.Date("2025-01-20"),
    full_account_name = "Income:Membership",
    account_name = "Membership",
    description = "Cancellation refund",
    revenue = -200,
    month = 1L,
    year = 2025L,
    category = "other",
    source = "gnucash_csv",
    is_refund = TRUE
  )
  income <- dplyr::bind_rows(income, refund_row)

  result <- summarize_income_monthly(income)
  jan <- dplyr::filter(result, month == 1)

  expect_equal(jan$gross_revenue, 1500) # 1000 + 500, ignoring refund
  expect_equal(jan$total_refunds, 200) # magnitude of refund
  expect_equal(jan$total_revenue, 1300) # 1500 - 200
})

test_that("summarize_income_monthly derives is_refund when column is absent", {
  income <- make_monthly_income_tbl()
  # Add a negative row and drop is_refund column
  income <- income |>
    dplyr::bind_rows(tibble::tibble(
      practice_id = 1,
      date = as.Date("2025-01-25"),
      week_start = as.Date("2025-01-20"),
      full_account_name = "Income:Membership",
      account_name = "Membership",
      description = "Refund",
      revenue = -100,
      month = 1L,
      year = 2025L,
      category = "other",
      source = "gnucash_csv",
      is_refund = TRUE
    )) |>
    dplyr::select(-is_refund)

  result <- summarize_income_monthly(income)
  jan <- dplyr::filter(result, month == 1)

  expect_equal(jan$gross_revenue, 1500)
  expect_equal(jan$total_refunds, 100)
  expect_equal(jan$total_revenue, 1400)
})

test_that("summarize_income_monthly include_refunds = FALSE excludes refund rows", {
  income <- make_monthly_income_tbl()
  refund_row <- tibble::tibble(
    practice_id = 1,
    date = as.Date("2025-01-25"),
    week_start = as.Date("2025-01-20"),
    full_account_name = "Income:Membership",
    account_name = "Membership",
    description = "Refund",
    revenue = -200,
    month = 1L,
    year = 2025L,
    category = "other",
    source = "gnucash_csv",
    is_refund = TRUE
  )
  income <- dplyr::bind_rows(income, refund_row)

  result <- summarize_income_monthly(income, include_refunds = FALSE)
  jan <- dplyr::filter(result, month == 1)

  expect_equal(jan$total_revenue, 1500) # refund row excluded entirely
})

test_that("summarize_income_monthly groups by practice_id", {
  income <- dplyr::bind_rows(
    make_monthly_income_tbl(),
    dplyr::mutate(make_monthly_income_tbl(), practice_id = 2)
  )

  result <- summarize_income_monthly(income)
  expect_equal(nrow(result), 4) # 2 practices × 2 months
  expect_equal(sort(unique(result$practice_id)), c(1, 2))
})

test_that("summarize_income_monthly errors on missing required columns", {
  bad <- tibble::tibble(practice_id = 1, year = 2025, revenue = 1000)

  expect_error(
    summarize_income_monthly(bad),
    class = "dcForecastR_missing_columns"
  )
})

test_that("summarize_income_monthly total_refunds is always non-negative", {
  income <- make_monthly_income_tbl()
  refund_row <- tibble::tibble(
    practice_id = 1,
    date = as.Date("2025-01-25"),
    week_start = as.Date("2025-01-20"),
    full_account_name = "Income:Membership",
    account_name = "Membership",
    description = "Refund",
    revenue = -300,
    month = 1L,
    year = 2025L,
    category = "other",
    source = "gnucash_csv",
    is_refund = TRUE
  )
  income <- dplyr::bind_rows(income, refund_row)

  result <- summarize_income_monthly(income)
  expect_true(all(result$total_refunds >= 0))
})

# =============================================================================
# summarize_income_weekly
# =============================================================================

test_that("summarize_income_weekly returns correct structure", {
  result <- summarize_income_weekly(make_weekly_income_tbl())

  expect_s3_class(result, "data.frame")
  expected_cols <- c(
    "practice_id",
    "week_start",
    "total_revenue",
    "gross_revenue",
    "total_refunds"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("summarize_income_weekly aggregates to one row per week", {
  result <- summarize_income_weekly(make_weekly_income_tbl())
  expect_equal(nrow(result), 4) # 4 distinct weeks
})

test_that("summarize_income_weekly sums revenue within a week correctly", {
  # Two transactions in the same week
  income <- tibble::tibble(
    practice_id = c(1, 1),
    date = as.Date(c("2025-01-06", "2025-01-08")),
    week_start = as.Date(c("2025-01-06", "2025-01-06")),
    full_account_name = rep("Income:Membership", 2),
    account_name = rep("Membership", 2),
    description = rep("Fee", 2),
    revenue = c(400, 600),
    month = c(1L, 1L),
    year = c(2025L, 2025L),
    category = rep("other", 2),
    source = "gnucash_csv",
    is_refund = c(FALSE, FALSE)
  )

  result <- summarize_income_weekly(income)
  expect_equal(nrow(result), 1)
  expect_equal(result$total_revenue, 1000)
})

test_that("summarize_income_weekly handles refunds correctly", {
  income <- make_weekly_income_tbl()
  refund_row <- tibble::tibble(
    practice_id = 1,
    date = as.Date("2025-01-06"),
    week_start = as.Date("2025-01-06"),
    full_account_name = "Income:Membership",
    account_name = "Membership",
    description = "Refund",
    revenue = -100,
    month = 1L,
    year = 2025L,
    category = "other",
    source = "gnucash_csv",
    is_refund = TRUE
  )
  income <- dplyr::bind_rows(income, refund_row)

  result <- summarize_income_weekly(income)
  week1 <- dplyr::filter(result, week_start == as.Date("2025-01-06"))

  expect_equal(week1$gross_revenue, 500) # original row only
  expect_equal(week1$total_refunds, 100)
  expect_equal(week1$total_revenue, 400)
})

test_that("summarize_income_weekly derives is_refund when column is absent", {
  income <- make_weekly_income_tbl() |>
    dplyr::bind_rows(tibble::tibble(
      practice_id = 1,
      date = as.Date("2025-01-06"),
      week_start = as.Date("2025-01-06"),
      full_account_name = "Income:Membership",
      account_name = "Membership",
      description = "Refund",
      revenue = -50,
      month = 1L,
      year = 2025L,
      category = "other",
      source = "gnucash_csv",
      is_refund = TRUE
    )) |>
    dplyr::select(-is_refund)

  result <- summarize_income_weekly(income)
  week1 <- dplyr::filter(result, week_start == as.Date("2025-01-06"))

  expect_equal(week1$gross_revenue, 500)
  expect_equal(week1$total_refunds, 50)
  expect_equal(week1$total_revenue, 450)
})

test_that("summarize_income_weekly include_refunds = FALSE excludes refund rows", {
  income <- make_weekly_income_tbl()
  refund_row <- tibble::tibble(
    practice_id = 1,
    date = as.Date("2025-01-06"),
    week_start = as.Date("2025-01-06"),
    full_account_name = "Income:Membership",
    account_name = "Membership",
    description = "Refund",
    revenue = -100,
    month = 1L,
    year = 2025L,
    category = "other",
    source = "gnucash_csv",
    is_refund = TRUE
  )
  income <- dplyr::bind_rows(income, refund_row)

  result <- summarize_income_weekly(income, include_refunds = FALSE)
  week1 <- dplyr::filter(result, week_start == as.Date("2025-01-06"))

  expect_equal(week1$total_revenue, 500) # refund row excluded
})

test_that("summarize_income_weekly week_start column is Date class", {
  result <- summarize_income_weekly(make_weekly_income_tbl())
  expect_s3_class(result$week_start, "Date")
})

test_that("summarize_income_weekly groups by practice_id", {
  income <- dplyr::bind_rows(
    make_weekly_income_tbl(),
    dplyr::mutate(make_weekly_income_tbl(), practice_id = 2)
  )

  result <- summarize_income_weekly(income)
  expect_equal(nrow(result), 8) # 2 practices × 4 weeks
  expect_equal(sort(unique(result$practice_id)), c(1, 2))
})

test_that("summarize_income_weekly errors on missing required columns", {
  bad <- tibble::tibble(practice_id = 1, revenue = 500)

  expect_error(
    summarize_income_weekly(bad),
    class = "dcForecastR_missing_columns"
  )
})

test_that("summarize_income_weekly total_refunds is always non-negative", {
  income <- make_weekly_income_tbl()
  refund_row <- tibble::tibble(
    practice_id = 1,
    date = as.Date("2025-01-06"),
    week_start = as.Date("2025-01-06"),
    full_account_name = "Income:Membership",
    account_name = "Membership",
    description = "Refund",
    revenue = -400,
    month = 1L,
    year = 2025L,
    category = "other",
    source = "gnucash_csv",
    is_refund = TRUE
  )
  income <- dplyr::bind_rows(income, refund_row)

  result <- summarize_income_weekly(income)
  expect_true(all(result$total_refunds >= 0))
})
