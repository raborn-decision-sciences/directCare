make_overhead_tbl <- function(n = 3) {
  tibble::tibble(
    practice_id       = rep(1, n),
    date              = seq(as.Date("2025-01-01"), by = "month", length.out = n),
    week_start        = seq(as.Date("2024-12-30"), by = "month", length.out = n),
    month             = seq_len(n),
    year              = rep(2025L, n),
    full_account_name = rep("Expenses:Rent", n),
    account_name      = rep("Rent", n),
    description       = rep("Monthly rent", n),
    amount            = rep(1000, n),
    category          = rep("rent", n),
    source            = "manual"
  )
}

# --- Column validation --------------------------------------------------------

test_that("validate_overhead passes a well-formed tibble", {
  expect_no_error(validate_overhead(make_overhead_tbl()))
})

test_that("validate_overhead errors on missing required columns", {
  bad <- make_overhead_tbl() |> dplyr::select(-amount)
  expect_snapshot(validate_overhead(bad), error = TRUE)
})

test_that("validate_overhead reports all missing columns at once", {
  bad <- tibble::tibble(practice_id = 1, amount = 100)
  err <- tryCatch(validate_overhead(bad), error = \(e) e)
  expect_true(length(err$missing_columns) > 1)
})

test_that("validate_overhead error on missing columns has dcForecastR_missing_columns class", {
  bad <- make_overhead_tbl() |> dplyr::select(-amount)
  err <- tryCatch(validate_overhead(bad), error = \(e) e)
  expect_true(inherits(err, "dcForecastR_missing_columns"))
})

# --- Return value -------------------------------------------------------------

test_that("validate_overhead returns invisibly", {
  result <- withVisible(validate_overhead(make_overhead_tbl()))
  expect_false(result$visible)
})

test_that("validate_overhead adds is_refund column", {
  result <- validate_overhead(make_overhead_tbl())
  expect_true("is_refund" %in% names(result))
})

test_that("validate_overhead is_refund is FALSE for all positive amounts", {
  result <- validate_overhead(make_overhead_tbl())
  expect_true(all(!result$is_refund))
})

test_that("validate_overhead preserves all input rows and columns", {
  input  <- make_overhead_tbl()
  result <- validate_overhead(input)
  expect_equal(nrow(result), nrow(input))
  expect_true(all(names(input) %in% names(result)))
})

# --- Unrecoverable errors -----------------------------------------------------

test_that("validate_overhead errors on NA dates", {
  bad <- make_overhead_tbl()
  bad$date[2] <- NA
  expect_snapshot(validate_overhead(bad), error = TRUE)
})

test_that("validate_overhead errors on NA amounts", {
  bad <- make_overhead_tbl()
  bad$amount[1] <- NA
  expect_snapshot(validate_overhead(bad), error = TRUE)
})

test_that("validate_overhead error message includes the row count for NA amounts", {
  bad <- make_overhead_tbl()
  bad$amount[c(1, 3)] <- NA
  err <- tryCatch(validate_overhead(bad), error = \(e) e)
  expect_match(conditionMessage(err), "2")
})

test_that("validate_overhead NA date error has dcForecastR_invalid_dates class", {
  bad <- make_overhead_tbl()
  bad$date[1] <- NA
  err <- tryCatch(validate_overhead(bad), error = \(e) e)
  expect_true(inherits(err, "dcForecastR_invalid_dates"))
})

test_that("validate_overhead NA amount error has dcForecastR_missing_amounts class", {
  bad <- make_overhead_tbl()
  bad$amount[1] <- NA
  err <- tryCatch(validate_overhead(bad), error = \(e) e)
  expect_true(inherits(err, "dcForecastR_missing_amounts"))
})

# --- Recoverable warnings -----------------------------------------------------

test_that("validate_overhead warns on negative amounts", {
  overhead <- make_overhead_tbl()
  overhead$amount[2] <- -200
  expect_snapshot(validate_overhead(overhead))
})

test_that("validate_overhead tags is_refund correctly for negative amounts", {
  overhead <- make_overhead_tbl()
  overhead$amount[2] <- -200
  result <- suppressWarnings(validate_overhead(overhead))
  expect_true(result$is_refund[2])
  expect_false(result$is_refund[1])
})

test_that("validate_overhead attaches the refund rows to the warning", {
  overhead <- make_overhead_tbl()
  overhead$amount[2] <- -200
  w <- tryCatch(validate_overhead(overhead), warning = \(w) w)
  expect_false(is.null(w$refunds))
  expect_equal(nrow(w$refunds), 1)
})

test_that("validate_overhead refund warning has dcForecastR_refunds_detected class", {
  overhead <- make_overhead_tbl()
  overhead$amount[2] <- -200
  w <- tryCatch(validate_overhead(overhead), warning = \(w) w)
  expect_true(inherits(w, "dcForecastR_refunds_detected"))
})

test_that("validate_overhead warns on zero amounts", {
  overhead <- make_overhead_tbl()
  overhead$amount[1] <- 0
  expect_snapshot(validate_overhead(overhead))
})

test_that("validate_overhead zero warning has dcForecastR_zero_amounts class", {
  overhead <- make_overhead_tbl()
  overhead$amount[1] <- 0
  w <- tryCatch(validate_overhead(overhead), warning = \(w) w)
  expect_true(inherits(w, "dcForecastR_zero_amounts"))
})

test_that("validate_overhead warns on future dates", {
  overhead <- make_overhead_tbl()
  overhead$date[1] <- as.Date("2099-01-01")
  expect_snapshot(validate_overhead(overhead))
})

test_that("validate_overhead future date warning has dcForecastR_future_dates class", {
  overhead <- make_overhead_tbl()
  overhead$date[1] <- as.Date("2099-01-01")
  w <- tryCatch(validate_overhead(overhead), warning = \(w) w)
  expect_true(inherits(w, "dcForecastR_future_dates"))
})

test_that("validate_overhead attaches future rows to the warning", {
  overhead <- make_overhead_tbl()
  overhead$date[1] <- as.Date("2099-01-01")
  w <- tryCatch(validate_overhead(overhead), warning = \(w) w)
  expect_equal(nrow(w$future_rows), 1)
})

# --- Integration: filter_gnucash_overhead wires in validate_overhead ----------

test_that("filter_gnucash_overhead adds is_refund via validate_overhead", {
  transactions <- tibble::tibble(
    practice_id       = 1,
    date              = as.Date("2025-01-15"),
    week_start        = as.Date("2025-01-13"),
    month             = 1L, year = 2025L,
    full_account_name = "Expenses:Rent",
    account_name      = "Rent",
    description       = "Rent",
    amount            = 1000,
    category          = "rent",
    source            = "gnucash_csv"
  )
  result <- filter_gnucash_overhead(transactions)
  expect_true("is_refund" %in% names(result))
  expect_false(result$is_refund[1])
})
