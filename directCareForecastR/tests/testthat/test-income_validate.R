make_income_tbl <- function(n = 3) {
  tibble::tibble(
    practice_id       = rep(1, n),
    date              = seq(as.Date("2025-01-01"), by = "month", length.out = n),
    week_start        = seq(as.Date("2024-12-30"), by = "month", length.out = n),
    month             = seq_len(n),
    year              = rep(2025L, n),
    full_account_name = rep("Income:Membership", n),
    account_name      = rep("Membership", n),
    description       = rep("Monthly fee", n),
    revenue           = rep(1000, n),
    category          = rep("other", n),
    source            = "manual"
  )
}

# --- Column validation --------------------------------------------------------

test_that("validate_income passes a well-formed tibble", {
  expect_no_error(validate_income(make_income_tbl()))
})

test_that("validate_income errors on missing required columns", {
  bad <- make_income_tbl() |> dplyr::select(-revenue)

  expect_error(
    validate_income(bad),
    class = "dcForecastR_missing_columns"
  )
})

test_that("validate_income reports all missing columns at once", {
  bad <- tibble::tibble(practice_id = 1, revenue = 100)

  err <- tryCatch(validate_income(bad), error = function(e) e)
  expect_true(length(err$missing_columns) > 1)
})

# --- Return value -------------------------------------------------------------

test_that("validate_income returns invisibly", {
  result <- withVisible(validate_income(make_income_tbl()))
  expect_false(result$visible)
})

test_that("validate_income adds is_refund column", {
  result <- validate_income(make_income_tbl())
  expect_true("is_refund" %in% names(result))
})

test_that("validate_income is_refund is FALSE for all positive revenue", {
  result <- validate_income(make_income_tbl())
  expect_true(all(!result$is_refund))
})

test_that("validate_income preserves all input rows and columns", {
  input  <- make_income_tbl()
  result <- validate_income(input)

  expect_equal(nrow(result), nrow(input))
  expect_true(all(names(input) %in% names(result)))
})

# --- Unrecoverable errors -----------------------------------------------------

test_that("validate_income errors on NA dates", {
  bad <- make_income_tbl()
  bad$date[2] <- NA

  expect_error(validate_income(bad), class = "dcForecastR_invalid_dates")
})

test_that("validate_income errors on NA revenue", {
  bad <- make_income_tbl()
  bad$revenue[1] <- NA

  expect_error(validate_income(bad), class = "dcForecastR_missing_amounts")
})

test_that("validate_income error message includes the row count", {
  bad <- make_income_tbl()
  bad$revenue[c(1, 3)] <- NA

  err <- tryCatch(validate_income(bad), error = function(e) e)
  expect_match(conditionMessage(err), "2")
})

# --- Recoverable warnings -----------------------------------------------------

test_that("validate_income warns on negative revenue and tags is_refund", {
  income <- make_income_tbl()
  income$revenue[2] <- -100

  expect_warning(
    result <- validate_income(income),
    class = "dcForecastR_refunds_detected"
  )
  expect_true(result$is_refund[2])
  expect_false(result$is_refund[1])
})

test_that("validate_income attaches the refund rows to the warning", {
  income <- make_income_tbl()
  income$revenue[2] <- -100

  w <- tryCatch(validate_income(income), warning = function(w) w)
  expect_true(!is.null(w$refunds))
  expect_equal(nrow(w$refunds), 1)
})

test_that("validate_income warns on zero revenue", {
  income <- make_income_tbl()
  income$revenue[1] <- 0

  expect_warning(validate_income(income), class = "dcForecastR_zero_amounts")
})

test_that("validate_income warns on future dates", {
  income <- make_income_tbl()
  income$date[1] <- Sys.Date() + 30

  expect_warning(validate_income(income), class = "dcForecastR_future_dates")
})

test_that("validate_income attaches future rows to the warning", {
  income <- make_income_tbl()
  income$date[1] <- Sys.Date() + 30

  w <- tryCatch(validate_income(income), warning = function(w) w)
  expect_equal(nrow(w$future_rows), 1)
})

# --- Integration: ingest_manual wires in validate_income ---------------------

test_that("ingest_manual type = 'income' returns is_refund column", {
  df <- data.frame(
    date              = as.Date(c("2025-01-15", "2025-02-20")),
    full_account_name = c("Income:Membership", "Income:Membership"),
    account_name      = c("Membership", "Membership"),
    description       = c("Fee", "Fee"),
    revenue           = c(1000, 1200)
  )

  result <- ingest_manual(df, practice_id = 1, type = "income")
  expect_true("is_refund" %in% names(result))
  expect_true(all(!result$is_refund))
})

test_that("ingest_manual type = 'income' warns on negative revenue", {
  df <- data.frame(
    date              = as.Date(c("2025-01-15", "2025-02-20")),
    full_account_name = c("Income:Membership", "Income:Membership"),
    account_name      = c("Membership", "Membership"),
    description       = c("Fee", "Chargeback"),
    revenue           = c(1000, -200)
  )

  expect_warning(
    result <- ingest_manual(df, practice_id = 1, type = "income"),
    class = "dcForecastR_refunds_detected"
  )
  expect_true(result$is_refund[2])
})

test_that("ingest_manual type = 'overhead' does NOT call validate_income", {
  df <- data.frame(
    date              = as.Date("2025-01-15"),
    full_account_name = "Expenses:Rent",
    account_name      = "Rent",
    description       = "Office rent",
    amount            = 1000
  )

  # No is_refund column added — overhead path leaves that to validate_overhead
  result <- ingest_manual(df, practice_id = 1, type = "overhead")
  expect_false("is_refund" %in% names(result))
})
