test_that("normalize_overhead_manual processes manual overhead data correctly", {
  # Create sample manual overhead data
  manual_data <- data.frame(
    date = as.Date(c("2025-01-15", "2025-02-20")),
    full_account_name = c("Expenses:Rent", "Expenses:Utilities"),
    account_name = c("Rent", "Utilities"),
    description = c("Office rent", "Electric bill"),
    amount = c(-1000, -200),
    category = c("Fixed", "Variable")
  )
  
  result <- normalize_overhead_manual(manual_data, practice_id = 1, source = "manual")
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  
  # Check columns
  expected_cols <- c("practice_id", "date", "week_start", "month", "year",
                     "full_account_name", "account_name", "description", 
                     "amount", "category", "source")
  expect_true(all(expected_cols %in% names(result)))
  
  # Check values
  expect_equal(result$practice_id, c(1, 1))
  expect_equal(result$source, c("manual", "manual"))
  expect_equal(result$month, c(1, 2))
  expect_equal(result$year, c(2025, 2025))
  
  # Check date is Date class
  expect_s3_class(result$date, "Date")
  expect_s3_class(result$week_start, "Date")
})

test_that("normalize_overhead_manual handles missing category column", {
  # Create data without category
  manual_data <- data.frame(
    date = as.Date("2025-01-15"),
    full_account_name = "Expenses:Rent",
    account_name = "Rent",
    description = "Office rent",
    amount = -1000
  )
  
  result <- normalize_overhead_manual(manual_data, practice_id = 1)
  
  expect_true("category" %in% names(result))
  expect_true(is.na(result$category[1]))
})

test_that("normalize_overhead_manual errors on missing required columns", {
  # Create incomplete data
  incomplete_data <- data.frame(
    date = as.Date("2025-01-15"),
    amount = -1000
  )
  
  expect_error(
    normalize_overhead_manual(incomplete_data, practice_id = 1),
    "missing expected columns"
  )
})

test_that("normalize_income_manual processes manual income data correctly", {
  # Create sample manual income data
  manual_data <- data.frame(
    date = as.Date(c("2025-01-15", "2025-02-20")),
    full_account_name = c("Income:Sales", "Income:Consulting"),
    account_name = c("Sales", "Consulting"),
    description = c("Product sale", "Project A"),
    revenue = c(1000, 2000),
    category = c("Product", "Service")
  )
  
  result <- normalize_income_manual(manual_data, practice_id = 1, source = "manual")
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  
  # Check that revenue column exists
  expect_true("revenue" %in% names(result))
  expect_equal(result$revenue, c(1000, 2000))
  
  # Check other columns
  expect_equal(result$practice_id, c(1, 1))
  expect_equal(result$source, c("manual", "manual"))
})

test_that("normalize_income_manual accepts 'amount' as alternative to 'revenue'", {
  # Create data with 'amount' instead of 'revenue'
  manual_data <- data.frame(
    date = as.Date("2025-01-15"),
    full_account_name = "Income:Sales",
    account_name = "Sales",
    description = "Product sale",
    amount = 1000
  )
  
  result <- normalize_income_manual(manual_data, practice_id = 1)
  
  # Should rename 'amount' to 'revenue'
  expect_true("revenue" %in% names(result))
  expect_false("amount" %in% names(result))
  expect_equal(result$revenue, 1000)
})

test_that("normalize_income_manual errors when no revenue/amount column", {
  # Create data without revenue or amount
  incomplete_data <- data.frame(
    date = as.Date("2025-01-15"),
    full_account_name = "Income:Sales",
    account_name = "Sales",
    description = "Product sale"
  )
  
  expect_error(
    normalize_income_manual(incomplete_data, practice_id = 1),
    "revenue.*amount"
  )
})
