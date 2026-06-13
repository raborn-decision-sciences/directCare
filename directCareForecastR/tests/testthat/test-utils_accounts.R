make_raw_tbl <- function(
  accounts = "Expenses:Rent",
  amounts = 1000,
  dates = "01/15/2025"
) {
  tibble::tibble(
    account = accounts,
    amount = amounts,
    date = dates
  )
}

# ── default_account_map ────────────────────────────────────────────────────────

test_that("default_account_map returns a data frame", {
  expect_s3_class(default_account_map(), "data.frame")
})

test_that("default_account_map has required columns", {
  expect_named(
    default_account_map(),
    c("pattern", "category", "match_type", "ignore_case"),
    ignore.order = TRUE
  )
})

test_that("default_account_map pattern and category lengths match", {
  m <- default_account_map()
  expect_equal(length(m$pattern), length(m$category))
})

test_that("default_account_map match_type values are all valid", {
  m <- default_account_map()
  expect_true(all(m$match_type %in% c("contains", "exact", "regex")))
})

test_that("default_account_map ignore_case is logical", {
  m <- default_account_map()
  expect_type(m$ignore_case, "logical")
})

test_that("default_account_map category values are from the documented set", {
  valid <- c(
    "staff",
    "supplies",
    "software",
    "insurance",
    "marketing",
    "labs",
    "equipment",
    "rent",
    "licenses",
    "education",
    "other"
  )
  m <- default_account_map()
  expect_true(all(m$category %in% valid))
})

# ── map_accounts: basic matching ───────────────────────────────────────────────

test_that("map_accounts adds a category column", {
  result <- map_accounts(make_raw_tbl(), default_account_map())
  expect_true("category" %in% names(result))
})

test_that("map_accounts maps 'Salary' to 'staff'", {
  result <- map_accounts(make_raw_tbl("Salary"), default_account_map())
  expect_equal(result$category, "staff")
})

test_that("map_accounts maps 'Rent' to 'rent'", {
  result <- map_accounts(make_raw_tbl("Expenses:Rent"), default_account_map())
  expect_equal(result$category, "rent")
})

test_that("map_accounts maps 'Software' to 'software'", {
  result <- map_accounts(
    make_raw_tbl("Software Subscription"),
    default_account_map()
  )
  expect_equal(result$category, "software")
})

test_that("map_accounts maps 'Insurance' to 'insurance'", {
  result <- map_accounts(
    make_raw_tbl("Malpractice Insurance"),
    default_account_map()
  )
  expect_equal(result$category, "insurance")
})

test_that("map_accounts maps 'Lab' to 'labs'", {
  result <- map_accounts(make_raw_tbl("Lab Fees"), default_account_map())
  expect_equal(result$category, "labs")
})

test_that("map_accounts maps 'Equipment' to 'equipment'", {
  result <- map_accounts(
    make_raw_tbl("Medical Equipment"),
    default_account_map()
  )
  expect_equal(result$category, "equipment")
})

# ── map_accounts: ordering / first-match-wins ──────────────────────────────────

test_that("map_accounts maps 'Equipment Insurance' to 'insurance' (insurance rule precedes equipment)", {
  # The default map deliberately lists insurance before equipment so that
  # "Equipment Insurance" maps to insurance, not equipment.
  result <- map_accounts(
    make_raw_tbl("Equipment Insurance"),
    default_account_map()
  )
  expect_equal(result$category, "insurance")
})

test_that("map_accounts maps 'Equipment Rental' to 'equipment' (equipment rule precedes rent)", {
  result <- map_accounts(
    make_raw_tbl("Equipment Rental"),
    default_account_map()
  )
  expect_equal(result$category, "equipment")
})

# ── map_accounts: case-insensitivity ──────────────────────────────────────────

test_that("map_accounts matching is case-insensitive for default map", {
  upper <- map_accounts(make_raw_tbl("SALARY"), default_account_map())
  lower <- map_accounts(make_raw_tbl("salary"), default_account_map())
  mixed <- map_accounts(make_raw_tbl("Salary"), default_account_map())
  expect_equal(upper$category, "staff")
  expect_equal(lower$category, "staff")
  expect_equal(mixed$category, "staff")
})

# ── map_accounts: unmatched accounts ──────────────────────────────────────────

test_that("map_accounts assigns 'other' to unmatched accounts", {
  result <- suppressWarnings(
    map_accounts(
      make_raw_tbl("Completely Unknown Account"),
      default_account_map()
    )
  )
  expect_equal(result$category, "other")
})

test_that("map_accounts warns on unmatched accounts", {
  expect_snapshot(
    map_accounts(
      make_raw_tbl("Completely Unknown Account"),
      default_account_map()
    )
  )
})

test_that("map_accounts warning has dcForecastR_unmapped_accounts class", {
  w <- tryCatch(
    map_accounts(make_raw_tbl("Unknown Account"), default_account_map()),
    warning = \(w) w
  )
  expect_true(inherits(w, "dcForecastR_unmapped_accounts"))
})

test_that("map_accounts attaches unique unmatched names to the warning", {
  raw <- make_raw_tbl(
    accounts = c("Unknown A", "Unknown A", "Unknown B"),
    amounts = c(100, 200, 300),
    dates = c("01/01/2025", "02/01/2025", "03/01/2025")
  )
  w <- tryCatch(map_accounts(raw, default_account_map()), warning = \(w) w)
  expect_equal(sort(w$unmatched), c("Unknown A", "Unknown B"))
})

# ── map_accounts: mixed matched and unmatched ─────────────────────────────────

test_that("map_accounts correctly categorizes a mixed batch", {
  raw <- tibble::tibble(
    account = c("Salary", "Rent", "Unknown Account"),
    amount = c(5000, 1000, 50),
    date = c("01/01/2025", "01/01/2025", "01/01/2025")
  )
  result <- suppressWarnings(map_accounts(raw, default_account_map()))
  expect_equal(result$category, c("staff", "rent", "other"))
})

# ── map_accounts: input validation ────────────────────────────────────────────

test_that("map_accounts errors when required columns are missing", {
  bad <- tibble::tibble(x = 1, y = 2)
  expect_error(map_accounts(bad, default_account_map()))
})

test_that("map_accounts errors when input is not a data frame", {
  expect_error(map_accounts(
    list(account = "Rent", amount = 100, date = "01/01/2025"),
    default_account_map()
  ))
})

# ── map_accounts: custom account map ──────────────────────────────────────────

test_that("map_accounts respects a custom map passed by the caller", {
  custom_map <- tibble::tibble(
    pattern = "special",
    category = "supplies",
    match_type = "contains",
    ignore_case = TRUE
  )
  result <- map_accounts(make_raw_tbl("Special Order"), custom_map)
  expect_equal(result$category, "supplies")
})

test_that("map_accounts exact match_type only matches the full string", {
  exact_map <- tibble::tibble(
    pattern = "rent",
    category = "rent",
    match_type = "exact",
    ignore_case = TRUE
  )
  # "rent" → matches; "rent payment" → does not match (assigned other)
  exact_hit <- suppressWarnings(
    map_accounts(make_raw_tbl("rent"), exact_map)
  )
  exact_miss <- suppressWarnings(
    map_accounts(make_raw_tbl("rent payment"), exact_map)
  )
  expect_equal(exact_hit$category, "rent")
  expect_equal(exact_miss$category, "other")
})

test_that("map_accounts regex match_type applies perl-compatible patterns", {
  regex_map <- tibble::tibble(
    pattern = "^payroll|^wages",
    category = "staff",
    match_type = "regex",
    ignore_case = FALSE
  )
  raw <- tibble::tibble(
    account = c("payroll taxes", "wages", "Other"),
    amount = c(1000, 2000, 300),
    date = c("01/01/2025", "01/01/2025", "01/01/2025")
  )
  result <- suppressWarnings(map_accounts(raw, regex_map))
  expect_equal(result$category[1], "staff")
  expect_equal(result$category[2], "staff")
  expect_equal(result$category[3], "other")
})
