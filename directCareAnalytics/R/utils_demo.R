# -- Demo mode -----------------------------------------------------------------
# Populates the shared `r` state from a bundled fixture so a `?demo=1` session
# lands in the same state a real GnuCash CSV upload would produce, without
# requiring login or a real upload. Mirrors mod_upload.R's GnuCash CSV path
# (ingest -> filter/normalize -> summarize) exactly, so demo sessions exercise
# the real data pipeline rather than a hand-built stub.

#' Populate shared state with the bundled demo dataset
#'
#' @param r Shared `reactiveValues` object from `app_server()`.
#' @noRd
.load_demo_data <- function(r) {
  r$practice_id <- "riverside-demo"
  r$practice_name <- "Riverside Direct Care (Demo)"

  csv_path <- system.file(
    "extdata",
    "demo-gnucash.csv",
    package = "directCareAnalytics"
  )

  transactions <- directCareForecastR::ingest_gnucash_csv(
    path = csv_path,
    practice_id = r$practice_id
  )

  # Trim to a ~6-month window (the fixture's own range runs well past this)
  # and tell a simple, legible story: overhead exists from day one, but
  # revenue starts at $0 through the end of 2025, as if the practice were
  # covering rent/EHR/etc. before actually opening its doors and billing in
  # January. Zeroing `amount` (rather than dropping the rows) keeps every
  # month present in both series after summarizing -- summarize_*_monthly()
  # group by year/month and only emit a row for months that have at least
  # one row, so dropping rows entirely would make Oct-Dec income silently
  # *disappear* rather than read as $0, and would leave overhead_monthly
  # and income_monthly covering different month ranges.
  demo_end <- as.Date("2026-03-31")
  demo_income_start <- as.Date("2026-01-01")
  transactions <- dplyr::filter(transactions, date <= demo_end)
  is_income_row <- grepl("Income", transactions$full_account_name, fixed = TRUE)
  transactions$amount[is_income_row & transactions$date < demo_income_start] <- 0

  overhead <- directCareForecastR::filter_gnucash_overhead(transactions)
  income <- directCareForecastR::normalize_gnucash_income(transactions)

  r$transactions <- transactions
  r$overhead <- overhead
  r$income <- income
  r$overhead_monthly <- directCareForecastR::summarize_overhead_monthly(overhead)
  r$income_monthly <- directCareForecastR::summarize_income_monthly(income)
  r$validation <- list()
}
