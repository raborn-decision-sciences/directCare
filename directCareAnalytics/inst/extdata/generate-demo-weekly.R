# Generate demo-gnucash-weekly.csv
#
# Explicit model assumptions
# --------------------------
# Monthly membership fee : $85 / member
# Panel growth           : 32 members (Apr 2024) -> 85 members (Mar 2026)
# Weekly Stripe payment  : panel_size x $85 / 4.33 weeks/month (+/- 3% noise)
# FFS income             : sporadic ElationHealth payments, ~40% of weekdays,
#                          $50-$150 scaled lightly with panel size
# Payroll                : bi-weekly (1st & 15th), $906 -> $1,050 over 2 years
# Rent                   : $722 / month, flat
# Utilities              : $110-$145 / month (seasonal)
# EHR (Elation Health)   : $171 / month, rises to $225 from Jun 2025
# Malpractice insurance  : $436 / quarter (Jan, Apr, Jul, Oct)
# Medical supplies       : sporadic, $15-$80, ~70% of months
# Labs                   : sporadic, $20-$120, ~40% of months

set.seed(2024)

FEE <- 85 # monthly membership fee per member ($)
START_DATE <- as.Date("2024-04-01")
END_DATE <- as.Date("2026-03-31")

# Panel size by calendar month (YYYY-MM)
panel_by_month <- c(
  "2024-04" = 32,
  "2024-05" = 35,
  "2024-06" = 38,
  "2024-07" = 41,
  "2024-08" = 44,
  "2024-09" = 48,
  "2024-10" = 52,
  "2024-11" = 55,
  "2024-12" = 57,
  "2025-01" = 62, # January HSA/enrollment bump
  "2025-02" = 65,
  "2025-03" = 68,
  "2025-04" = 70,
  "2025-05" = 72,
  "2025-06" = 74,
  "2025-07" = 75,
  "2025-08" = 77,
  "2025-09" = 79,
  "2025-10" = 81,
  "2025-11" = 82,
  "2025-12" = 83,
  "2026-01" = 88, # January HSA/enrollment bump
  "2026-02" = 87,
  "2026-03" = 85
)

# ---- helpers -----------------------------------------------------------------

new_id <- function() {
  paste0(sample(c(letters[1:6], 0:9), 32, replace = TRUE), collapse = "")
}

fmt_sym <- function(x) sprintf("$%.2f", x)

# One double-entry pair (account row + Imbalance-USD offset row)
txn_rows <- function(date, description, full_acct, acct_name, amount) {
  id <- new_id()
  ds <- format(date, "%m/%d/%Y")
  pos <- fmt_sym(amount)
  neg <- fmt_sym(-amount)
  base <- data.frame(
    "Date" = ds,
    "Transaction ID" = id,
    "Number" = "",
    "Description" = description,
    "Notes" = "",
    "Commodity/Currency" = "CURRENCY::USD",
    "Void Reason" = "",
    "Action" = "",
    "Memo" = "",
    "Full Account Name" = NA_character_,
    "Account Name" = NA_character_,
    "Amount With Sym" = NA_character_,
    "Amount Num." = NA_real_,
    "Value With Sym" = NA_character_,
    "Value Num." = NA_real_,
    "Reconcile" = "n",
    "Reconcile Date" = "",
    "Rate/Price" = "1.00",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  r1 <- base
  r2 <- base
  r1[["Full Account Name"]] <- full_acct
  r1[["Account Name"]] <- acct_name
  r1[["Amount With Sym"]] <- pos
  r1[["Amount Num."]] <- amount
  r1[["Value With Sym"]] <- pos
  r1[["Value Num."]] <- amount
  r2[["Full Account Name"]] <- "Imbalance-USD"
  r2[["Account Name"]] <- "Imbalance-USD"
  r2[["Amount With Sym"]] <- neg
  r2[["Amount Num."]] <- -amount
  r2[["Value With Sym"]] <- neg
  r2[["Value Num."]] <- -amount
  rbind(r1, r2)
}

panel_for <- function(date) {
  ym <- format(date, "%Y-%m")
  unname(panel_by_month[ym])
}

# ---- build transactions ------------------------------------------------------

all_dates <- seq(START_DATE, END_DATE, by = "day")
mondays <- all_dates[format(all_dates, "%u") == "1"]

rows <- list()

# Weekly Stripe membership payments (every Monday)
for (d in mondays) {
  d <- as.Date(d, origin = "1970-01-01")
  p <- panel_for(d)
  if (is.na(p)) {
    next
  }
  # panel x fee / 4.33 weeks per month; +/- ~3% payment-timing noise
  weekly_rev <- round(p * FEE / 4.33 * rnorm(1, 1, 0.03), 2)
  rows[[length(rows) + 1]] <- txn_rows(
    d,
    "Online Transfer / Payment: Credit from STRIPE",
    "Income:Membership Fees",
    "Membership Fees",
    weekly_rev
  )
}

# Sporadic FFS payments (Tue-Thu, ~40% of those days)
ffs_candidates <- all_dates[format(all_dates, "%u") %in% c("2", "3", "4")]
ffs_dates <- sort(sample(ffs_candidates, round(length(ffs_candidates) * 0.40)))
for (d in ffs_dates) {
  d <- as.Date(d, origin = "1970-01-01")
  p <- panel_for(d)
  if (is.na(p)) {
    next
  }
  # Amount scales mildly with panel (more patients -> more FFS opportunities)
  amt <- round(runif(1, 50, 150) * (1 + (p - 32) / 150), 2)
  rows[[length(rows) + 1]] <- txn_rows(
    d,
    "Online Transfer / Payment: Credit from ElationHealth",
    "Income:Fee-for-Service",
    "Fee-for-Service",
    amt
  )
}

# Monthly overhead
month_starts <- seq(START_DATE, as.Date("2026-03-01"), by = "month")

supply_items <- c(
  "Specula kit",
  "Exam table paper roll",
  "Hand sanitizer",
  "Nitrile gloves",
  "Bandage supplies",
  "BP cuff calibration kit",
  "Otoscope tips",
  "Lancets and strips",
  "Tongue depressors"
)

for (mo in month_starts) {
  mo <- as.Date(mo, origin = "1970-01-01")
  mo_num <- as.integer(format(mo, "%m"))
  elapsed_months <- as.numeric(difftime(mo, START_DATE, units = "days")) / 30.44

  # Rent (1st of month)
  rows[[length(rows) + 1]] <- txn_rows(
    mo,
    "Zelle Payment to Riverside Medical - Rent",
    "Expenses:Rent and Utilities:Rent",
    "Rent",
    722.00
  )

  # Utilities (1st; seasonal)
  util_lo <- if (mo_num %in% c(12, 1, 2)) 130 else 110
  util_hi <- if (mo_num %in% c(12, 1, 2)) 145 else 130
  rows[[length(rows) + 1]] <- txn_rows(
    mo,
    "Zelle Payment to Riverside Medical - Utilities",
    "Expenses:Rent and Utilities:Utilities",
    "Utilities",
    round(runif(1, util_lo, util_hi))
  )

  # EHR subscription (10th; price increase Jun 2025)
  ehr_fee <- if (mo >= as.Date("2025-06-01")) 225 else 171
  rows[[length(rows) + 1]] <- txn_rows(
    mo + 9,
    "ELATION HEALTH SUBSCRIPTION",
    "Expenses:Software and Subscriptions",
    "Software and Subscriptions",
    ehr_fee
  )

  # Malpractice insurance (2nd; quarterly Jan/Apr/Jul/Oct)
  if (mo_num %in% c(1, 4, 7, 10)) {
    rows[[length(rows) + 1]] <- txn_rows(
      mo + 1,
      "PHYSICIANS INSURANCE GROUP - Malpractice",
      "Expenses:Malpractice Insurance",
      "Malpractice Insurance",
      436.00
    )
  }

  # Payroll: bi-weekly (1st & 15th); $906 -> $1,050 linear growth over 24 months
  payroll_amt <- round(906 + elapsed_months * (1050 - 906) / 24)
  rows[[length(rows) + 1]] <- txn_rows(
    mo,
    "Zelle Payment to Jordan Rivera - Payroll",
    "Expenses:Payroll Expenses",
    "Payroll Expenses",
    payroll_amt
  )
  rows[[length(rows) + 1]] <- txn_rows(
    mo + 14,
    "Zelle Payment to Jordan Rivera - Payroll",
    "Expenses:Payroll Expenses",
    "Payroll Expenses",
    payroll_amt
  )

  # Medical supplies (~70% of months)
  if (runif(1) < 0.70) {
    rows[[length(rows) + 1]] <- txn_rows(
      mo + sample(2:25, 1),
      sample(supply_items, 1),
      "Expenses:Medical Supplies",
      "Medical Supplies",
      round(runif(1, 15, 80), 2)
    )
  }

  # Labs (~40% of months)
  if (runif(1) < 0.40) {
    rows[[length(rows) + 1]] <- txn_rows(
      mo + sample(5:25, 1),
      "Quest Diagnostics - Lab fees",
      "Expenses:Labs",
      "Labs",
      round(runif(1, 20, 120), 2)
    )
  }
}

# ---- assemble and write -------------------------------------------------------

result <- do.call(rbind, rows)
result <- result[
  order(as.Date(result[["Date"]], "%m/%d/%Y"), result[["Transaction ID"]]),
]
rownames(result) <- NULL

out <- file.path(
  dirname(normalizePath("generate-demo-weekly.R")),
  "demo-gnucash-weekly.csv"
)
write.csv(result, out, row.names = FALSE, quote = TRUE)

# ---- summary -----------------------------------------------------------------
mbr <- result[
  result[["Account Name"]] == "Membership Fees" &
    result[["Amount Num."]] > 0,
]
cat(sprintf(
  "Written %d rows to %s\n\nModel assumptions:\n  Fee:         $%d/member/month\n  Start panel: %d members (Apr 2024) -> ~$%d/week\n  End panel:   %d members (Mar 2026) -> ~$%d/week\n\nObserved membership revenue:\n  Min weekly:  $%.2f\n  Max weekly:  $%.2f\n  Mean weekly: $%.2f\n",
  nrow(result),
  basename(out),
  FEE,
  panel_by_month[["2024-04"]],
  round(panel_by_month[["2024-04"]] * FEE / 4.33),
  panel_by_month[["2026-03"]],
  round(panel_by_month[["2026-03"]] * FEE / 4.33),
  min(mbr[["Amount Num."]]),
  max(mbr[["Amount Num."]]),
  mean(mbr[["Amount Num."]])
))
