# directCareForecastR — Internal Developer Reference

**Internal package. Not published to CRAN.**
Backend for the `directCareAnalytics` Shiny app. Handles all data ingestion,
normalization, validation, and financial forecasting for Direct Primary Care
(DPC) practices.

---

## What it does

DPC practices operate outside insurance, relying on recurring membership fees
and occasional fee-for-service visits. This package answers the two financial
questions every new DPC practice needs to track:

1. **Break-even** — when will membership revenue cover monthly overhead?
2. **Target** — when will net income reach a specific goal?

Data flows in from bookkeeping exports (GnuCash CSV today; QuickBooks and
generic CSV are stubbed). The package normalizes it into a common schema,
summarizes it by period, and projects it forward using linear regression, ETS,
or ARIMA.

---

## Pipeline

```
Bookkeeping file / manual data frame
          │
          ▼
   ingest_gnucash_csv()          ingest_manual()
   ingest_manual()
          │
          ▼
   [normalized transaction tibble]   ← validate_overhead() / validate_income()
     practice_id | date | week_start | month | year
     full_account_name | account_name | description
     amount / revenue | category | source | is_refund
          │
     ┌────┴────┐
     ▼         ▼
filter_gnucash_overhead()    normalize_gnucash_income()
     │                                │
     ▼                                ▼
summarize_overhead_monthly()   summarize_income_monthly()
                                summarize_income_weekly()
     │                                │
     └──────────────┬─────────────────┘
                    ▼
         forecast_breakeven()
         forecast_revenue()
         forecast_target()
```

---

## Data schemas

### Normalized transaction tibble
Output of `ingest_gnucash_csv()` and `ingest_manual()`. Both income and expense
rows are present until filtered.

| Column | Type | Notes |
|---|---|---|
| `practice_id` | chr / int | Passed in by caller |
| `date` | Date | Parsed from source |
| `week_start` | Date | Monday of the transaction's week |
| `month` | int | 1–12 |
| `year` | int | Calendar year |
| `full_account_name` | chr | e.g. `"Expenses:Rent"`, `"Income:Membership"` |
| `account_name` | chr | Leaf account name |
| `description` | chr | Transaction memo |
| `amount` / `revenue` | dbl | `amount` for overhead path; renamed to `revenue` by `normalize_gnucash_income()` |
| `category` | chr | Mapped by `map_accounts()`; `"other"` if unmapped |
| `source` | chr | `"gnucash_csv"` or `"manual"` |
| `is_refund` | lgl | Added by `validate_overhead()` / `validate_income()`; `TRUE` when amount/revenue < 0 |

### Monthly overhead summary
Output of `summarize_overhead_monthly()`. Fed into `forecast_breakeven()` and
`forecast_target()`.

| Column | Type | Notes |
|---|---|---|
| `practice_id` | chr / int | |
| `year` | int | |
| `month` | int | |
| `total_overhead` | dbl | Net overhead after refunds (`sum(amount)`) |
| `gross_overhead` | dbl | Positive expenses only |
| `total_refunds` | dbl | Magnitude of credits; always ≥ 0 |

### Monthly / weekly income summary
Output of `summarize_income_monthly()` and `summarize_income_weekly()`.

| Column | Type | Notes |
|---|---|---|
| `practice_id` | chr / int | |
| `year` | int | Monthly only |
| `month` | int | Monthly only |
| `week_start` | Date | Weekly only |
| `total_revenue` | dbl | Net revenue after chargebacks |
| `gross_revenue` | dbl | Positive transactions only |
| `total_refunds` | dbl | Magnitude of chargebacks; always ≥ 0 |

---

## Amount sign convention

| Sign | Meaning |
|---|---|
| Positive | Normal expense (overhead) or normal revenue (income) |
| Negative | Refund/credit (overhead) or chargeback/cancellation (income) |

`is_refund = amount < 0` for overhead; `is_refund = revenue < 0` for income.
`total_refunds` is always reported as a positive magnitude (the sign is
negated during summarization).

---

## Function reference

### Ingest

| Function | Description |
|---|---|
| `ingest_gnucash_csv(path, practice_id, account_map)` | Read a GnuCash CSV export. Returns full transaction tibble (income + expenses). Calls `map_accounts()` then `validate_overhead()` internally. |
| `ingest_manual(df, practice_id, type)` | Normalize a hand-built data frame. `type = "overhead"` or `"income"`. Income path calls `validate_income()` automatically. |

**Stubs (not yet implemented):**
- `ingest_gnucash_xml()` — GnuCash XML format
- `ingest_quickbooks()` — QuickBooks export
- `ingest_csv_generic()` — generic CSV with caller-specified column names

### Account mapping

| Function | Description |
|---|---|
| `default_account_map()` | Returns the built-in pattern→category mapping tibble. Categories: `rent`, `staff`, `supplies`, `software`, `insurance`, `marketing`, `other`. |
| `map_accounts(raw_tbl, account_map)` | Applies a mapping tibble to an account column. First matching rule wins. Unmapped accounts get `"other"` and a warning. Supports `match_type` values: `"contains"`, `"exact"`, `"regex"`. |

To add practice-specific rules, prepend rows to `default_account_map()` (earlier rows win):

```r
custom_map <- dplyr::bind_rows(
  tibble::tibble(pattern = "lab", category = "supplies",
                 match_type = "contains", ignore_case = TRUE),
  default_account_map()
)
ingest_gnucash_csv("export.csv", practice_id = 1, account_map = custom_map)
```

### Filter & normalize

| Function | Description |
|---|---|
| `filter_gnucash_overhead(data)` | Keep rows where `full_account_name` contains `"Expenses"`. |
| `normalize_gnucash_income(data)` | Keep rows where `full_account_name` contains `"Income"`; renames `amount` → `revenue`. |

### Validate

| Function | Description |
|---|---|
| `validate_overhead(data)` | Checks for missing columns, NA dates, NA amounts, future dates, zero amounts. Adds `is_refund` column. Returns tibble invisibly. |
| `validate_income(data)` | Same checks for income tibbles (`revenue` column instead of `amount`). Called automatically by `ingest_manual(type = "income")`. |

Both functions use structured error/warning classes (see [Error classes](#error-classes) below).

### Summarize

| Function | Description |
|---|---|
| `summarize_overhead_monthly(overhead_tbl, include_refunds)` | Aggregate to practice/year/month. `include_refunds = FALSE` excludes refund rows before summing. |
| `summarize_income_monthly(income_tbl, include_refunds)` | Aggregate to practice/year/month. |
| `summarize_income_weekly(income_tbl, include_refunds)` | Aggregate to practice/week_start. |

Both income summarize functions derive `is_refund` from the sign of `revenue`
if the column is absent (e.g. data from a custom pipeline that skipped
`validate_income()`).

### Forecast

All three functions auto-detect whether the income data is weekly or monthly
and set a default horizon accordingly (52 weeks / 12 months).

| Function | Key parameters | Returns |
|---|---|---|
| `forecast_revenue(income_summary, method, horizon, confidence_level)` | — | `current_revenue`, `forecast_data`, `method`, `frequency` |
| `forecast_breakeven(income_summary, overhead_summary, method, horizon, confidence_level)` | — | `breakeven_date`, `periods_to_breakeven`, `current_surplus_deficit`, `confidence_interval`, `forecast_data`, `method`, `frequency` |
| `forecast_target(income_summary, overhead_summary, target_income, method, horizon, confidence_level)` | `target_income`: desired net income per period | `target_date`, `periods_to_target`, `current_gap`, `required_revenue_now`, `confidence_interval`, `forecast_data`, `target_income`, `method`, `frequency` |

All `forecast_data` tibbles share these columns (plus function-specific ones):

| Column | Description |
|---|---|
| `period_start` | Date of the first day of each forecast period |
| `revenue_forecast` | Point estimate of revenue |
| `revenue_lower` / `revenue_upper` | Confidence interval bounds |
| `overhead_forecast` | Point estimate of overhead (breakeven & target only) |
| `net_forecast` | `revenue_forecast - overhead_forecast` |

#### Choosing a forecasting method

| Method | When to use | Requires |
|---|---|---|
| `"linear"` | < 20 data points; new practice with sparse history | Nothing (base R `lm`) |
| `"ets"` | ≥ 20 points; handles level/trend/seasonality automatically | `forecast` package |
| `"arima"` | ≥ 20 points; good for autocorrelated series | `forecast` package |

The `forecast` package is listed under `Suggests` — install it separately if
you need ETS or ARIMA:

```r
install.packages("forecast")
```

#### How `forecast_target` works

Required revenue in period *t* = `overhead_forecast[t] + target_income`.
This is a moving threshold — if overhead is growing, the bar rises over time.
The function finds the first period where `revenue_forecast[t] >=
required_revenue[t]`.

Confidence interval interpretation:
- **Lower bound** (pessimistic) — earliest date where the revenue *lower* CI
  exceeds the overhead *upper* CI + target
- **Upper bound** (optimistic) — earliest date where the revenue *upper* CI
  exceeds the overhead *lower* CI + target

---

## Error classes

All structured conditions use `rlang::abort()` / `rlang::warn()` and carry
the class as the first element so callers can catch them specifically with
`tryCatch(..., error = function(e) inherits(e, "dcForecastR_xyz"))`.

| Class | Level | Meaning |
|---|---|---|
| `dcForecastR_missing_columns` | error | Required columns absent from input tibble |
| `dcForecastR_invalid_dates` | error | NA dates after parsing |
| `dcForecastR_missing_amounts` | error | NA in `amount` or `revenue` |
| `dcForecastR_refunds_detected` | warning | Negative amounts found; attached as `$refunds` |
| `dcForecastR_zero_amounts` | warning | Zero-value rows found |
| `dcForecastR_future_dates` | warning | Dates after `Sys.Date()`; attached as `$future_rows` |
| `dcForecastR_unmapped_accounts` | warning | Account names with no matching rule; attached as `$unmatched` |
| `dcForecastR_target_not_reached` | warning | `forecast_target()` horizon exhausted before target |
| `dcForecastR_not_implemented` | error | Called a stub function |

---

## Complete pipeline example

```r
library(directCareForecastR)

# 1. Ingest -------------------------------------------------------------------
transactions <- ingest_gnucash_csv("data/gnucash_2025.csv", practice_id = 1)

# 2. Split into income and overhead streams -----------------------------------
income   <- normalize_gnucash_income(transactions)
overhead <- filter_gnucash_overhead(transactions)

# 3. Summarize by month -------------------------------------------------------
income_monthly   <- summarize_income_monthly(income)
overhead_monthly <- summarize_overhead_monthly(overhead)

# 4. Forecast -----------------------------------------------------------------

# When will revenue cover overhead?
breakeven <- forecast_breakeven(income_monthly, overhead_monthly, method = "linear")
breakeven$breakeven_date
breakeven$periods_to_breakeven
breakeven$forecast_data

# When will the practice clear $5,000/month net?
target <- forecast_target(
  income_monthly, overhead_monthly,
  target_income = 5000,
  method = "linear"
)
target$target_date
target$required_revenue_now
target$current_gap

# Revenue-only projection
rev <- forecast_revenue(income_monthly, method = "linear", horizon = 24)
rev$current_revenue
rev$forecast_data
```

### Manual data entry

```r
overhead_df <- data.frame(
  date              = as.Date(c("2025-01-15", "2025-02-01")),
  full_account_name = c("Expenses:Rent", "Expenses:Utilities"),
  account_name      = c("Rent", "Utilities"),
  description       = c("Office rent", "Electric"),
  amount            = c(1200, 150)
)

overhead <- ingest_manual(overhead_df, practice_id = 1, type = "overhead")

income_df <- data.frame(
  date              = as.Date(c("2025-01-01", "2025-02-01")),
  full_account_name = c("Income:Membership", "Income:Membership"),
  account_name      = c("Membership", "Membership"),
  description       = c("Jan fees", "Feb fees"),
  revenue           = c(3200, 3600)
)

income <- ingest_manual(income_df, practice_id = 1, type = "income")
```

---

## Development

```r
# Run tests
devtools::test()

# Full check (should return 0 errors, 0 warnings, 0 notes)
devtools::check()

# Regenerate NAMESPACE and man/ after editing roxygen
devtools::document()
```

### Adding a new dplyr column name

If a new bare column name in a `dplyr` verb triggers a global-variable NOTE
from `R CMD check`, add the name to `R/utils-globals.R`.

### Test count baseline

| File | Tests |
|---|---|
| test-forecast_breakeven.R | 6 |
| test-forecast_helpers.R | 7 |
| test-forecast_revenue.R | 15 |
| test-forecast_target.R | 22 |
| test-income_summarize.R | 22 |
| test-income_validate.R | 24 |
| test-ingest_gnucash.R | 3 |
| test-normalize_gnucash.R | 3 |
| test-normalize_manual.R | 7 |
| test-overhead_filter.R | 2 |
| test-overhead_summarize.R | 3 |
| **Total** | **234** |

---

## Known gaps / future work

- **`ingest_gnucash_xml()`** — GnuCash XML format not yet implemented
- **`ingest_quickbooks()`** — QuickBooks CSV/export not yet implemented  
- **`ingest_csv_generic()`** — Generic CSV with caller-specified column mapping not yet implemented
- **`validate_overhead()` not called for manual overhead** — `ingest_manual(type = "overhead")` normalizes but does not call `validate_overhead()`. Callers can invoke it manually if needed.
- **`directCareAnalytics` not yet built** — the Shiny app scaffold exists but no UI modules or server logic have been wired to this package yet
- **Income category breakdown** — membership vs. visit revenue are not distinguished in summarization; would require an income-specific account map
