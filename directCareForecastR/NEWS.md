# directCareForecastR 0.0.2

## Data ingestion

- `ingest_csv_generic()` added. Reads a caller-specified CSV file, maps
  user-supplied column names to the internal schema, and validates the result.
  Supports three modes via the `type` argument: `"both"` (default) splits a
  single mixed file into overhead and income using pattern matching on a
  `col_type` column and returns `list(overhead = ..., income = ...)`; `"overhead"`
  and `"income"` return a validated tibble directly.
- `ingest_csv_generic()` emits `dcForecastR_unclassified_rows` when rows in a
  mixed file match neither `overhead_pattern` nor `income_pattern`. The warning
  carries `n_unmatched` and `unmatched_values` fields.
- `ingest_csv_generic()` accepts an optional `date_format` argument; when
  omitted, the four most common date formats are tried in order.
- `ingest_gnucash_csv()` and `ingest_gnucash_xml()` are unchanged.

## Overhead summarisation

- `summarize_overhead_monthly()` now validates required columns on entry and
  emits `dcForecastR_missing_columns` (matching the behaviour of
  `summarize_income_monthly()`).
- `summarize_overhead_weekly()` added, mirroring `summarize_income_weekly()`.

## Forecasting

- `.forecast_series()` now guards against insufficient data before dispatching
  to ETS or ARIMA:
  - Fewer than 2 observations: emits `dcForecastR_insufficient_data` and
    returns a flat forecast.
  - Below method-specific minimums (ETS: 6, ARIMA: 8): emits
    `dcForecastR_method_fallback` and falls back to linear regression.
  - Below the recommended threshold (20) for ETS/ARIMA, or fewer than 3
    observations for linear: emits `dcForecastR_low_data_advisory`.
- `.forecast_series_tracked()` added (internal). Wraps `.forecast_series()`
  with `withCallingHandlers` to capture data-volume warning messages without
  muffling them, so they propagate to both the caller and the UI.
- `forecast_breakeven()`, `forecast_revenue()`, and `forecast_target()` now
  collect warnings from `.forecast_series_tracked()` and return them in a
  `data_warnings` field (`NULL` when no warnings were emitted).
- ARIMA now strips high-frequency seasonality (frequency > 24) before fitting,
  preventing `auto.arima` from silently producing a flat ARIMA(0,0,0) on weekly
  data.

## Validation

- `validate_overhead()` added. Mirrors `validate_income()`: checks required
  columns, date parseability, amount integrity, refund detection, and future
  dates. Emits the same condition classes as the income equivalent
  (`dcForecastR_missing_columns`, `dcForecastR_invalid_dates`,
  `dcForecastR_missing_amounts`, `dcForecastR_refunds_detected`,
  `dcForecastR_zero_amounts`, `dcForecastR_future_dates`).

## Account mapping

- `default_account_map()` and `map_accounts()` are now tested. No behaviour
  change.

## Sample data

- `inst/extdata/sample_overhead.csv` — 10-row overhead-only sample.
- `inst/extdata/sample_income.csv` — 10-row income-only sample.
- `inst/extdata/sample_transactions.csv` — 18-row mixed file suitable for
  testing `ingest_csv_generic()` with `type = "both"`.

---

# directCareForecastR 0.0.1

- `ingest_gnucash_csv()` reads a GnuCash transaction CSV export, normalises
  account names, and splits rows into overhead and income tibbles.
- `ingest_gnucash_xml()` reads a GnuCash XML ledger file.
- `ingest_manual()` accepts a data frame of period summaries (overhead or
  income) entered without a bookkeeping export.
- `filter_gnucash_overhead()` and `normalize_gnucash_income()` filter raw
  GnuCash tibbles to the expense or income subset respectively.
- `validate_income()` validates an income tibble against the internal schema.
- `summarize_income_monthly()` and `summarize_income_weekly()` aggregate
  validated income tibbles to period totals with refund handling.
- `summarize_overhead_monthly()` aggregates validated overhead tibbles.
- `default_account_map()` returns the built-in GnuCash account mapping.
- `map_accounts()` applies an account map to a raw GnuCash tibble, assigning
  each transaction to an overhead category.
- `forecast_revenue()` projects revenue forward using linear regression, ETS,
  or ARIMA.
- `forecast_breakeven()` projects the date at which revenue is forecast to
  cover overhead.
- `forecast_target()` projects the date at which revenue is forecast to reach a
  user-specified income target.
- All forecast functions accept `method`, `horizon`, `confidence`, and
  `frequency` arguments and return a standardised list including `forecast_data`,
  `method`, `frequency`, and scalar summary values.
