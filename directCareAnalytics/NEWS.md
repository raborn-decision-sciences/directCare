# directCareAnalytics 0.0.1

Initial release of the Direct Care Analytics Shiny application.

## Data entry

- **Upload workflow** (`mod_upload`): landing screen lets the user choose
  between uploading bookkeeping data or opening the Quick Estimator. Supports
  GnuCash CSV exports and generic CSVs via `directCareForecastR::ingest_csv_generic()`.
- **Manual entry workflow** (`mod_manual_entry`): accepts period-level
  overhead and revenue summaries when no bookkeeping export is available.
  Supports both monthly and weekly granularity.
- **Quick Estimator** (`mod_edit`, scenario mode): lets the user model a
  practice from scratch by entering panel size, monthly membership fee,
  growth rate, and overhead line items. Generates synthetic monthly data
  without requiring a CSV upload.

## Review & edit

- **`mod_edit`** displays uploaded transactions in an editable DT table.
  Supports row deletion, category re-mapping, and revenue sub-type correction
  (membership vs. fee-for-service). Shows a summary card (total overhead,
  earliest and most recent revenue) once data is loaded.
- When overhead data is present but no transaction rows exist (manual entry
  path), the tab shows an informational card instead of an empty table.

## Summary

- **`mod_summary`** shows overhead and revenue breakdowns for the loaded data
  period. Value boxes display total and average per period. Bar charts and
  data tables update dynamically based on a monthly/weekly frequency toggle.
  Overhead is further broken down by account category.

## Projections

- **`mod_projections`** runs `forecast_breakeven()`, `forecast_revenue()`, and
  `forecast_target()` from `directCareForecastR`. Each forecast panel
  displays value boxes for key scalars, a `ggplot2` time-series chart with a
  shaded confidence interval, and a narrative interpretation paragraph.
- Method selector (linear / ETS / ARIMA), horizon slider, confidence level,
  and optional panel size and membership fee inputs are shared across all
  three forecast types.
- Toast notifications surface data-volume warnings from the backend
  (`dcForecastR_insufficient_data`, `dcForecastR_method_fallback`,
  `dcForecastR_low_data_advisory`) without interrupting the forecast.
- Data-volume warnings also appear as a "Data quality note" section in each
  interpretation paragraph.

## Reports

- **`mod_projections`** provides a "Download Report" button that renders a
  Typst-based PDF. The report includes practice metadata, overhead and income
  summary tables, forecast charts, and the full interpretation text for each
  active forecast type.

## Formatting

- All displayed dollar amounts are formatted to exactly two decimal places via
  `fmt_dollar()` / `fmt_dollar_format()` helpers defined in `utils_globals.R`.
  This is enforced across value boxes, plot axes, narrative text, data tables,
  and PDF report fields.

## Infrastructure

- Golem-based package structure with `page_navbar` / bslib Bootstrap 5 UI.
- `thematic::thematic_shiny()` applied so base R and `ggplot2` plots inherit
  the app theme.
- Custom CSS loaded from `inst/app/www/custom.css`.
- `utils_interpret.R` contains rule-based narrative generators for all three
  forecast types; no external API required.
- `utils_report.R` assembles the Typst report data structure and handles HTML
  to plain-text conversion for the PDF template.
