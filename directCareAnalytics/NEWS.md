# directCareAnalytics 0.0.2

Substantial functionality added on top of the 0.0.1 base, plus a round of
bug fixes and performance work. Grouped by area, not exhaustive.

## Authentication & accounts

- Practice login, signup, and self-service password reset (email delivery
  via ZeptoMail), all backed by the shared `directCareAuth` package.
- Login lockout and password-reset/signup rate limiting, both keyed on
  email and on the client's IP address, to resist credential-spray and
  spam attacks that vary the email but not the source.
- Account Settings modal for editing practice profile and changing
  password without contacting support.

## Guided tours & demo mode

- Three interactive `cicerone`-based walkthroughs (Historical Data upload,
  Plan My Practice / synthetic history, Quick Calculator).
- A `?demo=1` demo mode with seeded sample data and the real upload/manual-
  entry paths gated off, for prospective-customer evaluation without a
  real account.

## Saved scenarios

- Named, user-labeled scenario slots (3 per practice per feature area) for
  Projections' full forecast pipeline and the Quick Calculator, backed by
  the shared `directCareScenarios` package. Save/Load and a "viewing saved
  scenario" banner are available from every tab, not just where they were
  first built.

## Performance & UX

- Run Forecast now executes in a background process
  (`shiny::ExtendedTask` + `mirai`) instead of blocking the entire app
  (every session, every practice) for the duration of one click.
- Dark mode toggle no longer flashes unstyled content on switch.
- Enter Data Manually restructured into Set Up / Overhead / Income tabs
  instead of one long scroll; adopted the same always-visible sticky
  footer the other tabs use.
- Assorted correctness fixes: Membership Profile inputs debounced instead
  of recomputing on every keystroke; generic and GnuCash CSV upload
  validation and format-reference tooltips corrected against the actual
  parser requirements.

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
