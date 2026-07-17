# directCarePlanR — Internal Developer Reference

**Internal package. Not published to CRAN.**
Business logic for the Direct Care Practice Launch Planner. Unlike
`directCareForecastR` (which forecasts from a practice's actual financial
history), this package projects forward from a physician's assumptions
about a practice that does not yet exist — no actuals, no time-series
fitting. Every function takes assumptions in and returns market context,
revenue, financial projections, capital requirements, narrative
interpretation, or a rendered PDF report out.

Reference market data (Census, SAHIE, NPPES, geography crosswalks, and a
curated direct care practice directory) lives in a separate package,
`directCareData`, so it can be refreshed and versioned on its own
schedule, independent of this package's business logic.

---

## What it does

Answers the questions a physician planning a practice launch needs before
committing capital:

1. **Market context** — what does the local market look like at this ZIP
   or county (population, income, uninsured rate, physician density,
   nearby direct care competition)?
2. **Revenue** — what does membership + fee-for-service revenue look like
   as the panel ramps up?
3. **Projections** — month-by-month net income under base, conservative,
   and optimistic assumptions?
4. **Capital** — how much startup capital and personal runway does this
   require?
5. **Interpretation** — decisional narrative: what does this plan
   *require* to work, not just what the numbers are.
6. **Report** — assemble everything into a PDF via a Typst template.

---

## Pipeline

```
ZIP or county + assumptions (panel size, fees, overhead, startup costs, ...)
          │
          ▼
   build_market_context()   ← resolve_geography() + get_population_income()
                               + get_uninsured_estimate() + get_physician_density()
                               + get_direct_care_landscape()
          │
          ▼
   calc_mixed_revenue()      ← calc_membership_revenue() + calc_fee_revenue()
          │
          ▼
   project_practice()  /  project_scenarios()
          │
     ┌────┴─────────────────┬───────────────────────┐
     ▼                       ▼                       ▼
calc_startup_costs()   calc_personal_runway()   calc_loan_amortization()
          │
          ▼
interpret_revenue() / interpret_projection() / interpret_capital()
          │
          ▼
   build_report_data()  →  render_plan_report()   [PDF via Typst]
```

---

## Data schemas

### Market context (`dcPlanR_market_context`)

Output of `build_market_context()`.

| Element | Type | Notes |
|---|---|---|
| `geography` | `dcPlanR_geography` list | `county_fips`, `state_fips`, `metro_fips`, `county_name`, `state_abb` |
| `population_income` | `dcPlanR_population_income` list | `county_fips`, `population`, `median_household_income` |
| `uninsured` | `dcPlanR_uninsured_estimate` list | `county_fips`, `uninsured_count`, `uninsured_rate` |
| `physician_density` | `dcPlanR_physician_density` list | `county_fips`, `physician_count`, `physician_density_per_10k` |
| `landscape` | tibble | Nearby direct care practices; zero rows if none known (`directCareData::direct_care_landscape` is currently an empty placeholder — no real source identified yet) |
| `provenance` | list | `directCareData::data_provenance` — vintage/retrieval date/source per underlying data source |

### Revenue (`dcPlanR_revenue`)

Output of `calc_mixed_revenue()`.

| Element | Type | Notes |
|---|---|---|
| `membership` | `dcPlanR_membership_revenue` tibble, or `NULL` | `month`, `panel_size`, `revenue` |
| `fee_for_service` | `dcPlanR_fee_revenue` tibble, or `NULL` | `month`, `visit_volume`, `revenue` |
| `total` | tibble | `month`, `membership_revenue`, `fee_revenue`, `total_revenue` (0-filled, not `NA`, for an omitted component) |

### Projection (`dcPlanR_projection` / `dcPlanR_scenario_projection`)

Output of `project_practice()` / `project_scenarios()`. Columns: `month`,
`membership_revenue`, `fee_revenue`, `total_revenue`, `overhead`,
`net_income`, `cumulative_net_income` — plus `scenario`
(`"conservative"` / `"base"` / `"optimistic"`) for `project_scenarios()`.

### Capital

| Function | Returns | Notes |
|---|---|---|
| `calc_startup_costs()` | `dcPlanR_startup_costs` list | `total`, `line_items` (input, preserved) |
| `calc_personal_runway()` | `dcPlanR_personal_runway` list | `total`, `monthly_expenses`, `months_coverage` |
| `calc_loan_amortization()` | `dcPlanR_loan_amortization` tibble | `month`, `payment`, `principal_paid`, `interest_paid`, `remaining_balance` |

---

## Function reference

### Market context (`market_context.R`)

| Function | Description |
|---|---|
| `resolve_geography(location, state)` | ZIP or county name → FIPS/metro identifiers. ZIPs spanning multiple counties resolve to the highest-overlap ("primary") county. |
| `get_population_income(fips)` | Population + median household income for a county. |
| `get_uninsured_estimate(fips)` | Uninsured count + rate for a county. |
| `get_physician_density(fips)` | Physician count + density per 10,000 population for a county. |
| `get_direct_care_landscape(fips)` | Known direct care practices for a county (zero rows, not an error, if none). |
| `build_market_context(location, state)` | Calls all of the above and assembles the full market context. |

### Revenue (`revenue.R`)

| Function | Description |
|---|---|
| `calc_membership_revenue(panel_size, fee, horizon_months, ramp_months, ramp_shape)` | Membership revenue ramping to `panel_size` over `ramp_months` (`"linear"` or `"s_curve"` smoothstep). |
| `calc_fee_revenue(visit_volume, new_visit_fee, follow_up_fee, new_visit_pct, horizon_months)` | Flat fee-for-service revenue from visit volume × new/follow-up fee mix. |
| `calc_mixed_revenue(membership_args, fee_args, horizon_months)` | Combines both by month; at least one of `membership_args`/`fee_args` is required. |

### Projections (`projections.R`)

| Function | Description |
|---|---|
| `project_practice(assumptions, horizon_months)` | Single-scenario month-by-month projection: revenue netted against overhead (flat or compounding via `overhead_growth_rate`). |
| `project_scenarios(assumptions, horizon_months, scenario_params)` | Conservative/base/optimistic wrapper, varying ramp speed and overhead via configurable multipliers (defaults: conservative 1.5×/1.1×, optimistic 0.75×/0.9×). |

### Capital (`capital.R`)

| Function | Description |
|---|---|
| `calc_startup_costs(cost_items)` | Sums a named vector/list of one-time cost line items. |
| `calc_personal_runway(monthly_expenses, months_coverage)` | `monthly_expenses × months_coverage`. |
| `calc_loan_amortization(principal, annual_rate, term_months)` | Standard fixed-rate amortization schedule; handles interest-free loans (`annual_rate = 0`). |

### Interpretation (`interpret.R`)

All three return plain text with `"\n\n"` paragraph breaks — not HTML.
Framing is decisional (what the plan requires), not diagnostic.

| Function | Description |
|---|---|
| `interpret_revenue(revenue)` | Revenue composition at full ramp; names the more sensitive lever (fee vs. visit volume) by comparing the dollar impact of a 10% shift in each. |
| `interpret_projection(projection)` | Per-scenario cash-flow recovery month (first month `cumulative_net_income >= 0`); flags the conservative/optimistic spread as highly sensitive (> 6 months) or relatively robust. |
| `interpret_capital(startup_costs, personal_runway)` | Combines startup costs and personal runway into a single financing decision. |

### Report (`report.R`)

| Function | Description |
|---|---|
| `build_report_data(market_context, revenue, projections, capital, interpretations, practice_name)` | Assembles a flat, JSON-serializable schema; each section is `NULL` when its input is omitted. |
| `render_plan_report(data, out_file)` | Writes `data.json`, compiles the bundled Typst template (`inst/report/report.typ`) via the `typst` CLI or the `typr` package. |

---

## Error classes

All structured conditions use `rlang::abort()` (or `rlang::inform()` for
the one informational case) and carry the class as the first element, so
callers can catch them specifically with
`tryCatch(..., error = function(e) inherits(e, "dcPlanR_xyz"))`.

| Class | Level | Meaning |
|---|---|---|
| `dcPlanR_invalid_argument` | error | Malformed numeric/scalar argument (revenue, capital, and projection functions) |
| `dcPlanR_invalid_fips` | error | Malformed `location`/`fips` argument |
| `dcPlanR_fips_not_found` | error | Well-formed FIPS not present in `directCareData::county_market_data` |
| `dcPlanR_zip_not_found` | error | ZIP not present in `directCareData::zip_county_crosswalk` |
| `dcPlanR_zip_multi_county` | message | Informational: a ZIP spans multiple counties; the highest-overlap county was used |
| `dcPlanR_county_not_found` | error | County name not present in `directCareData::county_cbsa_crosswalk` |
| `dcPlanR_county_ambiguous` | error | County name matches multiple states and no/insufficient `state` disambiguator given |
| `dcPlanR_missing_revenue_component` | error | Neither `membership_args` nor `fee_args` supplied to `calc_mixed_revenue()`/`project_practice()` |
| `dcPlanR_typst_compile_error` | error | The `typst` CLI returned a compile error |
| `dcPlanR_typst_missing` | error | Neither the `typst` CLI nor the `typr` package is available |
| `dcPlanR_typst_no_output` | error | Compilation reported success but produced no PDF |

---

## Complete pipeline example

```r
library(directCarePlanR)

# 1. What does the market look like here?
market <- build_market_context("30309") # Midtown Atlanta, GA

# 2. Project revenue from a membership + fee-for-service mix
revenue <- calc_mixed_revenue(
  membership_args = list(panel_size = 300, fee = 100, ramp_months = 12),
  fee_args = list(visit_volume = 100, new_visit_fee = 200, follow_up_fee = 100),
  horizon_months = 24
)

# 3. Project conservative / base / optimistic scenarios
assumptions <- list(
  membership_args = list(panel_size = 300, fee = 100, ramp_months = 12),
  fee_args = list(visit_volume = 100, new_visit_fee = 200, follow_up_fee = 100),
  overhead_monthly = 12000
)
projections <- project_scenarios(assumptions, horizon_months = 24)

# 4. Size the capital required to get there
startup_costs <- calc_startup_costs(c(
  ehr_setup = 8000, equipment = 5000, licensing = 1500, marketing = 3000
))
personal_runway <- calc_personal_runway(monthly_expenses = 5000, months_coverage = 6)

# 5. Turn the numbers into decisional narrative
interpretations <- list(
  revenue = interpret_revenue(revenue),
  projection = interpret_projection(projections),
  capital = interpret_capital(startup_costs, personal_runway)
)

# 6. Assemble and render the report
report_data <- build_report_data(
  market_context = market,
  revenue = revenue,
  projections = projections,
  capital = list(startup_costs = startup_costs, personal_runway = personal_runway),
  interpretations = interpretations
)
render_plan_report(report_data, "practice_launch_plan.pdf")
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

### Test count baseline

| File | Tests |
|---|---|
| test-capital.R | 33 |
| test-interpret.R | 29 |
| test-market_context.R | 38 |
| test-projections.R | 27 |
| test-report.R | 27 |
| test-revenue.R | 33 |
| **Total** | **187** |

---

## Known gaps / future work

- **`direct_care_landscape` is an empty placeholder** — `directCareData` ships it with the correct schema but zero rows; no real competitor-directory source has been identified yet. No code changes are needed elsewhere when one is.
- **`inst/report/report.typ` is unbranded** — a minimal, functionally-complete placeholder (no RDS navy/teal design system, KPI boxes, or section headers like `directCareAnalytics`'s report). A dedicated design pass is expected once real report content/section order has been validated against actual use.
- **`project_scenarios()`'s `scenario_params` is scoped to two levers** — `ramp_months_multiplier` and `overhead_multiplier` only, matching the spec's own named example. Broadening to arbitrary assumption overrides would be a non-breaking future addition.
- **No Shiny app yet** — `directCarePlanner`, the Golem app meant to consume this package (form-based assumption collection, operational checklist, report download), has not been started.
