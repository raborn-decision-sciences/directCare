# directCarePlanR 0.0.1

The scaffolded stubs from 0.0.0.9000 are now fully implemented and backing
the `directCarePlanner` (Launch Planner) Shiny app in production.

* `market_context.R` -- ZIP-based market context lookup (population,
  income, uninsured rate, physician density, nearby direct care
  practices).
* `revenue.R` -- membership and fee-for-service revenue modeling. Both
  ramp up to their target (panel size / visit volume) over a configurable
  number of months, using either a linear or smoothstep S-curve shape,
  rather than starting at full volume on day one.
* `projections.R` -- multi-scenario (conservative / base / optimistic)
  financial projections combining revenue against overhead, with
  configurable overhead growth.
* `capital.R` -- startup cost and personal runway calculations.
* `interpret.R` -- rule-based narrative text generation for revenue,
  projections, and capital, including which assumption is the most
  sensitive lever in a given plan.
* `report.R` -- assembles a downloadable, Typst-rendered PDF launch plan
  combining all of the above.
