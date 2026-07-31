// Direct Care Practice Launch Plan — RDS-branded report template.
// Design system (colors, section-head/sub-head/kpi/table helpers,
// header/footer) ported from directCareAnalytics's report.typ, since
// both products share the same Raborn Decision Sciences brand.
//
// data.json is written to the same temp directory by R before
// typst compile is called.

#let d = json("data.json")

// ── Page & typography ──────────────────────────────────────────────────
#set page(
  paper: "us-letter",
  margin: (x: 0.85in, y: 0.9in),
  header: context {
    if counter(page).get().first() > 1 [
      #set text(size: 8pt, fill: rgb("#6B7280"))
      #d.practice_name — Direct Care Practice Launch Planner
      #h(1fr)
      Generated: #d.report_date
      #v(-4pt)
      #line(length: 100%, stroke: 0.5pt + rgb("#E2E8F0"))
    ]
  },
  footer: [
    #line(length: 100%, stroke: 0.5pt + rgb("#E2E8F0"))
    #v(-4pt)
    #set text(size: 8pt, fill: rgb("#6B7280"))
    #text(style: "italic")[Direct Care Practice Launch Planner — Confidential]
    #h(1fr)
    #context counter(page).display("1 of 1", both: true)
  ]
)
#set text(
  font: ("Helvetica Neue", "Helvetica", "Arial"),
  size: 10pt,
  fill: rgb("#172033")
)
#set par(leading: 0.65em, spacing: 0.9em, first-line-indent: 0pt)

// ── Brand colours ─────────────────────────────────────────────────────
#let navy      = rgb("#172033")
#let teal      = rgb("#14B8A6")
#let teal-dark = rgb("#0d9488")
#let light-bg  = rgb("#F8FAFC")
#let border    = rgb("#E2E8F0")
#let muted     = rgb("#6B7280")
#let green     = rgb("#16A34A")
#let red       = rgb("#DC2626")

// ── Layout helpers ──────────────────────────────────────────────────────

// Navy section bar
#let section-head(title) = {
  v(0.5em)
  rect(
    fill: navy, width: 100%,
    inset: (x: 8pt, y: 5pt), radius: 2pt
  )[#text(fill: white, weight: "bold", size: 10.5pt)[#title]]
  v(0.3em)
}

// Teal subsection label
#let sub-head(title) = {
  v(0.35em)
  text(weight: "semibold", fill: teal-dark, size: 10pt)[#title]
  v(0.1em)
  line(length: 100%, stroke: 0.5pt + border)
  v(0.2em)
}

// Compact KPI box
#let kpi(label, value, color: navy) = rect(
  fill: light-bg, stroke: 0.75pt + border,
  inset: (x: 7pt, y: 5pt), radius: 3pt, width: 100%
)[
  #set align(center)
  #text(size: 7.5pt, fill: muted)[#label]
  #linebreak()
  #text(weight: "bold", size: 10.5pt, fill: color)[#value]
]

// Alternating table row fill
#let stripe(i) = if calc.odd(i) { light-bg } else { white }

// Value-dependent colour (green = surplus/gain, red = deficit/loss)
#let sign-color(x) = if x >= 0 { green } else { red }

// Standard table stroke rule
#let tbl-stroke(x, cols) = (
  bottom: 0.5pt + border,
  right: if x < cols - 1 { 0.5pt + border } else { none }
)

// Renders a KPI row from a variable-length array of kpi() calls.
#let kpi-row(boxes) = grid(
  columns: (1fr,) * boxes.len(),
  column-gutter: 6pt,
  ..boxes
)

#let dollar(x) = "$" + str(calc.round(x))

// Turns a snake_case programmatic key (interpretation section names,
// scenario names, startup cost line-item names, ...) into a readable
// label: "ehr_setup" -> "EHR Setup", "fee_for_service" -> "Fee For
// Service". Extend `acronyms` as new all-caps abbreviations show up in
// exposed keys.
#let acronyms = ("ehr": "EHR")
#let humanize(key) = {
  key
    .split("_")
    .map(word => if word in acronyms {
      acronyms.at(word)
    } else {
      upper(word.slice(0, 1)) + word.slice(1)
    })
    .join(" ")
}

// First scenario in `scenarios` whose name is "base", or none.
#let find-base-scenario(scenarios) = {
  let result = none
  for s in scenarios {
    if s.scenario == "base" { result = s }
  }
  result
}

// First month in a scenario's table where cumulative_net_income turns
// non-negative, or none if it never does within the horizon.
#let recovery-month(table) = {
  let result = none
  for row in table {
    if result == none and row.cumulative_net_income >= 0 {
      result = row.month
    }
  }
  result
}

// ── Title block ──────────────────────────────────────────────────────
#rect(
  fill: navy, width: 100%,
  inset: (x: 14pt, y: 14pt), radius: 4pt
)[
  #set align(center)
  #text(fill: white, weight: "bold", size: 18pt)[#d.practice_name]
  #v(3pt)
  #text(fill: teal, size: 11pt)[Practice Launch Plan]
  #v(1pt)
  #text(fill: rgb("#94A3B8"), size: 9pt)[Direct Care Practice Launch Planner — #d.report_date]
]
#v(0.5em)

// ── Highlights strip ─────────────────────────────────────────────────
// Only shown when every section is present -- these four numbers are
// only meaningfully "the headline" together; a partial report relies on
// each section's own content instead.
#if d.market != none and d.revenue != none and d.projections != none and d.capital != none [
  #let base-scenario = find-base-scenario(d.projections.scenarios)
  #let recovery = recovery-month(base-scenario.table)
  #let recovery-label = if recovery == none { "Not in horizon" } else { "Month " + str(recovery) }

  #kpi-row((
    kpi("Location", d.market.county_name + ", " + d.market.state_abb),
    kpi("Revenue at Full Ramp", dollar(d.revenue.total_final)),
    kpi("Cash-Flow Recovery (Base)", recovery-label, color: teal-dark),
    kpi("Capital Required", dollar(d.capital.combined_total)),
  ))
]

// ── Market Context ───────────────────────────────────────────────────
#if d.market != none [
  #section-head("Market Context")

  #block(breakable: false)[
    #sub-head(d.market.county_name + ", " + d.market.state_abb)
    #kpi-row((
      kpi("Population", str(d.market.population)),
      kpi("Median HH Income", dollar(d.market.median_household_income)),
      kpi("Uninsured Rate", str(d.market.uninsured_rate_pct) + "%"),
      kpi("Physician Density", str(d.market.physician_density_per_10k) + " / 10k"),
    ))
    #v(0.3em)
    #text(size: 9pt, fill: muted)[
      #d.market.landscape_count known direct care practices nearby.
    ]
  ]
]

// ── Revenue ───────────────────────────────────────────────────────────
#if d.revenue != none [
  #let rev = d.revenue

  #section-head("Revenue")

  #block(breakable: false)[
    #let revenue-boxes = {
      let boxes = (kpi("Total at Full Ramp", dollar(rev.total_final)),)
      if rev.membership_final != none {
        boxes = boxes + (kpi("Membership Revenue", dollar(rev.membership_final)),)
      }
      if rev.fee_final != none {
        boxes = boxes + (kpi("Fee-for-Service Revenue", dollar(rev.fee_final)),)
      }
      boxes
    }
    #kpi-row(revenue-boxes)
  ]

  #v(0.4em)
  #block(breakable: false)[
    #sub-head("Monthly Revenue")
    #table(
      columns: (1fr, 1fr, 1fr, 1fr),
      fill: (_, y) => stripe(y),
      stroke: (x, y) => tbl-stroke(x, 4),
      inset: (x: 6pt, y: 4pt),
      table.header(
        text(weight: "bold")[Month],
        text(weight: "bold")[Membership],
        text(weight: "bold")[Fee-for-Service],
        text(weight: "bold")[Total]
      ),
      ..rev.table.map(row => (
        str(row.month),
        dollar(row.membership_revenue),
        dollar(row.fee_revenue),
        text(weight: "semibold")[#dollar(row.total_revenue)],
      )).flatten()
    )
  ]
]

// ── Scenario Projections ────────────────────────────────────────────
#if d.projections != none [
  #section-head("Scenario Projections")

  #for scenario in d.projections.scenarios [
    #block(breakable: false)[
      #sub-head(humanize(scenario.scenario))
      #table(
        columns: (1fr, 1fr, 1fr, 1fr, 1fr),
        fill: (_, y) => stripe(y),
        stroke: (x, y) => tbl-stroke(x, 5),
        inset: (x: 6pt, y: 4pt),
        table.header(
          text(weight: "bold")[Month],
          text(weight: "bold")[Revenue],
          text(weight: "bold")[Overhead],
          text(weight: "bold")[Net Income],
          text(weight: "bold")[Cumulative]
        ),
        ..scenario.table.map(row => (
          str(row.month),
          dollar(row.total_revenue),
          dollar(row.overhead),
          text(fill: sign-color(row.net_income), weight: "semibold")[#dollar(row.net_income)],
          text(fill: sign-color(row.cumulative_net_income))[#dollar(row.cumulative_net_income)],
        )).flatten()
      )
    ]
    #v(0.3em)
  ]
]

// ── Capital Requirements ────────────────────────────────────────────
#if d.capital != none [
  #section-head("Capital Requirements")

  #block(breakable: false)[
    #sub-head("Startup Costs")
    #table(
      columns: (1fr, 1fr),
      fill: (_, y) => stripe(y),
      stroke: (x, y) => tbl-stroke(x, 2),
      inset: (x: 6pt, y: 4pt),
      table.header(
        text(weight: "bold")[Item],
        text(weight: "bold")[Amount]
      ),
      ..d.capital.startup_line_items.pairs().map(item => (
        humanize(item.at(0)),
        dollar(item.at(1)),
      )).flatten()
    )
  ]

  #v(0.4em)
  #block(breakable: false)[
    #kpi-row((
      kpi("Total Startup Costs", dollar(d.capital.startup_total)),
      kpi("Personal Runway", dollar(d.capital.runway_total)),
      kpi("Combined Total", dollar(d.capital.combined_total), color: teal-dark),
    ))
  ]
]

// ── Interpretation ───────────────────────────────────────────────────
#if d.interpretations != none [
  #section-head("Interpretation")

  #for (section, paragraphs) in d.interpretations [
    #if paragraphs.len() > 0 [
      #block(breakable: false)[
        #sub-head(humanize(section))
        #text(size: 9.5pt)[#paragraphs.at(0)]
      ]
      #for para in paragraphs.slice(1) [
        #v(0.5em)
        #text(size: 9.5pt)[#para]
      ]
    ]
  ]
]

// ── Footnotes ─────────────────────────────────────────────────────────
// Scenario definitions sourced directly from
// directCarePlanR::project_scenarios()'s built-in defaults
// (conservative: ramp_months_multiplier = 1.5, overhead_multiplier = 1.1;
// optimistic: ramp_months_multiplier = 0.75, overhead_multiplier = 0.9) --
// the app never overrides these with custom scenario_params, so these are
// always the actual numbers in effect, not just illustrative ones.
#if d.projections != none [
  #v(0.8em)
  #line(length: 100%, stroke: 0.5pt + border)
  #v(0.2em)
  #text(size: 8pt, fill: muted)[
    *Scenario definitions:* Base reflects the assumptions you entered.
    Conservative assumes membership growth takes 50% longer to reach your
    target panel size, with monthly overhead 10% higher than entered.
    Optimistic assumes membership growth reaches your target panel size
    25% faster, with monthly overhead 10% lower than entered.
  ]
]

#v(0.5em)
#line(length: 100%, stroke: 0.5pt + border)
#v(0.2em)
#text(size: 8pt, fill: muted)[
  *Disclaimer:* These projections are generated from the assumptions you
  provided. They are estimates, not guarantees. Actual results depend on
  market conditions, execution, enrollment, and other practice-specific
  factors. This report is for internal planning purposes only and does
  not constitute financial, legal, or medical advice.
]
