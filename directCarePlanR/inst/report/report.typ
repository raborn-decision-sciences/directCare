// Direct Care Practice Launch Plan — placeholder report template.
// Minimal, unbranded layout: correct and complete, not polished. A
// dedicated design pass adapts directCareAnalytics's RDS-branded layout
// once real report content/section order has been validated.
//
// data.json is written to the same temp directory by R before
// typst compile is called.

#let d = json("data.json")

#set page(paper: "us-letter", margin: 1in)
#set text(size: 10pt)
#set par(spacing: 0.9em)

#align(center)[
  #text(size: 18pt, weight: "bold")[#d.practice_name]

  #text(size: 10pt, fill: gray)[Practice Launch Plan --- Generated #d.report_date]
]

#v(1em)

#let dollar(x) = "$" + str(calc.round(x))

#if d.market != none [
  == Market Context
  - County: #d.market.county_name, #d.market.state_abb
  - Population: #d.market.population
  - Median household income: #dollar(d.market.median_household_income)
  - Uninsured rate: #d.market.uninsured_rate_pct%
  - Physician density: #d.market.physician_density_per_10k per 10,000 population
  - Known direct care practices nearby: #d.market.landscape_count
]

#if d.revenue != none [
  == Revenue

  Total revenue at full ramp: *#dollar(d.revenue.total_final)/month*

  #table(
    columns: 4,
    [*Month*], [*Membership*], [*Fee-for-Service*], [*Total*],
    ..d.revenue.table.map(row => (
      str(row.month),
      dollar(row.membership_revenue),
      dollar(row.fee_revenue),
      dollar(row.total_revenue),
    )).flatten()
  )
]

#if d.projections != none [
  == Scenario Projections

  #for scenario in d.projections.scenarios [
    === #scenario.scenario

    #table(
      columns: 5,
      [*Month*], [*Revenue*], [*Overhead*], [*Net Income*], [*Cumulative*],
      ..scenario.table.map(row => (
        str(row.month),
        dollar(row.total_revenue),
        dollar(row.overhead),
        dollar(row.net_income),
        dollar(row.cumulative_net_income),
      )).flatten()
    )
  ]
]

#if d.capital != none [
  == Capital Requirements
  - Startup costs: #dollar(d.capital.startup_total)
  - Personal runway: #dollar(d.capital.runway_total)
  - *Combined total: #dollar(d.capital.combined_total)*
]

#if d.interpretations != none [
  == Interpretation

  #for (section, paragraphs) in d.interpretations [
    === #section
    #for p in paragraphs [
      #p

    ]
  ]
]
