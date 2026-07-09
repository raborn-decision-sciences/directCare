// Direct Care Analytics — Practice Financial Analysis Report
// Data and plot images are written to the same temp directory by R before
// typst compile is called.

#let d = json("data.json")

// ── Page & typography ──────────────────────────────────────────────────────────
#set page(
  paper: "us-letter",
  margin: (x: 0.85in, y: 0.9in),
  header: context {
    if counter(page).get().first() > 1 [
      #set text(size: 8pt, fill: rgb("#6B7280"))
      #d.practice_name — Direct Care Analytics
      #h(1fr)
      Generated: #d.report_date
      #v(-4pt)
      #line(length: 100%, stroke: 0.5pt + rgb("#E2E8F0"))
    ]
  },
  footer: context {
    if counter(page).get().first() > 1 [
      #line(length: 100%, stroke: 0.5pt + rgb("#E2E8F0"))
      #v(-4pt)
      #set text(size: 8pt, fill: rgb("#6B7280"))
      #text(style: "italic")[Direct Care Analytics — Confidential]
      #h(1fr)
      #context counter(page).display("1 of 1", both: true)
    ]
  }
)
#set text(
  font: ("Helvetica Neue", "Helvetica", "Arial"),
  size: 10pt,
  fill: rgb("#172033")
)
#set par(leading: 0.65em, spacing: 0.9em, first-line-indent: 0pt)

// ── Brand colours ─────────────────────────────────────────────────────────────
#let navy      = rgb("#172033")
#let teal      = rgb("#14B8A6")
#let teal-dark = rgb("#0d9488")
#let light-bg  = rgb("#F8FAFC")
#let border    = rgb("#E2E8F0")
#let muted     = rgb("#6B7280")
#let green     = rgb("#16A34A")
#let red       = rgb("#DC2626")
#let amber     = rgb("#D97706")

// ── Layout helpers ────────────────────────────────────────────────────────────

#let section-head(title) = {
  v(0.5em)
  rect(
    fill: navy, width: 100%,
    inset: (x: 8pt, y: 5pt), radius: 2pt
  )[#text(fill: white, weight: "bold", size: 10.5pt)[#title]]
  v(0.3em)
}

#let sub-head(title) = {
  v(0.35em)
  text(weight: "semibold", fill: teal-dark, size: 10pt)[#title]
  v(0.1em)
  line(length: 100%, stroke: 0.5pt + border)
  v(0.2em)
}

#let kpi(label, value, color: navy) = rect(
  fill: light-bg, stroke: 0.75pt + border,
  inset: (x: 7pt, y: 5pt), radius: 3pt, width: 100%
)[
  #set align(center)
  #text(size: 7.5pt, fill: muted)[#label]
  #linebreak()
  #text(weight: "bold", size: 10.5pt, fill: color)[#value]
]

#let stripe(i) = if calc.odd(i) { light-bg } else { white }
#let sign-color(sign) = if sign == "pos" { green } else { red }

#let tbl-stroke(x, cols) = (
  bottom: 0.5pt + border,
  right: if x < cols - 1 { 0.5pt + border } else { none }
)

// ── Title Page ────────────────────────────────────────────────────────────────
// Header and footer are suppressed on page 1 via the counter check above.

// Top branding strip
#rect(
  fill: navy, width: 100%,
  inset: (x: 14pt, y: 11pt), radius: 3pt
)[
  #grid(
    columns: (1fr, auto),
    align: (left + horizon, right + horizon),
    [
      #text(fill: white, weight: "bold", size: 11pt)[Raborn Decision Sciences]
      #linebreak()
      #text(fill: teal, size: 9pt, tracking: 0.04em)[Direct Care Analytics]
    ],
    [
      #text(fill: rgb("#94A3B8"), size: 8.5pt)[#d.report_date]
    ]
  )
]

#v(1.4in)

// Practice name and report type
#align(center)[
  #text(
    size: 8.5pt, fill: muted, weight: "semibold", tracking: 0.1em
  )[PRACTICE FINANCIAL ANALYSIS REPORT]
  #v(0.5em)
  #line(length: 2.8in, stroke: 2pt + teal)
  #v(0.65em)
  #text(size: 26pt, weight: "bold", fill: navy)[#d.practice_name]
  #v(0.25em)
  #text(size: 10.5pt, fill: muted)[Practice ID: #d.practice_id]
]

#v(1.1in)

// Metadata KPI row
#grid(
  columns: (1fr, 1fr, 1fr),
  column-gutter: 8pt,
  kpi("Data Frequency",
    if d.frequency == "weekly" { "Weekly" } else { "Monthly" }),
  kpi("Workflow",
    if d.workflow == "scenario" { "Plan My Practice" } else { "Historical Upload" }),
  kpi("Report Date", d.report_date),
)

#v(0.75em)

// Report contents list
#rect(
  fill: light-bg, stroke: 0.75pt + border,
  inset: (x: 12pt, y: 10pt), radius: 3pt, width: 100%
)[
  #text(size: 8pt, weight: "semibold", fill: muted, tracking: 0.06em)[THIS REPORT INCLUDES]
  #v(0.5em)
  #let bsec = text(fill: teal-dark, size: 9.5pt)[▸]
  #let section-items = ([Data Summary],)
    + if d.scenario  != none { ([Scenario Parameters],)    } else { () }
    + if d.breakeven != none { ([Break-even Analysis],)    } else { () }
    + if d.revenue   != none { ([Revenue Forecast],)       } else { () }
    + if d.target    != none { ([Income Target Analysis],) } else { () }
  #set list(marker: bsec, body-indent: 7pt, spacing: 5pt)
  #list(..section-items)
]

#v(1.2in)

#align(center)[
  #text(size: 8pt, fill: muted, style: "italic")[
    Confidential — For internal planning purposes only
  ]
]

// ── Scenario Parameters ───────────────────────────────────────────────────────
#if d.scenario != none [
  #pagebreak()
  #section-head("Scenario Parameters")

  #block(breakable: false)[
    #sub-head("Overhead Assumptions (per month)")
    #table(
      columns: (1fr, 1fr, 1fr, 1fr),
      fill: (_, y) => stripe(y),
      stroke: (x, y) => tbl-stroke(x, 4),
      inset: (x: 6pt, y: 4pt),
      table.header(
        text(weight: "bold")[Rent & Facility],
        text(weight: "bold")[Staff & Payroll],
        text(weight: "bold")[EHR & Software],
        text(weight: "bold")[Malpractice Ins.]
      ),
      d.scenario.overhead.rent_fmt,
      d.scenario.overhead.payroll_fmt,
      d.scenario.overhead.ehr_fmt,
      d.scenario.overhead.malpractice_fmt,
    )
  ]
  #v(0.3em)
  #block(breakable: false)[
    #table(
      columns: (1fr, 1fr, 1fr),
      fill: (_, y) => stripe(y),
      stroke: (x, y) => tbl-stroke(x, 3),
      inset: (x: 6pt, y: 4pt),
      table.header(
        text(weight: "bold")[Supplies & Labs],
        text(weight: "bold")[Other Overhead],
        text(weight: "bold")[Total Monthly Overhead]
      ),
      d.scenario.overhead.supplies_fmt,
      d.scenario.overhead.other_fmt,
      text(weight: "bold")[#d.scenario.overhead.total_fmt],
    )
  ]

  #block(breakable: false)[
    #sub-head("Revenue Assumptions")
    #grid(
      columns: (1fr, 1fr, 1fr, 1fr),
      column-gutter: 6pt,
      kpi("Starting Panel Size",  d.scenario.panel_size_fmt + " members"),
      kpi("Monthly Fee",          d.scenario.monthly_fee_fmt + "/member"),
      kpi("Panel Growth",         str(d.scenario.monthly_growth) + " members/mo"),
      kpi("Scenario Length",      str(d.scenario.n_months) + " months from " + d.scenario.start_period),
    )
  ]

  #if d.scenario.ffs.new_patients_mo > 0 or d.scenario.ffs.followups_mo > 0 [
    #block(breakable: false)[
      #v(0.4em)
      #sub-head("Fee-for-Service Assumptions")
      #grid(
        columns: (1fr, 1fr, 1fr),
        column-gutter: 6pt,
        kpi("New Visit Fee × Patients/mo",
          d.scenario.ffs.new_visit_fee_fmt + " × " + str(d.scenario.ffs.new_patients_mo)),
        kpi("Follow-up Fee × Visits/mo",
          d.scenario.ffs.followup_fee_fmt + " × " + str(d.scenario.ffs.followups_mo)),
        kpi("Other Monthly Income", d.scenario.ffs.other_income_fmt),
      )
    ]
  ]
]

// ── Data Summary ──────────────────────────────────────────────────────────────
#pagebreak()
#section-head("Data Summary")

#block(breakable: false)[
  #sub-head("Overhead by " + d.period_label)
  #table(
    columns: (1.5fr, 1fr, 1fr, 1fr),
    fill: (_, y) => stripe(y),
    stroke: (x, y) => tbl-stroke(x, 4),
    inset: (x: 6pt, y: 4pt),
    table.header(
      text(weight: "bold")[Period],
      text(weight: "bold", fill: navy)[Gross Overhead],
      text(weight: "bold")[Refunds],
      text(weight: "bold", fill: navy)[Net Overhead]
    ),
    ..d.overhead_summary.map(row => (
      row.period,
      row.gross_fmt,
      row.refunds_fmt,
      text(weight: "semibold")[#row.net_fmt],
    )).flatten()
  )
]

#if d.has_income [
  #v(0.3em)
  #block(breakable: false)[
    #sub-head("Income by " + d.period_label)
    #table(
      columns: (1.5fr, 1fr),
      fill: (_, y) => stripe(y),
      stroke: (x, y) => tbl-stroke(x, 2),
      inset: (x: 6pt, y: 4pt),
      table.header(
        text(weight: "bold")[Period],
        text(weight: "bold", fill: green)[Total Revenue]
      ),
      ..d.income_summary.map(row => (
        row.period,
        text(fill: green, weight: "semibold")[#row.revenue_fmt],
      )).flatten()
    )
  ]
] else [
  #v(0.3em)
  #rect(
    fill: light-bg, stroke: 0.75pt + border,
    inset: (x: 10pt, y: 8pt), radius: 3pt, width: 100%
  )[
    #text(fill: muted, size: 9.5pt)[
      No income data available.
      #if d.workflow == "upload" [
        The uploaded file did not contain income records. Projections used a
        proportional overhead proxy.
      ] else [
        Revenue projections are based on the scenario parameters above.
      ]
    ]
  ]
]

#if d.membership_tiers != none and d.membership_tiers.len() > 0 [
  #v(0.3em)
  #block(breakable: false)[
    #sub-head("Membership Profile")
    #if d.membership_tiers.len() == 1 [
      #grid(
        columns: (1fr, 1fr, 1fr),
        column-gutter: 6pt,
        kpi("Panel Size",
          if d.panel_size_fmt != none { d.panel_size_fmt + " members" } else { "—" }),
        kpi("Monthly Fee",
          if d.membership_fee_fmt != none { d.membership_fee_fmt + "/member" } else { "—" }),
        kpi("Tier",
          if d.membership_tiers.at(0).label != "" { d.membership_tiers.at(0).label } else { "General" }),
      )
    ] else [
      #table(
        columns: (1fr, 1fr, 1fr),
        fill: (_, y) => stripe(y),
        stroke: (x, y) => tbl-stroke(x, 3),
        inset: (x: 6pt, y: 4pt),
        table.header(
          text(weight: "bold")[Tier],
          text(weight: "bold")[Members],
          text(weight: "bold")[Monthly Fee / Member]
        ),
        ..d.membership_tiers.map(tier => (
          if tier.label != "" { tier.label } else { "—" },
          tier.members_fmt,
          tier.fee_fmt,
        )).flatten(),
        text(weight: "semibold")[Total / Weighted Avg],
        text(weight: "semibold")[#d.panel_size_fmt members],
        text(weight: "semibold")[#d.membership_fee_fmt / member],
      )
    ]
  ]
] else if d.panel_size_fmt != none or d.membership_fee_fmt != none [
  #v(0.3em)
  #block(breakable: false)[
    #sub-head("Membership Profile")
    #grid(
      columns: (1fr, 1fr),
      column-gutter: 6pt,
      kpi("Panel Size",  if d.panel_size_fmt    != none { d.panel_size_fmt    + " members" } else { "—" }),
      kpi("Monthly Fee", if d.membership_fee_fmt != none { d.membership_fee_fmt + "/member"  } else { "—" }),
    )
  ]
]

// ── Break-even Analysis ───────────────────────────────────────────────────────
#if d.breakeven != none [
  #let bkevn = d.breakeven
  #let s-col = sign-color(bkevn.surplus_sign)

  #pagebreak()
  #section-head("Break-even Analysis")

  #block(breakable: false)[
    #grid(
      columns: (1fr, 1fr, 1fr, 1fr),
      column-gutter: 6pt,
      kpi("Break-even Status", bkevn.status,
          color: if bkevn.status in ("Achieved", "Likely") { green }
                 else if bkevn.status in ("Not Achievable", "Unlikely") { red }
                 else if bkevn.status == "Achieved (at risk)" { amber }
                 else { teal-dark }),
      kpi("Current Revenue",           bkevn.current_revenue_fmt),
      kpi("Current Overhead",          bkevn.current_overhead_fmt),
      kpi("Current Surplus / Deficit", bkevn.current_surplus_fmt, color: s-col),
    )

    #if bkevn.breakeven_date != none [
      #v(0.3em)
      #grid(
        columns: (1fr, 1fr, 2fr),
        column-gutter: 6pt,
        kpi("Projected Break-even",  bkevn.breakeven_date, color: teal-dark),
        kpi("Periods to Break-even",
            str(bkevn.periods_to_breakeven) + " " + d.frequency.replace("ly", "s")),
        [],
      )
    ]
  ]

  #if bkevn.has_plot [
    #v(0.4em)
    #block(breakable: false)[
      #sub-head("Break-even Forecast Chart")
      #image("breakeven.png", width: 100%)
    ]
  ]

  #if bkevn.interpretation.len() > 0 [
    #block(breakable: false)[
      #sub-head("Interpretation")
      #text(size: 9.5pt)[#bkevn.interpretation.at(0)]
    ]
    #for para in bkevn.interpretation.slice(1) [
      #v(0.5em)
      #text(size: 9.5pt)[#para]
    ]
  ]

  #v(0.4em)
  #block(breakable: false)[
    #sub-head("Forecast Data")
    #table(
      columns: (1.5fr, 1fr, 1fr, 1fr),
      fill: (_, y) => stripe(y),
      stroke: (x, y) => tbl-stroke(x, 4),
      inset: (x: 6pt, y: 4pt),
      table.header(
        text(weight: "bold")[Period],
        text(weight: "bold")[Revenue],
        text(weight: "bold")[Overhead],
        text(weight: "bold")[Surplus / Deficit]
      ),
      ..bkevn.table.map(row => (
        row.period,
        row.revenue_fmt,
        row.overhead_fmt,
        text(fill: sign-color(row.surplus_sign), weight: "semibold")[#row.surplus_fmt],
      )).flatten()
    )
  ]
]

// ── Revenue Forecast ──────────────────────────────────────────────────────────
#if d.revenue != none [
  #let rev = d.revenue
  #let pct-str = if rev.pct_change >= 0 { "+" } else { "" }
  #let pct-str = pct-str + str(rev.pct_change) + "%"

  #pagebreak()
  #section-head("Revenue Forecast")

  #block(breakable: false)[
    #grid(
      columns: (1fr, 1fr, 1fr, 1fr),
      column-gutter: 6pt,
      kpi("Current Revenue", rev.current_revenue_fmt),
      kpi("Projected (" + rev.end_period + ")", rev.projected_end_fmt,
          color: if rev.pct_change_sign == "pos" { green } else { red }),
      kpi("Change over Horizon", pct-str,
          color: if rev.pct_change_sign == "pos" { green } else { red }),
      kpi("Method / Horizon",
          if d.forecast_method != none { upper(d.forecast_method) } else { "—" } + " / " +
          if d.forecast_horizon != none {
            str(d.forecast_horizon) + " " + d.frequency.replace("ly", "s")
          } else { "—" }),
    )
  ]

  #if rev.has_plot [
    #v(0.4em)
    #block(breakable: false)[
      #sub-head("Revenue Forecast Chart")
      #image("revenue.png", width: 100%)
    ]
  ]

  #if rev.interpretation.len() > 0 [
    #block(breakable: false)[
      #sub-head("Interpretation")
      #text(size: 9.5pt)[#rev.interpretation.at(0)]
    ]
    #for para in rev.interpretation.slice(1) [
      #v(0.5em)
      #text(size: 9.5pt)[#para]
    ]
  ]

  #v(0.4em)
  #block(breakable: false)[
    #sub-head("Forecast Data")
    #table(
      columns: (1.5fr, 1fr, 1fr, 1fr),
      fill: (_, y) => stripe(y),
      stroke: (x, y) => tbl-stroke(x, 4),
      inset: (x: 6pt, y: 4pt),
      table.header(
        text(weight: "bold")[Period],
        text(weight: "bold")[Revenue (forecast)],
        text(weight: "bold")[#d.ci_label — Low],
        text(weight: "bold")[#d.ci_label — High]
      ),
      ..rev.table.map(row => (
        row.period,
        text(weight: "semibold")[#row.revenue_fmt],
        text(fill: muted)[#row.lo_fmt],
        text(fill: muted)[#row.hi_fmt],
      )).flatten()
    )
  ]
]

// ── Income Target Analysis ────────────────────────────────────────────────────
#if d.target != none [
  #let tgt = d.target
  #let g-col = sign-color(tgt.gap_sign)

  #pagebreak()
  #section-head("Income Target Analysis")

  #block(breakable: false)[
    #grid(
      columns: (1fr, 1fr, 1fr, 1fr),
      column-gutter: 6pt,
      kpi("Net Income Target",    tgt.target_income_fmt),
      kpi("Status",               tgt.status,
          color: if tgt.status in ("Achieved", "Likely") { green }
                 else if tgt.status in ("Not Achievable", "Unlikely") { red }
                 else if tgt.status == "Achieved (at risk)" { amber }
                 else { teal-dark }),
      kpi("Required Revenue Now", tgt.required_revenue_fmt),
      kpi("Current Gap",          tgt.current_gap_fmt, color: g-col),
    )

    #if tgt.target_date != none [
      #v(0.3em)
      #grid(
        columns: (1fr, 2fr),
        column-gutter: 6pt,
        kpi("Target Date", tgt.target_date, color: teal-dark),
        [],
      )
    ]
  ]

  #if tgt.has_plot [
    #v(0.4em)
    #block(breakable: false)[
      #sub-head("Income Target Forecast Chart")
      #image("target.png", width: 100%)
    ]
  ]

  #if tgt.interpretation.len() > 0 [
    #block(breakable: false)[
      #sub-head("Interpretation")
      #text(size: 9.5pt)[#tgt.interpretation.at(0)]
    ]
    #for para in tgt.interpretation.slice(1) [
      #v(0.5em)
      #text(size: 9.5pt)[#para]
    ]
  ]

  #v(0.4em)
  #block(breakable: false)[
    #sub-head("Forecast Data")
    #table(
      columns: (1.5fr, 1fr, 1fr, 1fr),
      fill: (_, y) => stripe(y),
      stroke: (x, y) => tbl-stroke(x, 4),
      inset: (x: 6pt, y: 4pt),
      table.header(
        text(weight: "bold")[Period],
        text(weight: "bold")[Revenue (forecast)],
        text(weight: "bold")[Required Revenue],
        text(weight: "bold")[Gap]
      ),
      ..tgt.table.map(row => (
        row.period,
        text(weight: "semibold")[#row.revenue_fmt],
        row.req_fmt,
        text(fill: sign-color(row.gap_sign), weight: "semibold")[#row.gap_fmt],
      )).flatten()
    )
  ]
]

// ── Disclaimer ────────────────────────────────────────────────────────────────
#v(0.8em)
#line(length: 100%, stroke: 0.5pt + border)
#v(0.2em)
#text(size: 8pt, fill: muted)[
  *Disclaimer:* These projections are generated from historical data and scenario
  assumptions. They are estimates, not guarantees. Actual results depend on
  enrollment changes, fee-for-service volume, overhead fluctuations, and other
  practice-specific factors. This report is for internal planning purposes only
  and does not constitute financial or legal advice.
]
