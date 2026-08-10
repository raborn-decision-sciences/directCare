# -- Results-tab helpers -------------------------------------------------------

# A single paragraph from an interpret_*() string, rendered as a plain <p>
# -- unless it contains "\n- " bullet lines (interpret_projection()'s
# goal-seek paragraph does this when more than one lever is achievable, see
# .describe_goal_seek()'s own doc), in which case the non-bullet lines
# become an intro <p> and the "- "-prefixed lines become a real <ul>. The
# Pro badge, when present, always prefixes the intro/only <p>, never a
# bullet item.
.render_paragraph <- function(text, is_pro) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  is_bullet <- startsWith(lines, "- ")

  badge_prefix <- if (isTRUE(is_pro)) tagList(.pro_badge(), " ") else NULL

  if (!any(is_bullet)) {
    return(tags$p(badge_prefix, text))
  }

  intro <- paste(lines[!is_bullet], collapse = " ")
  items <- sub("^- ", "", lines[is_bullet])

  tagList(
    tags$p(badge_prefix, intro),
    tags$ul(lapply(items, tags$li))
  )
}

# interpret_*() functions return plain text with "\n\n" paragraph breaks
# (not HTML -- a deliberate directCarePlanR design choice since Typst, not
# a live UI, was its near-term consumer). Wrap each paragraph in <p> for
# display here; this is display formatting, an app-side concern.
#
# interpret_projection()'s "pro_paragraph" attribute (see its own doc) marks
# which paragraphs came from a Pro-exclusive sensitivity_decomposition/
# goal_seek addition -- read here (not re-derived) so a Pro user sees a
# small badge on exactly those, without this function needing to know
# anything about billing tiers itself. Absent on every other interpret_*()
# output (revenue, capital, scenario-comparison narrative), so this is a
# no-op there.
.paragraphs_to_html <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(NULL)
  }
  paragraphs <- trimws(strsplit(text, "\n\n", fixed = TRUE)[[1]])
  is_pro <- attr(text, "pro_paragraph")

  keep <- nzchar(paragraphs)
  paragraphs <- paragraphs[keep]
  is_pro <- if (is.null(is_pro)) rep(FALSE, length(paragraphs)) else is_pro[keep]

  tagList(lapply(seq_along(paragraphs), function(i) {
    .render_paragraph(paragraphs[i], is_pro[i])
  }))
}

.fmt_dollar <- function(x) scales::dollar(x, accuracy = 1)

# The 6 fields shown for a single `dcPlanR_market_context` object (as
# returned by directCarePlanR::build_market_context()), as {label, value}
# pairs -- shared by the single-location Market Context card and the
# Pro-exclusive location-comparison table below, so the set/order/labels
# live in exactly one place instead of two.
.market_context_rows <- function(context) {
  list(
    list(
      label = "Location",
      value = paste0(context$geography$county_name, ", ", context$geography$state_abb)
    ),
    list(
      label = "Population",
      value = format(context$population_income$population, big.mark = ",")
    ),
    list(
      label = "Median household income",
      value = .fmt_dollar(context$population_income$median_household_income)
    ),
    list(
      label = "Uninsured rate",
      value = scales::percent(context$uninsured$uninsured_rate, accuracy = 0.1)
    ),
    list(
      label = "Physician density",
      value = paste0(round(context$physician_density$physician_density_per_10k, 1), " per 10,000 population")
    ),
    list(
      label = "Known nearby direct care practices",
      value = as.character(nrow(context$landscape))
    )
  )
}

# Plain-language description of what the three Scenario Projections lines
# actually vary, sourced directly from directCarePlanR::project_scenarios()'s
# built-in defaults (conservative: ramp_months_multiplier = 1.5,
# overhead_multiplier = 1.1; optimistic: ramp_months_multiplier = 0.75,
# overhead_multiplier = 0.9) -- this app never passes a custom
# `scenario_params`, so these are always the actual numbers in effect, not
# just illustrative ones. Kept as a small, muted block near the bottom of
# the page rather than a card of its own -- it's a definitional aside, not
# a result.
.scenario_footnote <- function() {
  tags$p(
    class = "small text-muted mt-3 mb-0",
    tags$strong("Base"),
    " reflects the assumptions you entered. ",
    tags$strong("Conservative"),
    " assumes membership growth takes 50% longer to reach your target ",
    "panel size, with monthly overhead 10% higher than entered. ",
    tags$strong("Optimistic"),
    " assumes membership growth reaches your target panel size 25% ",
    "faster, with monthly overhead 10% lower than entered."
  )
}

# Human-readable labels for calc_startup_costs()'s line_items names, which
# are the raw internal names passed in from mod_plan_inputs.R's
# cost_ehr/cost_equipment/etc inputs (ehr_setup, equipment, licensing,
# marketing, other). Falls back to the raw name for any key not listed here,
# rather than dropping it, in case new cost categories get added later.
.cost_item_labels <- c(
  ehr_setup = "EHR setup",
  equipment = "Equipment",
  licensing = "Licensing",
  marketing = "Marketing",
  other = "Other startup costs",
  # "Single total" mode in mod_plan_inputs.R submits one line item keyed
  # "total" instead of the 5 itemized categories above.
  total = "Total startup costs"
)
# `overrides` is an optional named chr vector (slug -> custom label),
# typically `r$cost_item_labels` set via the "Customize Category Labels"
# editor in Plan Inputs' Capital Requirements card (matches
# directCareAnalytics's .pretty_cat() override pattern).
.humanize_cost_items <- function(names_vec, overrides = NULL) {
  labels_map <- .cost_item_labels
  if (!is.null(overrides)) {
    labels_map[names(overrides)] <- overrides
  }
  labels <- unname(labels_map[names_vec])
  missing <- is.na(labels)
  labels[missing] <- names_vec[missing]
  labels
}

#' results UI Function
#'
#' @param id Internal parameter for `{shiny}`.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("content")),
    .branded_footer()
  )
}

#' results Server Functions
#'
#' @param id Internal parameter for `{shiny}`.
#' @param r Shared reactiveValues object, populated by mod_plan_inputs.
#' @param parent_session The top-level session, used to switch the active
#'   navbar tab back to Plan Inputs.
#'
#' @noRd
mod_results_server <- function(id, r, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$btn_back, {
      updateNavbarPage(parent_session %||% session, "main_nav", selected = "plan_inputs")
    })

    output$content <- renderUI({
      if (is.null(r$projections)) {
        # No inline Back button here -- the sticky nav bar above always
        # shows one for this tab now (see this module's `nav_footer` return
        # value), so a second copy here would be a duplicate.
        return(
          card(
            card_body(
              class = "text-center text-muted py-5",
              bsicons::bs_icon("arrow-left-circle", size = "2em"),
              tags$p("Build a plan in the Plan Inputs tab to see results here.")
            )
          )
        )
      }

      base_projection <- r$projections[r$projections$scenario == "base", ]
      recovery_idx <- which(base_projection$cumulative_net_income >= 0)[1]
      recovery_label <- if (is.na(recovery_idx)) "Not within horizon" else paste0("Month ", base_projection$month[recovery_idx])
      combined_capital <- r$capital$startup_costs$total + r$capital$personal_runway$total

      tagList(
        layout_columns(
          col_widths = c(4, 4, 4),
          value_box(
            title = "Revenue at full ramp",
            value = .fmt_dollar(utils::tail(r$revenue$total$total_revenue, 1)),
            theme = "primary",
            height = "90px"
          ),
          value_box(
            title = "Cash-flow recovery (base scenario)",
            value = recovery_label,
            theme = "secondary",
            height = "90px"
          ),
          value_box(
            title = "Capital required",
            value = .fmt_dollar(combined_capital),
            theme = "info",
            height = "90px"
          )
        ),
        layout_columns(
          col_widths = c(4, 8),
          fill = FALSE,
          fillable = FALSE,
          # Gated at pro+ (Market Context is a paid feature, see
          # STRIPE_BILLING.md's v1 gating scope). .has_paid_plan()/
          # .locked_feature_card() live in utils_billing.R.
          if (!.has_paid_plan(r$plan_tier)) {
            .locked_feature_card(
              title = "Market Context",
              description = paste0(
                "See local population, income, insurance coverage, and ",
                "nearby direct care competition for your practice's area."
              ),
              ns = ns,
              btn_id = "btn_see_plans_market",
              class = NULL
            )
          } else {
            card(
              card_header(bsicons::bs_icon("geo-alt"), " Market Context"),
              card_body(
                tagList(lapply(.market_context_rows(r$market_context), function(row) {
                  tags$p(tags$strong(row$label, ": "), row$value)
                }))
              )
            )
          },
          card(
            full_screen = TRUE,
            card_header(bsicons::bs_icon("graph-up-arrow"), " Scenario Projections"),
            card_body(
              plotOutput(ns("projection_plot"), height = "320px")
            )
          )
        ),
        # Pro-exclusive: side-by-side market comparison against the
        # additional ZIP codes entered on Plan Inputs, if any. `requested`
        # is tracked regardless of tier (mod_plan_inputs.R's .build_plan())
        # so a non-Pro practice that filled these in still sees the
        # upsell -- matching every other Pro teaser's "only show up once
        # it's actually relevant" behavior (scenario_comparison_card
        # below is the closest precedent). Renders nothing at all when no
        # compare ZIPs were entered, same as that precedent.
        if (isTRUE((r$market_context_compare_requested %||% 0L) > 0L)) {
          layout_columns(
            col_widths = 12,
            fill = FALSE,
            fillable = FALSE,
            if (.has_pro_plan(r$plan_tier) && length(r$market_context_compare) > 0L) {
              all_contexts <- c(list(r$market_context), r$market_context_compare)
              all_rows <- lapply(all_contexts, .market_context_rows)
              n_fields <- length(all_rows[[1]])
              col_labels <- c("Primary", paste("Compare", seq_along(r$market_context_compare)))
              card(
                card_header(.pro_badge(), " Location Comparison"),
                card_body(
                  tags$div(
                    class = "table-responsive",
                    tags$table(
                      class = "table table-sm",
                      tags$thead(tags$tr(
                        tags$th(""),
                        lapply(col_labels, function(lbl) tags$th(class = "text-nowrap", lbl))
                      )),
                      tags$tbody(lapply(seq_len(n_fields), function(field_i) {
                        tags$tr(
                          tags$td(tags$strong(all_rows[[1]][[field_i]]$label)),
                          lapply(all_rows, function(rows) tags$td(rows[[field_i]]$value))
                        )
                      }))
                    )
                  )
                )
              )
            } else if (!.has_pro_plan(r$plan_tier)) {
              card(
                card_header(bsicons::bs_icon("geo-alt"), " Location Comparison"),
                card_body(
                  .pro_upsell_note(
                    ns,
                    "Pro also compares market fundamentals across the additional locations you entered, side by side with your primary one.",
                    btn_id = "btn_see_plans_pro_location_compare"
                  )
                )
              )
            }
          )
        },
        layout_columns(
          col_widths = c(6, 6),
          fill = FALSE,
          fillable = FALSE,
          card(
            full_screen = TRUE,
            card_header(bsicons::bs_icon("cash-coin"), " Monthly Revenue"),
            card_body(DT::DTOutput(ns("revenue_table")))
          ),
          card(
            full_screen = TRUE,
            card_header(bsicons::bs_icon("piggy-bank"), " Capital Requirements"),
            card_body(
              tags$p(tags$strong("Startup costs: "), .fmt_dollar(r$capital$startup_costs$total)),
              DT::DTOutput(ns("startup_table")),
              tags$p(class = "mt-3", tags$strong("Personal runway: "), .fmt_dollar(r$capital$personal_runway$total)),
              tags$p(class = "text-muted small", r$capital$personal_runway$months_coverage, " months at ", .fmt_dollar(r$capital$personal_runway$monthly_expenses), "/month")
            )
          )
        ),
        layout_columns(
          col_widths = 12,
          fill = FALSE,
          fillable = FALSE,
          card(
            card_header(bsicons::bs_icon("lightbulb"), " Interpretation"),
            card_body(
              tags$h6("Revenue"),
              .paragraphs_to_html(r$interpretations$revenue),
              tags$h6(class = "mt-3", "Projections"),
              .paragraphs_to_html(r$interpretations$projection),
              if (!.has_pro_plan(r$plan_tier)) {
                # Which Pro perk to advertise depends on which one would
                # actually have fired for this plan -- goal-seek only ever
                # says anything when the base scenario doesn't recover
                # (mirrors mod_plan_inputs.R's base_recovers gate), the
                # sensitivity breakdown otherwise.
                .pro_upsell_note(
                  ns,
                  if (is.na(recovery_idx)) {
                    "Pro also tells you what it would take -- an overhead cut, a faster ramp, or both -- to recover within your horizon."
                  } else {
                    "Pro also breaks down which assumption -- membership ramp speed or overhead -- drives more of the conservative-to-optimistic spread."
                  },
                  btn_id = "btn_see_plans_pro_sensitivity"
                )
              },
              tags$h6(class = "mt-3", "Capital"),
              .paragraphs_to_html(r$interpretations$capital)
            )
          ),
          uiOutput(ns("scenario_comparison_card"))
        ),
        .scenario_footnote()
      )
    })

    # Pro-exclusive: compares saved plan_scenarios' cash-flow recovery
    # timing. Unlike DCA's dca_forecast_scenarios (which snapshot full
    # computed results, see directCareAnalytics's equivalent reactive),
    # plan_scenarios stores inputs only -- see SAVED_SCENARIOS.md -- so
    # each saved scenario's assumptions must be rebuilt and re-projected
    # here rather than just read back. req(r$projections) is a proxy
    # invalidation trigger (mirroring DCA's req(adj_breakeven())): this
    # re-queries the DB whenever the practice (re)builds a plan or loads a
    # saved scenario (on_load() also calls .build_plan(), which sets
    # r$projections), but NOT immediately after a bare Save -- a known,
    # accepted limitation, since mod_scenario_slots_server() exposes no
    # "scenario list changed" signal to invalidate on directly.
    scenario_compare_state <- reactive({
      req(r$projections)
      req(r$practice_id)
      con <- directCareAuth::db_connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      listed <- directCareScenarios::scenario_list(con, "plan_scenarios", r$practice_id)
      if (nrow(listed) < 2L || !.has_pro_plan(r$plan_tier)) {
        return(list(count = nrow(listed), narrative = NULL))
      }

      scenarios <- lapply(listed$id, function(id) {
        loaded <- directCareScenarios::scenario_load_json(con, "plan_scenarios", id)
        if (is.null(loaded)) {
          return(NULL)
        }
        assumptions <- .assumptions_from_saved_inputs(loaded$inputs)
        horizon <- loaded$inputs$horizon_months %||% 24L
        # A scenario saved mid-edit (e.g. both revenue toggles off) can't
        # be projected -- tryCatch() skips it rather than erroring the
        # whole comparison out for every other valid saved scenario.
        proj <- tryCatch(
          directCarePlanR::project_practice(assumptions, horizon_months = horizon),
          error = function(e) NULL
        )
        if (is.null(proj)) {
          return(NULL)
        }
        idx <- which(proj$cumulative_net_income >= 0)[1]
        list(
          label = loaded$label,
          recovery_month = if (is.na(idx)) NA_integer_ else proj$month[idx]
        )
      })
      scenarios <- Filter(Negate(is.null), scenarios)

      list(
        count = nrow(listed),
        narrative = if (length(scenarios) >= 2L) {
          directCarePlanR::compare_plan_scenarios(scenarios)
        } else {
          NULL
        }
      )
    })

    output$scenario_comparison_card <- renderUI({
      state <- scenario_compare_state()
      if (!is.null(state$narrative)) {
        card(
          card_header(
            class = "d-flex align-items-center gap-2",
            bsicons::bs_icon("bar-chart-line"), " Scenario Comparison",
            tags$span(class = "ms-auto", .pro_badge())
          ),
          card_body(.paragraphs_to_html(state$narrative))
        )
      } else if (!.has_pro_plan(r$plan_tier) && state$count >= 2L) {
        card(
          card_body(
            .pro_upsell_note(
              ns,
              paste0(
                "Pro also compares your saved scenarios' cash-flow recovery ",
                "timing, naming which one recovers soonest."
              ),
              btn_id = "btn_see_plans_pro_scenario_compare"
            )
          )
        )
      }
    })

    output$projection_plot <- renderPlot({
      r$dark_mode # dependency only -- forces a redraw when the theme toggles
      req(r$projections)
      ggplot2::ggplot(
        r$projections,
        ggplot2::aes(x = month, y = net_income, color = scenario)
      ) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::scale_color_manual(
          values = c(conservative = "#DC2626", base = "#172033", optimistic = "#14B8A6")
        ) +
        ggplot2::scale_y_continuous(labels = scales::dollar) +
        ggplot2::labs(x = "Month", y = "Net income", color = "Scenario") +
        ggplot2::theme_minimal()
    })

    output$revenue_table <- DT::renderDT({
      req(r$revenue)
      DT::datatable(
        r$revenue$total,
        options = list(pageLength = 6, dom = "tp"),
        rownames = FALSE,
        colnames = c("Month", "Membership Revenue", "Fee-for-Service Revenue", "Total Revenue")
      ) |>
        DT::formatCurrency(c("membership_revenue", "fee_revenue", "total_revenue"), digits = 0)
    })

    output$startup_table <- DT::renderDT({
      req(r$capital$startup_costs)
      items <- r$capital$startup_costs$line_items
      df <- data.frame(
        item = .humanize_cost_items(names(items), r$cost_item_labels),
        amount = as.numeric(items)
      )
      DT::datatable(
        df,
        options = list(dom = "t", paging = FALSE),
        rownames = FALSE,
        colnames = c("Item", "Amount")
      ) |>
        DT::formatCurrency("amount", digits = 0)
    })

    # Triggered from the "See plans" buttons/links swapped in for gated
    # features below (Market Context card, Download Report button, the
    # Projections interpretation's Pro upsell note) -- all three open the
    # same modal, defined once in utils_billing.R.
    observeEvent(input$btn_see_plans_pro_sensitivity, {
      .show_plans_modal()
    })
    observeEvent(input$btn_see_plans_pro_scenario_compare, {
      .show_plans_modal()
    })
    observeEvent(input$btn_see_plans_pro_location_compare, {
      .show_plans_modal()
    })
    observeEvent(input$btn_see_plans_market, {
      .show_plans_modal()
    })
    observeEvent(input$btn_see_plans_report, {
      .show_plans_modal()
    })

    # -- Async PDF report generation ------------------------------------------
    # render_plan_report() shells out to a real `typst compile` subprocess --
    # like DCA's own report_task (mod_projections.R in directCareAnalytics,
    # which this mirrors), that would otherwise block the *entire app* (every
    # other session, not just this one) for its duration, since this is a
    # single-process Shiny app. Unlike DCA, this app had no ExtendedTask/mirai
    # infrastructure at all before this -- report_task is the first, so it
    # gets its own new mirai::daemons() pool (run_app.R) rather than sharing
    # one.
    #
    # build_report_data() stays in the main process below (it reads `r`, a
    # reactiveValues object, which has no meaning inside a worker) -- only
    # its resulting plain list crosses into $invoke(). out_path is computed
    # in the main process (never via tempfile() inside the
    # mirai::mirai({...}) block itself, which would bind the path to the
    # long-lived daemon process's own session tempdir instead of a per-call
    # scope) and lands inside .report_output_dir() (utils_globals.R) so
    # run_app.R's periodic sweep can find it if this session ends before the
    # task resolves.
    #
    # render_plan_report()/build_report_data() are exported from
    # directCarePlanR (a genuinely installed dependency package, unlike
    # DCA's own render_report_pdf() which lives in the app package itself),
    # so directCarePlanR:: resolves normally inside the worker with no
    # install-time caveat.
    report_task <- ExtendedTask$new(function(out_path, data) {
      mirai::mirai(
        {
          result <- tryCatch(
            {
              directCarePlanR::render_plan_report(data, out_path)
              list(out_path = out_path)
            },
            error = function(e) {
              structure(list(message = conditionMessage(e)), class = "report_task_error")
            }
          )
          result
        },
        out_path = out_path,
        data = data
      )
    })
    bslib::bind_task_button(report_task, "btn_generate_report")

    # Path of the most recently generated report for this session, or NULL
    # before the first Generate click / while regeneration is in flight (see
    # the invoke observer below, which resets this immediately to hide the
    # stale download button). Per-session best-effort cleanup here;
    # run_app.R's periodic sweep is the real backstop for a session that
    # ends mid-generation (see .report_output_dir()'s comment).
    report_path <- reactiveVal(NULL)
    session$onSessionEnded(function() {
      p <- isolate(report_path())
      if (!is.null(p) && file.exists(p)) {
        unlink(p)
      }
    })

    observeEvent(input$btn_generate_report, {
      # Defense in depth: nav_footer below already swaps this entire button
      # for a "See plans" trigger when ungated, so a free-tier practice
      # can't normally reach this observer at all -- this just guards the
      # case where the click is somehow still fired.
      req(.has_paid_plan(r$plan_tier))

      old <- isolate(report_path())
      if (!is.null(old) && file.exists(old)) {
        unlink(old)
      }
      report_path(NULL)

      # report.typ's own humanize() only knows how to prettify the raw
      # slug (e.g. "ehr_setup" -> "EHR Setup") -- it has no idea a
      # custom label was saved in-app. Rename line_items to the
      # resolved display labels before they reach build_report_data()
      # so the PDF matches what's shown on-screen; re-running an
      # already-human label through humanize() is a no-op (no
      # underscores left to split on), so this is safe either way.
      capital_for_report <- r$capital
      if (!is.null(capital_for_report$startup_costs$line_items)) {
        names(capital_for_report$startup_costs$line_items) <- .humanize_cost_items(
          names(capital_for_report$startup_costs$line_items),
          r$cost_item_labels
        )
      }
      data <- directCarePlanR::build_report_data(
        market_context = r$market_context,
        market_context_compare = r$market_context_compare,
        revenue = r$revenue,
        projections = r$projections,
        capital = capital_for_report,
        interpretations = r$interpretations,
        practice_name = r$practice_name,
        plan_tier = r$plan_tier
      )

      out_path <- tempfile(fileext = ".pdf", tmpdir = .report_output_dir())
      report_task$invoke(out_path = out_path, data = data)
    })

    observeEvent(report_task$result(), {
      res <- report_task$result()
      if (inherits(res, "report_task_error")) {
        showNotification(res$message, type = "error", duration = 8)
        return(invisible())
      }
      report_path(res$out_path)
    })

    output$dl_report <- downloadHandler(
      filename = function() {
        safe_name <- gsub("[^A-Za-z0-9_-]", "-", r$practice_name %||% "practice")
        paste0("plan-", safe_name, "-", format(Sys.Date(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        # Defense in depth, same rationale as the invoke observer above --
        # re-checked here too, in case plan_tier changed between Generate
        # and Download (e.g. a mid-flight downgrade).
        req(.has_paid_plan(r$plan_tier))
        req(report_path())
        file.copy(report_path(), file)
      }
    )

    # Returned for the central output$main_nav_footer (app_server.R) to
    # render when Results is the active tab -- see app_ui.R's header
    # comment. Last step in the sequence, so Download Report (not a "Next"
    # tab) takes the forward slot.
    #
    # Three states, not two: Free tier always sees the "Unlock" upsell;
    # once paid, Generate Report appears first (report_path() is NULL for a
    # fresh session/regeneration), then Download Report once report_task
    # resolves. .has_paid_plan() is re-checked here (not just at
    # invoke/download time) so a mid-flight downgrade immediately hides
    # Download/Generate in favor of the upsell, even if report_path() is
    # still set from before the downgrade.
    nav_footer <- reactive({
      .tour_nav_footer(
        current_step = 2L,
        back = actionButton(
          ns("btn_back"),
          tagList(bsicons::bs_icon("arrow-left-circle"), " Back to Plan Inputs"),
          class = "btn-outline-secondary"
        ),
        forward = if (!.has_paid_plan(r$plan_tier)) {
          actionButton(
            ns("btn_see_plans_report"),
            tagList(bsicons::bs_icon("lock-fill"), " Unlock Download Report"),
            class = "btn-outline-primary"
          )
        } else if (is.null(report_path())) {
          input_task_button(
            ns("btn_generate_report"),
            "Generate Report",
            icon = bsicons::bs_icon("file-earmark-pdf"),
            class = "btn-primary"
          )
        } else {
          downloadButton(
            ns("dl_report"),
            tagList(bsicons::bs_icon("file-earmark-arrow-down"), " Download Report"),
            class = "btn-primary"
          )
        },
        extra = .scenario_slots_ui("scenario", r$plan_tier)
      )
    })

    list(nav_footer = nav_footer)
  })
}
