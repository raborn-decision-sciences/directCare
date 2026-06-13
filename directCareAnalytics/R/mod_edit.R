# -- Quick Estimator UI helper -------------------------------------------------
# Shown in place of the edit table when the practice is in manual-entry mode
# (r$transactions exists but has 0 rows -- i.e. no CSV has been uploaded).
# Takes `ns` so all input IDs are correctly namespaced.

# `vals` is a named list of previous input values (passed via isolate() from
# the renderUI so the form repopulates when the user clicks "Revise Estimates").
# Defaults to an empty list on first render, giving value = 0 for every field.
.quickstart_ui <- function(ns, vals = list()) {
  # Helper: return the saved value if present and finite, otherwise `default`.
  .v <- function(name, default = 0) {
    x <- vals[[name]]
    if (is.null(x) || length(x) != 1L || is.na(x)) default else x
  }

  card(
    card_header(
      class = "d-flex align-items-center gap-2",
      bs_icon("lightning-charge"),
      " Quick Estimator",
      tooltip(
        bs_icon("info-circle", title = "About the Quick Estimator"),
        paste0(
          "No GnuCash export needed. Fill in your monthly estimates and click ",
          "'Generate' -- the app will build a synthetic financial history for the ",
          "forecast models. Rough figures are fine; you can revise and regenerate ",
          "at any time."
        )
      )
    ),
    card_body(
      tags$p(
        class = "text-muted mb-3",
        "Enter your monthly estimates below to build a starting scenario for ",
        "the Projections tab. All fields are optional -- enter $0 to skip any item. ",
        "The surplus / deficit preview at the bottom of each column updates live."
      ),

      # Scrollable inputs block -- keeps the Synthetic History controls and the
      # Generate button visible below without requiring the user to scroll the
      # whole page. Padding-right leaves room for the scrollbar so it doesn't
      # overlap the rightmost column inputs.
      div(
        style = paste0(
          "max-height: 460px; overflow-y: auto; overflow-x: hidden; ",
          "padding-right: 6px; margin-bottom: 0.25rem;"
        ),

        layout_columns(
          col_widths = c(6, 6),

          # ---- Overhead column ------------------------------------------------
          tagList(
            tags$h6(
              class = "fw-semibold mb-3",
              bs_icon("receipt"),
              " Monthly Overhead"
            ),
            numericInput(
              ns("est_rent"),
              "Rent & Facility ($)",
              value = .v("est_rent"),
              min = 0,
              step = 50
            ),
            numericInput(
              ns("est_payroll"),
              "Staff & Payroll ($)",
              value = .v("est_payroll"),
              min = 0,
              step = 100
            ),
            numericInput(
              ns("est_ehr"),
              tagList(
                "EHR & Software ($)",
                tooltip(
                  bs_icon(
                    "info-circle",
                    title = "EHR and software subscriptions"
                  ),
                  paste0(
                    "Includes your EHR (e.g. Elation Health, Hint Health), ",
                    "practice management tools, telehealth platforms, and other ",
                    "software subscriptions."
                  )
                )
              ),
              value = .v("est_ehr"),
              min = 0,
              step = 10
            ),
            numericInput(
              ns("est_malpractice"),
              tagList(
                "Malpractice Insurance ($)",
                tooltip(
                  bs_icon("info-circle", title = "Malpractice premium"),
                  paste0(
                    "Enter your monthly average. ",
                    "If paid quarterly, divide the quarterly premium by 3; ",
                    "if annually, divide by 12."
                  )
                )
              ),
              value = .v("est_malpractice"),
              min = 0,
              step = 10
            ),
            numericInput(
              ns("est_supplies"),
              "Supplies & Labs ($)",
              value = .v("est_supplies"),
              min = 0,
              step = 10
            ),
            numericInput(
              ns("est_other_overhead"),
              "Other Overhead ($)",
              value = .v("est_other_overhead"),
              min = 0,
              step = 10
            ),
            uiOutput(ns("ovhd_total_ui"))
          ),

          # ---- Revenue column -------------------------------------------------
          tagList(
            tags$h6(
              class = "fw-semibold mb-3",
              bs_icon("cash-coin"),
              " Monthly Revenue"
            ),

            numericInput(
              ns("est_panel_size"),
              "Current panel size (members)",
              value = .v("est_panel_size"),
              min = 0,
              step = 1
            ),
            numericInput(
              ns("est_monthly_fee"),
              "Monthly fee ($/member)",
              value = .v("est_monthly_fee"),
              min = 0,
              step = 5
            ),
            numericInput(
              ns("est_monthly_growth"),
              tagList(
                "Expected new members / month",
                tooltip(
                  bs_icon("info-circle", title = "Panel growth rate"),
                  paste0(
                    "Approximate average new members per month. ",
                    "Leave at 0 to model a flat panel throughout the scenario."
                  )
                )
              ),
              value = .v("est_monthly_growth"),
              min = 0,
              step = 1
            ),

            tags$hr(class = "my-2"),
            tags$p(
              class = "small fw-semibold text-muted mb-1",
              "Fee-for-Service ",
              tags$em(class = "fw-normal", "(optional)")
            ),

            layout_columns(
              col_widths = c(7, 5),
              numericInput(
                ns("est_new_visit_fee"),
                "First-member visit fee ($)",
                value = .v("est_new_visit_fee"),
                min = 0,
                step = 10
              ),
              numericInput(
                ns("est_new_patients_mo"),
                "New patients / mo",
                value = .v("est_new_patients_mo"),
                min = 0,
                step = 1
              )
            ),
            layout_columns(
              col_widths = c(7, 5),
              numericInput(
                ns("est_followup_fee"),
                "Follow-up visit fee ($)",
                value = .v("est_followup_fee"),
                min = 0,
                step = 10
              ),
              numericInput(
                ns("est_followups_mo"),
                "Follow-ups / mo",
                value = .v("est_followups_mo"),
                min = 0,
                step = 1
              )
            ),
            numericInput(
              ns("est_other_income"),
              "Other income / month ($)",
              value = .v("est_other_income"),
              min = 0,
              step = 50
            ),
            uiOutput(ns("inc_total_ui"))
          )
        ), # /layout_columns

        hr(),

        # ---- History-length selector ---------------------------------------
        tags$h6(
          class = "fw-semibold mb-1",
          bs_icon("calendar3"),
          " Synthetic History"
        ),
        tags$p(
          class = "text-muted small mb-2",
          "More synthetic months give the forecast models more signal, ",
          "producing narrower confidence intervals. ",
          "12 months is a solid default."
        ),
        layout_columns(
          col_widths = c(5, 4, 3),
          radioButtons(
            ns("est_n_months"),
            "Months to generate",
            choices = c("6" = 6L, "12" = 12L, "18" = 18L, "24" = 24L),
            selected = .v("est_n_months", 12L),
            inline = TRUE
          ),
          selectInput(
            ns("est_start_month"),
            "Starting month",
            choices = stats::setNames(seq_len(12L), month.name),
            selected = .v(
              "est_start_month",
              as.integer(format(Sys.Date(), "%m"))
            )
          ),
          numericInput(
            ns("est_start_year"),
            "Year",
            value = .v(
              "est_start_year",
              as.integer(format(Sys.Date(), "%Y")) - 1L
            ),
            min = 2015L,
            max = as.integer(format(Sys.Date(), "%Y")),
            step = 1L
          )
        )
      ), # /scrollable div

      tags$div(
        class = "alert alert-info d-flex gap-2 py-2 px-3 mt-2 mb-2",
        style = "font-size: 0.8rem; line-height: 1.4;",
        bs_icon("info-circle-fill", size = "1rem"),
        tags$span(
          tags$strong("Heads up:"),
          " the values you enter are used to build a synthetic financial history.",
          " The forecast models treat that history as real data, so projections and",
          " interpretations reflect the ",
          tags$strong("end of the synthetic period"),
          " -- not your starting panel size or initial overhead figures."
        )
      ),
      input_task_button(
        ns("btn_generate"),
        "Generate Practice Scenario",
        icon = bs_icon("lightning-charge"),
        class = "btn-primary w-100 mt-1"
      )
    )
  )
}


# -- Period-summary helpers (module-internal) -----------------------------------

# Blank grid data frames pre-populated with all categories / sources
.ovhd_grid <- function() {
  data.frame(
    Category = c(
      "Rent",
      "Staff / Payroll",
      "Supplies",
      "Software",
      "Insurance",
      "Marketing",
      "Labs",
      "Equipment",
      "Licenses",
      "Education / Training",
      "Other"
    ),
    Amount = rep(0, 11),
    stringsAsFactors = FALSE
  )
}

.inc_grid <- function() {
  data.frame(
    Source = c("Membership Fee", "Fee-for-Service", "Other"),
    Amount = rep(0, 3),
    stringsAsFactors = FALSE
  )
}

# Account-level info used when building ingest_manual() data frames
.ovhd_acct <- list(
  "Rent" = list(acct = "Rent", full = "Expenses:Rent and Utilities:Rent"),
  "Staff / Payroll" = list(
    acct = "Payroll Expenses",
    full = "Expenses:Payroll Expenses"
  ),
  "Supplies" = list(acct = "Supplies", full = "Expenses:Supplies"),
  "Software" = list(acct = "Software", full = "Expenses:Software"),
  "Insurance" = list(acct = "Insurance", full = "Expenses:Insurance"),
  "Marketing" = list(acct = "Marketing", full = "Expenses:Marketing"),
  "Labs" = list(acct = "Labs", full = "Expenses:Labs"),
  "Equipment" = list(
    acct = "Equipment Purchase",
    full = "Expenses:Equipment:Equipment Purchase"
  ),
  "Licenses" = list(
    acct = "Licenses and Permits",
    full = "Expenses:Licenses and Permits"
  ),
  "Education / Training" = list(
    acct = "Education",
    full = "Expenses:Education"
  ),
  "Other" = list(acct = "Other", full = "Expenses:Other")
)

.inc_acct <- list(
  "Membership Fee" = list(
    acct = "Membership Fees",
    full = "Income:Membership Fees",
    src = "membership"
  ),
  "Fee-for-Service" = list(
    acct = "Fee-for-Service",
    full = "Income:Fee-for-Service",
    src = "fee_for_service"
  ),
  "Other" = list(acct = "Other Income", full = "Income:Other", src = "other")
)


#' Review & Edit Module UI
#'
#' Tab 2 -- Inline category editor, account-level re-map tool, and a panel
#' for adding manual transactions via `ingest_manual()`.
#'
#' @param id Module namespace ID.
#' @noRd
mod_edit_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("content"))
}


#' Review & Edit Module Server
#'
#' @param id Module namespace ID.
#' @param r Shared `reactiveValues` object from `app_server`.
#' @noRd
mod_edit_server <- function(id, r, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    valid_categories <- c(
      "rent",
      "staff",
      "supplies",
      "software",
      "insurance",
      "marketing",
      "labs",
      "equipment",
      "licenses",
      "education",
      "other"
    )

    # Tracks whether the Quick Estimator has already generated data.
    # A local reactiveVal so it doesn't pollute the shared r object.
    estimator_done <- reactiveVal(FALSE)

    # TRUE only while a CSV-sourced transaction table (nrow > 0) is loaded.
    # Used to gate csv_edit_ui so it only re-renders on data-load, not on
    # every individual row edit.
    has_csv_data <- reactive({
      !is.null(r$transactions) && nrow(r$transactions) > 0L
    })

    # Shared error/warning handler used by all ingest_manual() call sites.
    # Muffles expected warnings and shows a notification on hard errors.
    # Relies on R's lazy-evaluation promise mechanism — same pattern as
    # run_forecast() in mod_projections.R.
    quietly <- function(expr) {
      tryCatch(
        withCallingHandlers(expr, warning = function(w) {
          invokeRestart("muffleWarning")
        }),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
    }

    # -- Outer gate: select which state placeholder to show -------------------
    # Reads only boolean state (NULL check, nrow, estimator_done) so it
    # re-fires only when the active workflow changes, not on every data edit.
    output$content <- renderUI({
      if (is.null(r$transactions)) {
        return(
          card(
            card_body(
              class = "text-center text-muted py-5",
              bs_icon("arrow-left-circle", size = "2em"),
              p("Upload a GnuCash CSV on the Upload tab first.")
            )
          )
        )
      }
      if (nrow(r$transactions) == 0L) {
        if (!is.null(r$overhead_monthly) && is.null(r$scenario_inputs)) {
          return(uiOutput(ns("manual_entry_ui")))
        }
        return(uiOutput(ns("estimator_ui")))
      }
      uiOutput(ns("csv_edit_ui"))
    })

    # -- Aggregate manual-entry message card ----------------------------------
    # Shown when r$overhead_monthly is populated but r$scenario_inputs is NULL
    # (manual period-summary entry, no CSV and no Quick Estimator).
    output$manual_entry_ui <- renderUI({
      n <- nrow(r$overhead_monthly %||% data.frame())
      freq <- if (
        !is.null(r$overhead_monthly) &&
          "week_start" %in% names(r$overhead_monthly)
      ) {
        "weekly"
      } else {
        "monthly"
      }
      card(
        card_header(
          class = "d-flex align-items-center gap-2",
          bs_icon("info-circle"),
          " No Transactions to Review"
        ),
        card_body(
          tags$p(HTML(paste0(
            "You entered <strong>",
            n,
            " ",
            freq,
            " period",
            if (n != 1L) "s",
            "</strong> of aggregate data manually. ",
            "The Review & Edit tab is designed for correcting individual ",
            "transactions from a bookkeeping CSV export -- it does not apply ",
            "to manually entered period summaries."
          ))),
          tags$p(
            "Your data is ready to use. Head to the ",
            tags$strong("Summary"),
            " tab for an overview, or",
            " the ",
            tags$strong("Projections"),
            " tab to run a forecast."
          ),
          div(
            class = "d-flex gap-2 mt-3",
            actionButton(
              ns("btn_manual_to_summary"),
              tagList(bs_icon("bar-chart-line"), " Go to Summary"),
              class = "btn-outline-primary"
            ),
            actionButton(
              ns("btn_manual_to_projections"),
              tagList(bs_icon("graph-up-arrow"), " Go to Projections"),
              class = "btn-primary"
            )
          )
        )
      )
    })

    # -- Quick Estimator UI (form or post-generation summary) -----------------
    output$estimator_ui <- renderUI({
      if (estimator_done()) {
        n_periods <- nrow(r$overhead_monthly %||% data.frame())
        avg_overhead <- if (n_periods > 0) {
          fmt_dollar(mean(r$overhead_monthly$total_overhead, na.rm = TRUE))
        } else {
          "--"
        }
        first_rev <- if (n_periods > 0) {
          fmt_dollar(head(r$income_monthly$total_revenue, 1L))
        } else {
          "--"
        }
        last_rev <- if (n_periods > 0) {
          fmt_dollar(tail(r$income_monthly$total_revenue, 1L))
        } else {
          "--"
        }

        return(
          card(
            card_header(
              class = "text-bg-success d-flex align-items-center gap-2",
              bs_icon("check-circle"),
              " Scenario Ready"
            ),
            card_body(
              # Build the summary sentence as a raw HTML string so that
              # paste0() controls exactly where spaces and punctuation land.
              # Wrap in tags$p(HTML(...)) so bslib sees an explicit paragraph
              # element rather than a bare character vector (which it would
              # otherwise enclose in a <div>, breaking the inline flow).
              tags$p(HTML(paste0(
                "A <strong>",
                n_periods,
                "-month</strong> financial scenario ",
                "has been generated for <strong>",
                htmltools::htmlEscape(r$practice_name %||% ""),
                "</strong>. Click <strong>Go to Projections</strong> below, ",
                "or use the nav bar above."
              ))),
              layout_column_wrap(
                width = "180px",
                fill = FALSE,
                value_box(
                  "Avg monthly overhead",
                  avg_overhead,
                  theme = "secondary",
                  height = "80px"
                ),
                value_box(
                  "Starting revenue",
                  first_rev,
                  theme = "info",
                  height = "80px"
                ),
                value_box(
                  "Ending revenue",
                  last_rev,
                  theme = "primary",
                  height = "80px"
                ),
                value_box(
                  "Months generated",
                  n_periods,
                  theme = "secondary",
                  height = "80px"
                )
              ),
              div(
                class = "d-flex gap-2 mt-3 flex-wrap",
                actionButton(
                  ns("btn_restart_estimator"),
                  "Revise Estimates",
                  icon = bs_icon("arrow-counterclockwise"),
                  class = "btn-outline-secondary"
                ),
                actionButton(
                  ns("btn_go_projections"),
                  tagList(bs_icon("graph-up-arrow"), " Go to Projections"),
                  class = "btn-primary"
                )
              )
            )
          )
        )
      }

      # Collect all current input values via isolate() so the form
      # repopulates with the previous entries when "Revise Estimates"
      # is clicked.  isolate() prevents this renderUI from taking a
      # reactive dependency on the estimator inputs themselves.
      .quickstart_ui(
        ns,
        vals = list(
          est_rent = isolate(input$est_rent),
          est_payroll = isolate(input$est_payroll),
          est_ehr = isolate(input$est_ehr),
          est_malpractice = isolate(input$est_malpractice),
          est_supplies = isolate(input$est_supplies),
          est_other_overhead = isolate(input$est_other_overhead),
          est_panel_size = isolate(input$est_panel_size),
          est_monthly_fee = isolate(input$est_monthly_fee),
          est_monthly_growth = isolate(input$est_monthly_growth),
          est_new_visit_fee = isolate(input$est_new_visit_fee),
          est_new_patients_mo = isolate(input$est_new_patients_mo),
          est_followup_fee = isolate(input$est_followup_fee),
          est_followups_mo = isolate(input$est_followups_mo),
          est_other_income = isolate(input$est_other_income),
          est_n_months = isolate(input$est_n_months),
          est_start_month = isolate(input$est_start_month),
          est_start_year = isolate(input$est_start_year)
        )
      )
    })

    # -- CSV edit UI (layout with DT tables) ----------------------------------
    # Gated on has_csv_data() so it fires once when a CSV is loaded, not on
    # every subsequent row edit. Date range bounds are read via isolate() for
    # the same reason -- active_date_range() tracks the live filter state.
    output$csv_edit_ui <- renderUI({
      req(has_csv_data())
      txns <- isolate(r$transactions)
      date_lo <- if (nrow(txns) > 0) {
        min(txns$date, na.rm = TRUE)
      } else {
        Sys.Date()
      }
      date_hi <- if (nrow(txns) > 0) {
        max(txns$date, na.rm = TRUE)
      } else {
        Sys.Date()
      }

      layout_columns(
        col_widths = c(8, 4),
        # -- Left: transaction table ----------------------------------------
        card(
          full_screen = TRUE,
          card_header(
            tagList(
              bs_icon("table"),
              " Transactions",
              tooltip(
                bs_icon("info-circle", title = "Editing categories"),
                paste0(
                  "Click a row in the Category column to edit it. ",
                  "Changes are applied immediately to the downstream ",
                  "overhead summaries and projections."
                )
              )
            ),
            class = "d-flex align-items-center gap-2"
          ),
          card_body(
            # Date range filter + aggregation frequency
            layout_columns(
              col_widths = c(6, 3, 3),
              dateRangeInput(
                ns("date_filter"),
                "Filter by date range",
                start = date_lo,
                end = date_hi
              ),
              selectInput(
                ns("agg_frequency"),
                tooltip(
                  tagList(
                    "Forecast period",
                    bs_icon("info-circle", title = "Aggregation frequency")
                  ),
                  paste0(
                    "Choose how your data is grouped for forecasting. ",
                    "Both options work with as little as 3-6 periods of data, ",
                    "though projections are more accurate and show a narrower confidence interval ",
                    "as more history is available. ",
                    "Monthly is a good default for most practices. ",
                    "Weekly gives finer-grained projections and works best with at least ",
                    "6 months (26 weeks) of data; with more than a year (52+ weeks) ",
                    "seasonal patterns can also be captured."
                  )
                ),
                choices = c("Monthly" = "monthly", "Weekly" = "weekly"),
                selected = "monthly"
              ),
              tags$div(
                class = "d-flex align-items-end h-100 pb-1",
                actionButton(
                  ns("btn_clear_filter"),
                  "Clear",
                  icon = bs_icon("x-circle"),
                  class = "btn-outline-secondary btn-sm"
                )
              )
            ),
            # Two-tab view: Expenses (editable) and Income (view + remove)
            navset_tab(
              nav_panel(
                tagList(bs_icon("receipt"), " Expenses"),
                DT::dataTableOutput(ns("edit_table")),
                layout_columns(
                  col_widths = c(6, 6),
                  class = "mt-2",
                  actionButton(
                    ns("btn_apply_edits"),
                    "Apply Category Edits",
                    icon = icon("check"),
                    class = "btn-primary"
                  ),
                  actionButton(
                    ns("btn_remove_rows"),
                    "Remove Selected",
                    icon = bs_icon("trash"),
                    class = "btn-outline-danger"
                  )
                )
              ),
              nav_panel(
                tagList(bs_icon("cash-coin"), " Income"),
                DT::dataTableOutput(ns("income_table")),
                tags$div(
                  class = "mt-2",
                  actionButton(
                    ns("btn_remove_income_rows"),
                    "Remove Selected",
                    icon = bs_icon("trash"),
                    class = "btn-outline-danger"
                  )
                )
              )
            )
          )
        ),

        # -- Right: account re-map + add transactions -----------------------
        tagList(
          card(
            card_header(bs_icon("arrow-repeat"), " Bulk Re-map Account"),
            card_body(
              p(
                class = "text-muted small",
                "Re-assign every transaction for a given account to a new category."
              ),
              selectInput(
                ns("remap_account"),
                "Account",
                choices = NULL # populated in server
              ),
              selectInput(
                ns("remap_category"),
                "New Category",
                choices = valid_categories
              ),
              actionButton(
                ns("btn_remap"),
                "Re-map",
                icon = bs_icon("arrow-repeat"),
                class = "btn-secondary w-100"
              )
            )
          ),

          card(
            card_header(bs_icon("plus-circle"), " Add Transactions"),
            card_body(
              accordion(
                open = FALSE,
                # ---- Overhead -----------------------------------------------
                accordion_panel(
                  "Add overhead transaction",
                  icon = bs_icon("receipt"),
                  dateInput(ns("ovhd_date"), "Date"),
                  textInput(
                    ns("ovhd_account"),
                    "Account Name",
                    placeholder = "e.g. Rent"
                  ),
                  textInput(
                    ns("ovhd_account_full"),
                    "Full Account Name",
                    placeholder = "e.g. Expenses:Rent"
                  ),
                  textInput(ns("ovhd_description"), "Description"),
                  numericInput(
                    ns("ovhd_amount"),
                    "Amount ($)",
                    value = 0,
                    min = 0
                  ),
                  actionButton(
                    ns("btn_add_overhead"),
                    "Add Overhead Row",
                    icon = icon("plus"),
                    class = "btn-outline-primary w-100"
                  )
                ),
                # ---- Income -------------------------------------------------
                accordion_panel(
                  "Add income transaction",
                  icon = bs_icon("cash-coin"),
                  dateInput(ns("inc_date"), "Date"),
                  textInput(
                    ns("inc_account"),
                    "Account Name",
                    placeholder = "e.g. Membership Fees"
                  ),
                  textInput(
                    ns("inc_account_full"),
                    "Full Account Name",
                    placeholder = "e.g. Income:Membership Fees"
                  ),
                  selectInput(
                    ns("inc_source"),
                    "Income source",
                    choices = c(
                      "Membership fee" = "membership",
                      "Fee-for-service" = "fee_for_service",
                      "Other" = "other"
                    )
                  ),
                  textInput(ns("inc_description"), "Description"),
                  numericInput(
                    ns("inc_revenue"),
                    "Revenue ($)",
                    value = 0,
                    min = 0
                  ),
                  actionButton(
                    ns("btn_add_income"),
                    "Add Income Row",
                    icon = icon("plus"),
                    class = "btn-outline-success w-100"
                  )
                ),
                # ---- Period summary -----------------------------------------
                accordion_panel(
                  "Add period summary",
                  icon = bs_icon("calendar-week"),
                  tags$p(
                    class = "text-muted small mb-2",
                    "Enter totals for a whole month or week at once.",
                    "Rows with $0 are skipped on submit."
                  ),
                  radioButtons(
                    ns("summary_type"),
                    "Transaction type",
                    choices = c("Overhead" = "overhead", "Income" = "income"),
                    inline = TRUE
                  ),
                  radioButtons(
                    ns("summary_period_type"),
                    "Period",
                    choices = c("Monthly" = "monthly", "Weekly" = "weekly"),
                    inline = TRUE,
                    selected = "monthly"
                  ),
                  uiOutput(ns("summary_period_ui")),
                  DT::dataTableOutput(ns("summary_grid")),
                  actionButton(
                    ns("btn_add_summary"),
                    "Add to Data",
                    icon = icon("plus"),
                    class = "btn-primary w-100 mt-3"
                  )
                )
              )
            )
          )
        )
      )
    })

    # -- Active date range ----------------------------------------------------
    # Single source of truth for the filter bounds; falls back to the full
    # transaction range so that NULL / uninitialized inputs never produce NA.
    # Guard against empty r$transactions (0-row tibble from manual-entry path).
    active_date_range <- reactive({
      lo <- if (!is.null(input$date_filter) && !is.na(input$date_filter[1])) {
        input$date_filter[1]
      } else if (!is.null(r$transactions) && nrow(r$transactions) > 0) {
        min(r$transactions$date, na.rm = TRUE)
      } else {
        as.Date("1900-01-01")
      }

      hi <- if (!is.null(input$date_filter) && !is.na(input$date_filter[2])) {
        input$date_filter[2]
      } else if (!is.null(r$transactions) && nrow(r$transactions) > 0) {
        max(r$transactions$date, na.rm = TRUE)
      } else {
        as.Date("9999-12-31")
      }

      list(lo = lo, hi = hi)
    })

    # -- Recompute period summaries on filter, frequency, or data change -------
    # This is the single place that writes r$overhead_monthly and
    # r$income_monthly. It reacts to the date filter, the aggregation-frequency
    # selector, AND changes in r$overhead / r$income (triggered by upload,
    # edits, or manual additions).
    # NOTE: the fields are named *_monthly for legacy reasons; they hold either
    # monthly or weekly summaries depending on the chosen frequency.
    observe({
      req(r$overhead, r$income)
      # When both series are empty (manual-entry path before any data is added),
      # skip recomputation -- r$overhead_monthly will be populated directly by
      # the Quick Estimator generate handler.  If a real CSV is later uploaded
      # and r$overhead gains rows, this guard no longer fires and the normal
      # pipeline takes over.
      if (nrow(r$overhead) == 0 && nrow(r$income) == 0) {
        return()
      }
      dr <- active_date_range()
      freq <- input$agg_frequency %||% "monthly"

      ovhd_filtered <- dplyr::filter(r$overhead, date >= dr$lo, date <= dr$hi)
      inc_filtered <- dplyr::filter(r$income, date >= dr$lo, date <= dr$hi)

      if (freq == "weekly") {
        r$overhead_monthly <- directCareForecastR::summarize_overhead_weekly(
          ovhd_filtered
        )
        r$income_monthly <- directCareForecastR::summarize_income_weekly(
          inc_filtered
        )
      } else {
        r$overhead_monthly <- directCareForecastR::summarize_overhead_monthly(
          ovhd_filtered
        )
        r$income_monthly <- directCareForecastR::summarize_income_monthly(
          inc_filtered
        )
      }
    })

    # -- Populate account selector --------------------------------------------
    observe({
      req(r$transactions)
      accounts <- sort(unique(r$transactions$account_name))
      updateSelectInput(session, "remap_account", choices = accounts)
    })

    # -- Clear date filter back to full range ---------------------------------
    observeEvent(input$btn_clear_filter, {
      req(r$transactions)
      shiny::updateDateRangeInput(
        session,
        "date_filter",
        start = min(r$transactions$date, na.rm = TRUE),
        end = max(r$transactions$date, na.rm = TRUE)
      )
    })

    # -- Quick Estimator server logic ------------------------------------------

    # Live reactive totals used by the rendered summary rows in the form.
    ovhd_total_r <- reactive({
      sum(
        c(
          input$est_rent %||% 0,
          input$est_payroll %||% 0,
          input$est_ehr %||% 0,
          input$est_malpractice %||% 0,
          input$est_supplies %||% 0,
          input$est_other_overhead %||% 0
        ),
        na.rm = TRUE
      )
    })

    # .nn(): coerce a potentially-NULL or NA numeric input to 0 so that
    # downstream multiplication/addition never propagates NA into the UI.
    # %||% only catches NULL; a cleared numericInput sends NA, not NULL.
    .nn <- function(x) {
      v <- x %||% 0
      if (length(v) != 1L || !is.finite(v)) 0 else v
    }

    membership_income_r <- reactive({
      .nn(input$est_panel_size) * .nn(input$est_monthly_fee)
    })

    ffs_income_r <- reactive({
      .nn(input$est_new_patients_mo) *
        .nn(input$est_new_visit_fee) +
        .nn(input$est_followups_mo) * .nn(input$est_followup_fee) +
        .nn(input$est_other_income)
    })

    # Overhead total display (bottom of the overhead column)
    output$ovhd_total_ui <- renderUI({
      tot <- ovhd_total_r()
      tags$div(
        class = "border-top pt-2 mt-1 small",
        tags$span(class = "fw-semibold", "Monthly overhead total: "),
        tags$span(
          class = if (isTRUE(tot > 0)) {
            "text-secondary fw-semibold"
          } else {
            "text-muted"
          },
          fmt_dollar(tot)
        )
      )
    })

    # Revenue + surplus/deficit display (bottom of the revenue column)
    output$inc_total_ui <- renderUI({
      mem <- membership_income_r()
      ffs <- ffs_income_r()
      total <- mem + ffs
      surplus <- total - ovhd_total_r()
      pos <- isTRUE(surplus >= 0)

      tagList(
        tags$div(
          class = "border-top pt-2 mt-1 small",
          tags$div(
            tags$span(class = "fw-semibold", "Membership: "),
            fmt_dollar(mem)
          ),
          if (isTRUE(ffs > 0)) {
            tags$div(
              tags$span(class = "fw-semibold", "FFS: "),
              fmt_dollar(ffs)
            )
          },
          tags$div(
            class = "fw-semibold",
            tags$span("Monthly revenue total: "),
            tags$span(
              class = if (isTRUE(total > 0)) "text-primary" else "text-muted",
              fmt_dollar(total)
            )
          ),
          if (isTRUE(ovhd_total_r() > 0)) {
            tags$div(
              class = paste(
                "mt-1 small",
                if (pos) "text-success" else "text-warning"
              ),
              bs_icon(if (pos) "arrow-up-circle" else "arrow-down-circle"),
              " Estimated surplus / deficit: ",
              tags$strong(
                if (pos) {
                  paste0("+", fmt_dollar(surplus))
                } else {
                  fmt_dollar(surplus)
                }
              )
            )
          }
        )
      )
    })

    # Generate handler: build summary tibbles directly and store in r.
    # We bypass ingest_manual() / r$transactions so that estimated data is
    # kept separate from real accounting records.
    observeEvent(input$btn_generate, {
      req(r$practice_id)

      ovhd <- ovhd_total_r()
      if (ovhd <= 0) {
        showNotification(
          "Enter at least one overhead amount to generate a scenario.",
          type = "warning"
        )
        return()
      }

      n_months <- as.integer(input$est_n_months %||% 12L)
      start_mo <- as.integer(input$est_start_month %||% 1L)
      start_yr <- as.integer(
        input$est_start_year %||%
          (as.integer(format(Sys.Date(), "%Y")) - 1L)
      )
      start_date <- as.Date(paste(
        start_yr,
        sprintf("%02d", start_mo),
        "01",
        sep = "-"
      ))
      months_seq <- seq(start_date, by = "month", length.out = n_months)
      years <- as.integer(format(months_seq, "%Y"))
      months <- as.integer(format(months_seq, "%m"))

      r$overhead_monthly <- tibble::tibble(
        practice_id = r$practice_id,
        year = years,
        month = months,
        total_overhead = ovhd,
        gross_overhead = ovhd,
        total_refunds = 0
      )

      panel_now <- input$est_panel_size %||% 0
      fee <- input$est_monthly_fee %||% 0
      growth <- input$est_monthly_growth %||% 0
      ffs_mo <- ffs_income_r()
      panel_sizes <- pmax(0, panel_now + (seq_len(n_months) - 1L) * growth)

      r$income_monthly <- tibble::tibble(
        practice_id = r$practice_id,
        year = years,
        month = months,
        total_revenue = panel_sizes * fee + ffs_mo
      )

      # Pre-populate the Practice Profile so member-count value boxes work
      # immediately when the user runs a forecast.
      if (panel_now > 0) {
        r$panel_size <- panel_now
      }
      if (fee > 0) {
        r$membership_fee <- fee
      }

      # Store estimator inputs so the report module can include them.
      r$scenario_inputs <- list(
        start_period = format(start_date, "%B %Y"),
        n_months = n_months,
        panel_size = panel_now,
        monthly_fee = fee,
        monthly_growth = growth,
        ffs_new_visit_fee = input$est_new_visit_fee %||% 0,
        ffs_new_patients_mo = input$est_new_patients_mo %||% 0,
        ffs_followup_fee = input$est_followup_fee %||% 0,
        ffs_followups_mo = input$est_followups_mo %||% 0,
        ffs_other_income = input$est_other_income %||% 0,
        overhead_rent = input$est_rent %||% 0,
        overhead_payroll = input$est_payroll %||% 0,
        overhead_ehr = input$est_ehr %||% 0,
        overhead_malpractice = input$est_malpractice %||% 0,
        overhead_supplies = input$est_supplies %||% 0,
        overhead_other = input$est_other_overhead %||% 0,
        overhead_total = ovhd
      )

      estimator_done(TRUE)
      showNotification(
        paste0(
          n_months,
          " months generated for ",
          r$practice_name,
          ". ",
          "Head to the Projections tab to run your forecast."
        ),
        type = "message",
        duration = 5L
      )
    })

    # Restart: clear generated data and reset the estimator form.
    observeEvent(input$btn_restart_estimator, {
      estimator_done(FALSE)
      r$overhead_monthly <- NULL
      r$income_monthly <- NULL
      r$panel_size <- NULL
      r$membership_fee <- NULL
      r$scenario_inputs <- NULL
    })

    # Navigate to the Projections tab via the parent (app-level) session.
    observeEvent(input$btn_go_projections, {
      nav_target <- parent_session %||% session
      updateNavbarPage(
        nav_target,
        inputId = "main_nav",
        selected = "projections"
      )
    })

    # Navigation buttons shown in the manual aggregate-entry message card
    observeEvent(input$btn_manual_to_summary, {
      updateNavbarPage(
        parent_session %||% session,
        "main_nav",
        selected = "summary"
      )
    })
    observeEvent(input$btn_manual_to_projections, {
      updateNavbarPage(
        parent_session %||% session,
        "main_nav",
        selected = "projections"
      )
    })

    # -- End of Quick Estimator server logic -----------------------------------

    # -- Filtered view of transactions (table display only) -------------------
    filtered_transactions <- reactive({
      req(r$transactions)
      dr <- active_date_range()
      dplyr::filter(r$transactions, date >= dr$lo, date <= dr$hi)
    })

    # Row positions of filtered transactions within r$transactions.
    # Used by cell-edit and remove handlers to translate DT row numbers
    # (relative to the visible subset) back to full-frame positions.
    filtered_row_indices <- reactive({
      req(r$transactions)
      dr <- active_date_range()
      which(r$transactions$date >= dr$lo & r$transactions$date <= dr$hi)
    })

    # -- Editable expense transaction table -----------------------------------
    output$edit_table <- DT::renderDataTable({
      req(r$transactions)

      filtered_transactions() |>
        dplyr::select(date, account_name, description, amount, category) |>
        DT::datatable(
          rownames = FALSE,
          colnames = c("Date", "Account", "Description", "Amount", "Category"),
          editable = list(
            target = "cell",
            disable = list(columns = c(0, 1, 2, 3))
          ),
          options = list(pageLength = 15, dom = "ftp", scrollX = TRUE),
          selection = list(mode = "multiple", target = "row")
        )
    })

    # -- Income transaction table (read-only, remove-only) --------------------
    # Row positions of visible income rows mapped back to r$income.
    filtered_income_row_indices <- reactive({
      req(r$income)
      dr <- active_date_range()
      which(r$income$date >= dr$lo & r$income$date <= dr$hi)
    })

    output$income_table <- DT::renderDataTable({
      req(r$income)

      dr <- active_date_range()
      dplyr::filter(r$income, date >= dr$lo, date <= dr$hi) |>
        dplyr::select(date, account_name, description, revenue, source) |>
        DT::datatable(
          rownames = FALSE,
          colnames = c(
            "Date",
            "Account",
            "Description",
            "Revenue ($)",
            "Source"
          ),
          options = list(pageLength = 15, dom = "ftp", scrollX = TRUE),
          selection = list(mode = "multiple", target = "row")
        )
    })

    # -- Remove selected income rows ------------------------------------------
    observeEvent(input$btn_remove_income_rows, {
      sel <- input$income_table_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        showNotification(
          "Select rows in the Income table first.",
          type = "warning"
        )
        return()
      }

      true_rows <- filtered_income_row_indices()[sel]
      n <- length(true_rows)

      # Removing from r$income triggers the central observe -> monthly summaries
      r$income <- r$income[-true_rows, ]

      showNotification(
        paste0("Removed ", n, " income row", if (n != 1L) "s" else "", "."),
        type = "message",
        duration = 3L
      )
    })

    # -- Apply inline cell edits ----------------------------------------------
    observeEvent(input$edit_table_cell_edit, {
      info <- input$edit_table_cell_edit
      # Column 4 = category (0-indexed in DT)
      if (info$col == 4) {
        new_val <- trimws(as.character(info$value))
        if (!new_val %in% valid_categories) {
          showNotification(
            paste0(
              "'",
              new_val,
              "' is not a valid category. ",
              "Choose one of: ",
              paste(valid_categories, collapse = ", ")
            ),
            type = "warning"
          )
          return()
        }

        true_row <- filtered_row_indices()[info$row]
        r$transactions$category[true_row] <- new_val
        # r$overhead update triggers the central observe -> monthly summaries
        r$overhead <- directCareForecastR::filter_gnucash_overhead(
          r$transactions
        )
      }
    })

    # -- Remove selected rows -------------------------------------------------
    observeEvent(input$btn_remove_rows, {
      sel <- input$edit_table_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        showNotification("Select rows in the table first.", type = "warning")
        return()
      }

      true_rows <- filtered_row_indices()[sel]
      n <- length(true_rows)

      r$transactions <- r$transactions[-true_rows, ]
      # Both updates below trigger the central observe -> monthly summaries
      r$overhead <- directCareForecastR::filter_gnucash_overhead(r$transactions)
      r$income <- suppressWarnings(
        directCareForecastR::normalize_gnucash_income(r$transactions)
      )

      showNotification(
        paste0("Removed ", n, " row", if (n != 1) "s" else "", "."),
        type = "message",
        duration = 3
      )
    })

    # -- Bulk account re-map --------------------------------------------------
    observeEvent(input$btn_remap, {
      req(input$remap_account, input$remap_category, r$transactions)

      mask <- r$transactions$account_name == input$remap_account
      n <- sum(mask)
      r$transactions$category[mask] <- input$remap_category

      # r$overhead update triggers the central observe -> monthly summaries
      r$overhead <- directCareForecastR::filter_gnucash_overhead(r$transactions)

      showNotification(
        paste0(
          "Re-mapped ",
          n,
          " rows of '",
          input$remap_account,
          "' -> ",
          input$remap_category
        ),
        type = "message",
        duration = 3
      )
    })

    # -- Add manual overhead row ----------------------------------------------
    observeEvent(input$btn_add_overhead, {
      req(r$practice_id)

      new_df <- data.frame(
        date = input$ovhd_date,
        full_account_name = input$ovhd_account_full,
        account_name = input$ovhd_account,
        description = input$ovhd_description,
        amount = input$ovhd_amount
      )

      new_rows <- quietly(
        directCareForecastR::ingest_manual(
          new_df,
          r$practice_id,
          type = "overhead"
        )
      )

      if (!is.null(new_rows)) {
        r$transactions <- dplyr::bind_rows(r$transactions, new_rows)
        # r$overhead update triggers the central observe -> monthly summaries
        r$overhead <- directCareForecastR::filter_gnucash_overhead(
          r$transactions
        )
        showNotification("Overhead row added.", type = "message", duration = 2)
      }
    })

    # -- Add manual income row ------------------------------------------------
    observeEvent(input$btn_add_income, {
      req(r$practice_id)

      new_df <- data.frame(
        date = input$inc_date,
        full_account_name = input$inc_account_full,
        account_name = input$inc_account,
        description = input$inc_description,
        revenue = input$inc_revenue,
        source = input$inc_source
      )

      new_rows <- quietly(
        directCareForecastR::ingest_manual(
          new_df,
          r$practice_id,
          type = "income"
        )
      )

      if (!is.null(new_rows)) {
        # r$income update triggers the central observe -> monthly summaries
        r$income <- dplyr::bind_rows(r$income, new_rows)
        showNotification("Income row added.", type = "message", duration = 2)
      }
    })

    #  Period summary panel

    # Grid data: starts as the overhead template; resets when type radio changes
    summary_grid_data <- shiny::reactiveVal(.ovhd_grid())

    observeEvent(input$summary_type, {
      if (identical(input$summary_type, "income")) {
        summary_grid_data(.inc_grid())
      } else {
        summary_grid_data(.ovhd_grid())
      }
    })

    # Period selector UI (Monthly selects vs. Weekly date input)
    output$summary_period_ui <- renderUI({
      if (isTRUE(input$summary_period_type == "weekly")) {
        tagList(
          dateInput(
            ns("summary_week_date"),
            "Any date in the week",
            value = Sys.Date()
          ),
          tags$p(
            class = "text-muted small",
            "Entry will be dated to the Monday of the selected week."
          )
        )
      } else {
        layout_columns(
          col_widths = c(7, 5),
          selectInput(
            ns("summary_month"),
            "Month",
            choices = stats::setNames(seq_len(12L), month.name),
            selected = as.integer(format(Sys.Date(), "%m"))
          ),
          numericInput(
            ns("summary_year"),
            "Year",
            value = as.integer(format(Sys.Date(), "%Y")),
            min = 2000L,
            max = 2100L,
            step = 1L
          )
        )
      }
    })

    # Editable grid: only the Amount column is editable (column index 1, 0-based)
    output$summary_grid <- DT::renderDataTable({
      req(summary_grid_data())
      is_ovhd <- !identical(input$summary_type, "income")
      col1_lbl <- if (is_ovhd) "Category" else "Source"

      DT::datatable(
        summary_grid_data(),
        rownames = FALSE,
        colnames = c(col1_lbl, "Amount ($)"),
        editable = list(target = "cell", disable = list(columns = 0L)),
        options = list(dom = "t", pageLength = 15L, ordering = FALSE),
        class = "compact stripe"
      )
    })

    # Propagate cell edits back into the reactiveVal
    observeEvent(input$summary_grid_cell_edit, {
      info <- input$summary_grid_cell_edit
      if (isTRUE(info$col == 1L)) {
        d <- summary_grid_data()
        d[info$row, 2L] <- as.numeric(info$value)
        summary_grid_data(d)
      }
    })

    # Submit: build ingest_manual() calls for every non-zero row
    observeEvent(input$btn_add_summary, {
      req(r$practice_id, summary_grid_data())

      d <- summary_grid_data()
      nonzero <- d[d[[2L]] > 0, , drop = FALSE]

      if (nrow(nonzero) == 0L) {
        showNotification(
          "Enter at least one non-zero amount.",
          type = "warning"
        )
        return()
      }

      # Resolve period date ------------------------------------------------
      period_date <- if (isTRUE(input$summary_period_type == "weekly")) {
        req(input$summary_week_date)
        raw <- as.Date(input$summary_week_date)
        raw - as.integer(format(raw, "%u")) + 1L # Monday of the week
      } else {
        req(input$summary_month, input$summary_year)
        as.Date(paste(
          as.integer(input$summary_year),
          sprintf("%02d", as.integer(input$summary_month)),
          "01",
          sep = "-"
        ))
      }

      period_label <- if (isTRUE(input$summary_period_type == "weekly")) {
        paste("week of", format(period_date, "%b %d, %Y"))
      } else {
        format(period_date, "%B %Y")
      }

      is_overhead <- !identical(input$summary_type, "income")

      all_rows <- lapply(seq_len(nrow(nonzero)), function(i) {
        label <- nonzero[[1L]][i]
        amount <- nonzero[[2L]][i]
        desc <- paste0("Period summary - ", label, " - ", period_label)

        if (is_overhead) {
          info <- .ovhd_acct[[label]]
          df <- data.frame(
            date = period_date,
            full_account_name = info$full,
            account_name = info$acct,
            description = desc,
            amount = amount
          )
          quietly(directCareForecastR::ingest_manual(
            df,
            r$practice_id,
            type = "overhead"
          ))
        } else {
          info <- .inc_acct[[label]]
          df <- data.frame(
            date = period_date,
            full_account_name = info$full,
            account_name = info$acct,
            description = desc,
            revenue = amount,
            source = info$src
          )
          quietly(directCareForecastR::ingest_manual(
            df,
            r$practice_id,
            type = "income"
          ))
        }
      })

      valid_rows <- Filter(Negate(is.null), all_rows)

      if (length(valid_rows) > 0L) {
        batch <- dplyr::bind_rows(valid_rows)

        if (is_overhead) {
          r$transactions <- dplyr::bind_rows(r$transactions, batch)
          r$overhead <- directCareForecastR::filter_gnucash_overhead(
            r$transactions
          )
        } else {
          r$income <- dplyr::bind_rows(r$income, batch)
        }

        # Reset amounts to 0
        g <- summary_grid_data()
        g[[2L]] <- rep(0, nrow(g))
        summary_grid_data(g)

        n_word <- if (length(valid_rows) == 1L) "entry" else "entries"
        showNotification(
          paste0(
            "Added ",
            length(valid_rows),
            " ",
            if (is_overhead) "overhead" else "income",
            " ",
            n_word,
            " for ",
            period_label,
            "."
          ),
          type = "message",
          duration = 3L
        )
      }
    })
  })
}
