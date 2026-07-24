#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @param res_auth The `reactiveValues` returned by
#'   `shinymanager::secure_server()`, holding the logged-in practice's
#'   `user_info` (practice_id, practice_name, email). `NULL` outside of
#'   `run_app()` (e.g. in tests), in which case the account menu renders
#'   without a practice name.
#' @import shiny
#' @importFrom thematic thematic_shiny
#' @noRd
app_server <- function(input, output, session, res_auth = NULL) {
  # -- Account menu: practice name + logout, top-right of the navbar --------
  # id = ".shinymanager_logout" is not a namespacing choice -- it's the
  # exact input id shinymanager::secure_server() listens for internally
  # (see fab_button()'s usage inside shinymanager::secure_app()). Reusing it
  # here lets this link trigger the same logout logic as the default
  # floating button, which run_app() disables via fab_position = "none".
  output$account_menu <- renderUI({
    practice_name <- res_auth$practice_name
    tags$span(
      class = "d-flex align-items-center gap-2",
      if (!is.null(practice_name) && nzchar(practice_name)) {
        tags$span(class = "text-light small", practice_name)
      },
      tags$a(
        id = ".shinymanager_logout",
        href = "#",
        class = "nav-link action-button",
        title = "Logout",
        bs_icon("box-arrow-right", title = "Logout")
      )
    )
  })
  # Shiny suspends renderUI evaluation for outputs it judges "hidden" by
  # default (suspendWhenHidden = TRUE) -- the navbar's <li class="... nav-item
  # form-inline"> wrapper reads as zero-size to that heuristic, so the
  # reactive never even ran (confirmed via Shiny.shinyapp.$values/$errors:
  # neither a value nor an error was ever recorded for this output).
  outputOptions(output, "account_menu", suspendWhenHidden = FALSE)

  # Pinned rather than "auto": auto-detection calls back into
  # shiny::getCurrentOutputInfo() to resolve bg/fg/accent from the
  # surrounding CSS. That resolution errored ("attempt to apply
  # non-function") for bg/fg on plotOutput()s inside full_screen bslib
  # cards (as ovhd_plot/inc_plot in mod_summary.R are). Pinning colors
  # avoids using any "auto" value, but thematic::auto_resolve_theme() still
  # unconditionally probes shiny::getCurrentOutputInfo() on every plot
  # render to see whether it *could* auto-resolve something — and warns
  # when that output context lacks CSS-reporting info, even though the
  # result goes unused since nothing here is actually "auto". Dropping
  # "shiny" from the auto-config priority list skips that probe entirely.
  thematic::auto_config_set(
    thematic::auto_config(priority = c("config", "bslib", "rstudio"))
  )
  thematic::thematic_shiny(
    bg = "#F8FAFC",
    fg = "#172033",
    accent = "#14B8A6"
  )

  # -- Shared reactive state --------------------------------------------------
  # All three modules read and write through `r`. Tab 1 populates the data;
  # Tab 2 may modify categories or append manual rows; Tab 3 consumes the
  # final result for forecasting and report generation.
  r <- reactiveValues(
    # Practice metadata
    practice_id = NULL, # character \u2014 future: sourced from auth session
    practice_name = NULL, # character \u2014 display label for reports
    panel_size = NULL, # numeric \u2014 current DPC panel size (members)
    membership_fee = NULL, # numeric \u2014 weighted avg monthly membership fee per member ($)
    membership_tiers = NULL, # list of {label, members, fee} \u2014 detail behind panel_size/membership_fee
    category_labels = NULL, # named chr vector: overhead category slug \u2192 custom display label
    source_labels = NULL, # named chr vector: income account_name \u2192 custom display label

    # Data pipeline state
    transactions = NULL, # normalized tibble from ingest_gnucash_csv()
    overhead = NULL, # filter_gnucash_overhead() output
    income = NULL, # normalize_gnucash_income() output
    overhead_monthly = NULL, # summarize_overhead_monthly() output
    income_monthly = NULL, # summarize_income_monthly() output
    scenario_inputs = NULL, # list of Quick Estimator form values

    # Validation flags surfaced from validate_overhead() / validate_income()
    validation = list(),

    # Incremented on every confirmed Start Over; modules observe this to
    # clear their own local reactive state (selected path, tier counts, etc).
    reset_signal = 0L
  )

  # -- Brand-click: return to Upload tab (navigation only, no data reset) -------
  observeEvent(
    input$brand_click,
    {
      updateNavbarPage(session, inputId = "main_nav", selected = "upload")
    },
    ignoreInit = TRUE
  )

  # -- Global Start Over: available from every page via the navbar icon --------
  observeEvent(
    input$global_start_over_click,
    {
      showModal(modalDialog(
        title = tagList(bs_icon("exclamation-triangle-fill"), " Start Over?"),
        p(
          "This will clear all loaded data and return you to the workflow selection screen."
        ),
        p(
          tags$strong("Your practice name and ID will be kept,"),
          " but all uploaded or generated financial data will be removed."
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            "global_confirm_start_over",
            "Yes, start over",
            class = "btn-warning"
          )
        ),
        easyClose = TRUE
      ))
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$global_confirm_start_over,
    {
      removeModal()
      r$panel_size <- NULL
      r$membership_fee <- NULL
      r$membership_tiers <- NULL
      r$category_labels <- NULL
      r$source_labels <- NULL
      r$transactions <- NULL
      r$overhead <- NULL
      r$income <- NULL
      r$overhead_monthly <- NULL
      r$income_monthly <- NULL
      r$scenario_inputs <- NULL
      r$validation <- list()
      r$reset_signal <- r$reset_signal + 1L
      updateNavbarPage(session, inputId = "main_nav", selected = "upload")
    },
    ignoreInit = TRUE
  )

  # -- Help modal ---------------------------------------------------------------
  observeEvent(
    input$help_click,
    {
      showModal(modalDialog(
        title = tagList(
          bsicons::bs_icon("question-circle"),
          " Direct Care Analytics \u2014 Help"
        ),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),

        # Workflow overview cards
        tags$h6(class = "fw-bold mt-2", "Choose a workflow"),
        tags$p(
          class = "small text-muted mb-3",
          "Enter your practice name and ID at the top of the page, then pick",
          " the workflow that fits your situation."
        ),
        tags$div(
          class = "row g-3 mb-3",

          # -- Workflow 1: Historical data --------------------------------------
          tags$div(
            class = "col-md-4",
            tags$div(
              class = "card h-100 border-primary",
              tags$div(
                class = "card-body",
                tags$h6(
                  class = "card-title",
                  bsicons::bs_icon("file-earmark-bar-graph"),
                  " Historical Data"
                ),
                tags$p(
                  class = "card-text small mb-2",
                  "For practices with existing financial records. Load your data",
                  " one of two ways:"
                ),
                tags$ul(
                  class = "small ps-3 mb-0",
                  tags$li(
                    tags$strong("CSV upload \u2014"),
                    " export transactions from GnuCash",
                    " (File \u2192 Export \u2192 Export Transactions to CSV).",
                    " The app maps accounts to expense categories automatically."
                  ),
                  tags$li(
                    tags$strong("Manual entry \u2014"),
                    " type in aggregate overhead and income totals",
                    " period by period. Good for spreadsheet-based records."
                  )
                ),
                tags$p(
                  class = "card-text small mt-2 mb-0 text-muted",
                  bsicons::bs_icon("arrow-right-short"),
                  " Review & Edit \u2192 Summary \u2192 Projections"
                )
              )
            )
          ),

          # -- Workflow 2: Plan My Practice ------------------------------------
          tags$div(
            class = "col-md-4",
            tags$div(
              class = "card h-100 border-info",
              tags$div(
                class = "card-body",
                tags$h6(
                  class = "card-title",
                  bsicons::bs_icon("sliders"),
                  " Plan My Practice"
                ),
                tags$p(
                  class = "card-text small",
                  "For practices in the planning stage or without accounting",
                  " software. Enter estimated monthly overhead, membership tiers",
                  " (with optional per-tier growth rates), and optional",
                  " fee-for-service income. The app builds a synthetic financial",
                  " history and runs forecasts from the end of that period."
                ),
                tags$p(
                  class = "card-text small mt-2 mb-0 text-muted",
                  bsicons::bs_icon("arrow-right-short"),
                  " Review Scenario \u2192 Projections"
                )
              )
            )
          ),

          # -- Workflow 3: Quick Calculator ------------------------------------
          tags$div(
            class = "col-md-4",
            tags$div(
              class = "card h-100 border-secondary",
              tags$div(
                class = "card-body",
                tags$h6(
                  class = "card-title",
                  bsicons::bs_icon("calculator"),
                  " Quick Calculator"
                ),
                tags$p(
                  class = "card-text small",
                  "Instant break-even and income-target math from your current",
                  " overhead and membership panel. Supports multiple membership",
                  " tiers. No historical data or forecast model \u2014 results",
                  " update as you type."
                ),
                tags$p(
                  class = "card-text small mt-2 mb-0 text-muted",
                  bsicons::bs_icon("arrow-right-short"),
                  " Results shown immediately, no navigation required"
                )
              )
            )
          )
        ),

        tags$hr(),

        # Workflow walkthroughs
        tags$h6(class = "fw-bold", "Step-by-step walkthroughs"),

        # Historical data walkthrough
        tags$p(
          class = "small fw-semibold mb-1",
          bsicons::bs_icon("file-earmark-bar-graph"),
          " Historical Data"
        ),
        tags$ol(
          class = "small mb-3",
          tags$li(
            tags$strong("Upload tab \u2014"),
            " enter your practice name and ID, then either upload a GnuCash CSV",
            " or click \u201cEnter Data Manually\u201d to type in period totals."
          ),
          tags$li(
            tags$strong("Review & Edit \u2014"),
            " for CSV uploads, verify that transactions are mapped to the correct",
            " expense categories and remove any erroneous rows. For manual entry,",
            " this step is skipped."
          ),
          tags$li(
            tags$strong("Summary \u2014"),
            " review overhead and revenue trends by period, with a category",
            " and income-source breakdown for CSV uploads."
          ),
          tags$li(
            tags$strong("Projections \u2014"),
            " choose a forecast method (Linear, ETS, or ARIMA), set the horizon",
            " and confidence level, and optionally add planned overhead events.",
            " Enter your membership tiers to unlock member-count metrics.",
            " Download the PDF report when ready."
          )
        ),

        # Plan My Practice walkthrough
        tags$p(
          class = "small fw-semibold mb-1",
          bsicons::bs_icon("sliders"),
          " Plan My Practice"
        ),
        tags$ol(
          class = "small mb-3",
          tags$li(
            tags$strong("Upload tab \u2014"),
            " enter your practice name and ID, then click \u201cPlan My Practice\u201d."
          ),
          tags$li(
            tags$strong("Quick Estimator \u2014"),
            " fill in monthly overhead by category, define one or more membership",
            " tiers (label, starting members, fee, and growth per month), and",
            " optionally add fee-for-service income.",
            " Set the synthetic history length and click \u201cGenerate Practice Scenario\u201d."
          ),
          tags$li(
            tags$strong("Review Scenario \u2014"),
            " inspect the generated summary, then click \u201cGo to Projections\u201d.",
            " Use \u201cRevise Estimates\u201d to regenerate with different inputs."
          ),
          tags$li(
            tags$strong("Projections \u2014"),
            " same as the Historical Data workflow. Membership tiers are",
            " pre-filled from the end of your synthetic period."
          )
        ),

        # Quick Calculator walkthrough
        tags$p(
          class = "small fw-semibold mb-1",
          bsicons::bs_icon("calculator"),
          " Quick Calculator"
        ),
        tags$ol(
          class = "small mb-3",
          tags$li(
            tags$strong("Upload tab \u2014"),
            " enter your practice name and ID, then click \u201cQuick Calculator\u201d."
          ),
          tags$li(
            tags$strong("Calculator \u2014"),
            " enter total monthly overhead, add membership tiers, and set an",
            " income target. Results (break-even and target scenarios) update",
            " instantly as you type."
          )
        ),

        tags$hr(),

        tags$p(
          class = "small text-muted mb-0",
          bsicons::bs_icon("envelope"),
          " Questions or feedback? Contact ",
          tags$a(
            href = "mailto:anthony@raborndecisionsciences.com",
            "anthony@raborndecisionsciences.com."
          )
        )
      ))
    },
    ignoreInit = TRUE
  )

  # -- Module wiring ----------------------------------------------------------
  mod_upload_server("upload", r, parent_session = session)
  mod_edit_server("edit", r, parent_session = session)
  mod_summary_server("summary", r, parent_session = session)
  mod_projections_server("projections", r, parent_session = session)
}
