#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @import shiny
#' @importFrom thematic thematic_shiny
#' @noRd
app_server <- function(input, output, session) {
  thematic::thematic_shiny()

  # -- Shared reactive state --------------------------------------------------
  # All three modules read and write through `r`. Tab 1 populates the data;
  # Tab 2 may modify categories or append manual rows; Tab 3 consumes the
  # final result for forecasting and report generation.
  r <- reactiveValues(
    # Practice metadata
    practice_id = NULL, # character \u2014 future: sourced from auth session
    practice_name = NULL, # character \u2014 display label for reports
    panel_size = NULL, # numeric \u2014 current DPC panel size (members)
    membership_fee = NULL, # numeric \u2014 monthly membership fee per member ($)

    # Data pipeline state
    transactions = NULL, # normalized tibble from ingest_gnucash_csv()
    overhead = NULL, # filter_gnucash_overhead() output
    income = NULL, # normalize_gnucash_income() output
    overhead_monthly = NULL, # summarize_overhead_monthly() output
    income_monthly = NULL, # summarize_income_monthly() output
    scenario_inputs = NULL, # list of Quick Estimator form values

    # Validation flags surfaced from validate_overhead() / validate_income()
    validation = list()
  )

  # -- Brand-click: return to Upload tab (navigation only, no data reset) -------
  observeEvent(
    input$brand_click,
    {
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

        # Workflow overview
        tags$h6(class = "fw-bold mt-2", "Getting started"),
        tags$p(
          "Enter your practice name and ID on the ",
          tags$strong("Upload"),
          " tab,",
          " then choose one of three workflows:"
        ),
        tags$div(
          class = "row g-3 mb-3",
          tags$div(
            class = "col-md-4",
            tags$div(
              class = "card h-100 border-primary",
              tags$div(
                class = "card-body",
                tags$h6(
                  class = "card-title",
                  bsicons::bs_icon("file-earmark-bar-graph"),
                  " Upload Real Data"
                ),
                tags$p(
                  class = "card-text small",
                  "Export a CSV from GnuCash (File \u2192 Export \u2192 Export",
                  " Transactions to CSV) and upload it. The app maps your accounts to",
                  " expense categories and builds monthly summaries automatically."
                )
              )
            )
          ),
          tags$div(
            class = "col-md-4",
            tags$div(
              class = "card h-100 border-secondary",
              tags$div(
                class = "card-body",
                tags$h6(
                  class = "card-title",
                  bsicons::bs_icon("table"),
                  " Enter Data Manually"
                ),
                tags$p(
                  class = "card-text small",
                  "Type in aggregate overhead and income totals period by period",
                  " (monthly or weekly). Good for practices with records in",
                  " spreadsheets or other software not yet supported."
                )
              )
            )
          ),
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
                  "Enter estimated monthly overhead and membership details to",
                  " generate a synthetic financial history. Ideal for practices",
                  " in the planning stage. Forecasts reflect the end of the",
                  " synthetic period, not your starting values."
                )
              )
            )
          )
        ),

        tags$hr(),

        # Tab guide
        tags$h6(class = "fw-bold", "Tab guide"),
        tags$dl(
          class = "row small",
          tags$dt(class = "col-sm-3", bsicons::bs_icon("upload"), " Upload"),
          tags$dd(
            class = "col-sm-9",
            "Enter practice details and load data via any of the three workflows above."
          ),

          tags$dt(
            class = "col-sm-3",
            bsicons::bs_icon("pencil-square"),
            " Review & Edit"
          ),
          tags$dd(
            class = "col-sm-9",
            "Correct account category mappings and add or remove individual",
            " transactions. Only applicable after a CSV upload or when using",
            " the Quick Estimator (Plan My Practice)."
          ),

          tags$dt(
            class = "col-sm-3",
            bsicons::bs_icon("bar-chart-line"),
            " Summary"
          ),
          tags$dd(
            class = "col-sm-9",
            "Overview of overhead and revenue trends by period.",
            " CSV-upload users also see a breakdown by expense category and",
            " income source."
          ),

          tags$dt(
            class = "col-sm-3",
            bsicons::bs_icon("graph-up-arrow"),
            " Projections"
          ),
          tags$dd(
            class = "col-sm-9",
            "Run break-even, revenue, and income-target forecasts. Adjust",
            " the method, horizon, confidence level, and growth assumptions",
            " in the sidebar. Enter your panel size and monthly fee to unlock",
            " member-count metrics. Download a PDF report when ready."
          )
        ),

        tags$hr(),

        tags$p(
          class = "small text-muted mb-0",
          bsicons::bs_icon("envelope"),
          " Questions or feedback? Contact ",
          tags$a(
            href = "mailto:anthony@raborndecisionsciences.com",
            "anthony@raborndecisionsciences.com"
          ),
          "."
        )
      ))
    },
    ignoreInit = TRUE
  )

  # -- Module wiring ----------------------------------------------------------
  mod_upload_server("upload", r, parent_session = session)
  mod_edit_server("edit", r, parent_session = session)
  mod_summary_server("summary", r)
  mod_projections_server("projections", r)
}
