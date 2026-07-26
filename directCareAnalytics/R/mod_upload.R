#' Upload Module UI
#'
#' Tab 1 -- Path choice (upload real bookkeeping data or open the practice
#' planning estimator). Practice identity is sourced from the logged-in
#' account (see app_server.R) rather than re-entered here.
#'
#' @param id Module namespace ID.
#' @noRd
mod_upload_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Plain, non-editable label -- practice_name is authoritative from
    # res_auth now (set once in app_server.R), not a field on this page.
    uiOutput(ns("practice_name_display")),

    # -- Path choice or upload flow (dynamic) --------------------------------
    uiOutput(ns("main_content")),

    # -- Next button (shown once data is loaded via the upload path) ----------
    uiOutput(ns("next_btn")),

    # -- Branded footer -------------------------------------------------------
    tags$div(
      class = "mt-5 pt-3 border-top d-flex align-items-center justify-content-center gap-3",
      style = "opacity:0.55;",
      tags$img(
        src = "www/logo-rds-alt.svg",
        height = "36px",
        alt = "Raborn Decision Sciences"
      )
    )
  )
}


#' Upload Module Server
#'
#' @param id Module namespace ID.
#' @param r Shared `reactiveValues` object from `app_server`.
#' @param parent_session The top-level Shiny session for cross-tab navigation.
#' @noRd
mod_upload_server <- function(id, r, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Which path the user has chosen: NULL | "upload" | "manual"
    path_chosen <- reactiveVal(NULL)

    # Track which generic-CSV files have been successfully loaded.
    loaded <- reactiveValues(overhead = FALSE, income = FALSE)

    # -- Global Start Over: clear this module's local UI state -------------
    # The actual data reset happens once in app_server; this just keeps the
    # path-selection UI in sync so it reverts to the initial choice screen.
    observeEvent(
      r$reset_signal,
      {
        path_chosen(NULL)
        loaded$overhead <- FALSE
        loaded$income <- FALSE
      },
      ignoreInit = TRUE
    )

    # Plain, non-editable display of the account's practice name -- see
    # mod_upload_ui()'s own comment for why this replaced the old in-app
    # Practice Name/ID re-entry card.
    output$practice_name_display <- renderUI({
      req(r$practice_name)
      tags$p(
        class = "text-muted mb-3",
        bs_icon("building"), " ", r$practice_name
      )
    })

    # -- Main content area --------------------------------------------------
    output$main_content <- renderUI({
      path <- path_chosen()

      if (is.null(path)) {
        # -- Path selection cards ------------------------------------------
        layout_columns(
          col_widths = c(4, 4, 4),
          card(
            class = "h-100",
            card_header(
              tagList(bs_icon("file-earmark-bar-graph"), " Use Real Data")
            ),
            card_body(
              tags$p(
                "Use your income and overhead data to analyse actual revenue ",
                "and expenses. Best for practices that already use ",
                "accounting software or maintain transaction records."
              ),
              tags$ul(
                class = "text-muted small mb-3",
                tags$li("Historical overhead breakdown"),
                tags$li("Actual revenue trends"),
                tags$li("Data-driven break-even forecast")
              ),
              actionButton(
                ns("btn_use_real"),
                "Use Bookkeeping Data",
                icon = bs_icon("upload"),
                class = "btn-primary w-100"
              )
            )
          ),
          card(
            class = "h-100",
            card_header(
              tagList(bs_icon("sliders"), " Plan My Practice")
            ),
            card_body(
              tags$p(
                "Enter estimated overhead costs and revenue targets to ",
                "explore financial scenarios. Ideal for practices in the ",
                "planning stage or without bookkeeping software."
              ),
              tags$ul(
                class = "text-muted small mb-3",
                tags$li("Estimate monthly overhead categories"),
                tags$li("Set membership fee and panel-size goals"),
                tags$li("Generate a synthetic financial scenario")
              ),
              actionButton(
                ns("btn_use_plan"),
                "Start Planning",
                icon = bs_icon("pencil"),
                class = "btn-outline-primary w-100"
              )
            )
          ),
          card(
            class = "h-100",
            card_header(
              tagList(bs_icon("calculator"), " Quick Calculator")
            ),
            card_body(
              tags$p(
                "Instantly see how much revenue your panel generates, where ",
                "you stand relative to overhead, and what it takes to hit your ",
                "income goal. No historical data needed."
              ),
              tags$ul(
                class = "text-muted small mb-3",
                tags$li("Enter overhead and membership details"),
                tags$li("Set an income target"),
                tags$li("See break-even and target scenarios instantly")
              ),
              actionButton(
                ns("btn_use_calculator"),
                "Open Calculator",
                icon = bs_icon("calculator"),
                class = "btn-outline-secondary w-100"
              )
            )
          )
        )
      } else if (path == "calculator") {
        mod_calculator_ui(ns("calculator"))
      } else if (path == "manual") {
        mod_manual_entry_ui(ns("manual"))
      } else if (path == "upload") {
        layout_columns(
          col_widths = c(3, 9),
          # Left: upload controls
          card(
            card_header(
              bs_icon("file-earmark-spreadsheet"),
              " Data Upload"
            ),
            card_body(
              radioButtons(
                ns("software"),
                label = tags$span(
                  class = "fw-semibold",
                  "Bookkeeping software"
                ),
                choiceNames = list(
                  "GnuCash",
                  "Other / Generic CSV",
                  tagList(
                    "QuickBooks Online ",
                    tags$span(
                      class = "badge text-bg-warning small ms-1",
                      "experimental"
                    )
                  ),
                  tagList(
                    "Wave ",
                    tags$span(
                      class = "badge text-bg-secondary small ms-1",
                      "coming soon"
                    )
                  )
                ),
                choiceValues = list("gnucash", "other", "quickbooks", "wave"),
                selected = "gnucash"
              ),
              tags$script(HTML(paste0(
                "$(document).ready(function() {",
                "  $('input[type=radio][value=wave]').prop('disabled', true);",
                "});"
              ))),
              hr(),
              uiOutput(ns("upload_controls")),
              hr(),
              actionButton(
                ns("btn_back"),
                "Back",
                icon = bs_icon("arrow-left"),
                class = "btn-outline-secondary btn-sm w-100"
              )
            )
          ),
          # Right: results
          uiOutput(ns("upload_results"))
        )
      }
    })

    # -- Dynamic upload controls (left panel) --------------------------------
    output$upload_controls <- renderUI({
      sw <- input$software %||% "gnucash"

      if (sw == "gnucash") {
        .gnucash_controls_ui(ns)
      } else if (sw == "other") {
        .generic_controls_ui(ns)
      } else if (sw == "quickbooks") {
        .quickbooks_controls_ui(ns)
      } else {
        p(
          class = "text-muted small",
          bs_icon("clock"),
          " This software format is not yet supported. ",
          "Select GnuCash or Other / Generic CSV."
        )
      }
    })

    # -- Dynamic right panel ------------------------------------------------
    output$upload_results <- renderUI({
      sw <- input$software %||% "gnucash"
      arr <- input$file_arrangement %||% "combined"

      # Separate-files path: two side-by-side status + preview cards
      if (sw == "other" && arr == "separate") {
        layout_columns(
          col_widths = c(6, 6),
          .file_status_card(
            ns,
            side = "overhead",
            loaded = loaded$overhead,
            n_rows = if (!is.null(r$overhead_monthly)) {
              nrow(r$overhead_monthly)
            } else {
              0L
            }
          ),
          .file_status_card(
            ns,
            side = "income",
            loaded = loaded$income,
            n_rows = if (!is.null(r$income_monthly)) {
              nrow(r$income_monthly)
            } else {
              0L
            }
          )
        )
      } else {
        # Combined path (GnuCash or generic combined): validation badges +
        # account mapping + data preview
        tagList(
          uiOutput(ns("validation_badges")),
          if (sw %in% c("gnucash", "quickbooks")) {
            card(
              card_header(
                tagList(
                  bs_icon("table"),
                  " Account Mapping",
                  tooltip(
                    bs_icon("info-circle", title = "About account mapping"),
                    paste0(
                      "Shows how each GnuCash account name was matched to ",
                      "an expense category. Unmapped accounts are tagged ",
                      "'other'. Re-assign categories in Review & Edit."
                    )
                  )
                )
              ),
              card_body(
                uiOutput(ns("mapping_placeholder")),
                DT::dataTableOutput(ns("mapping_table"))
              )
            )
          },
          card(
            full_screen = TRUE,
            card_header(bs_icon("eye"), " Data Preview"),
            card_body(
              uiOutput(ns("preview_placeholder")),
              DT::dataTableOutput(ns("preview_table"))
            )
          )
        )
      }
    })

    # -- Helpers ------------------------------------------------------------
    # Initialise the empty transaction tibble and navigate to the Edit tab.
    go_to_manual_entry <- function() {
      r$transactions <- tibble::tibble(
        practice_id = character(0),
        date = as.Date(character(0)),
        week_start = as.Date(character(0)),
        month = integer(0),
        year = integer(0),
        full_account_name = character(0),
        account_name = character(0),
        description = character(0),
        amount = numeric(0),
        category = character(0),
        source = character(0)
      )
      r$overhead <- tibble::tibble(
        practice_id = character(0),
        date = as.Date(character(0)),
        week_start = as.Date(character(0)),
        month = integer(0),
        year = integer(0),
        full_account_name = character(0),
        account_name = character(0),
        description = character(0),
        amount = numeric(0),
        category = character(0),
        source = character(0),
        is_refund = logical(0)
      )
      r$income <- tibble::tibble(
        practice_id = character(0),
        date = as.Date(character(0)),
        week_start = as.Date(character(0)),
        month = integer(0),
        year = integer(0),
        full_account_name = character(0),
        account_name = character(0),
        description = character(0),
        revenue = numeric(0),
        category = character(0),
        source = character(0),
        is_refund = logical(0)
      )
      r$validation <- list()
      # Also clear any manual-period-summary / Quick Estimator scenario state
      # left over from a previous workflow (a real CSV upload, demo mode's
      # preload, or an earlier manual entry) -- otherwise mod_edit.R's
      # `!is.null(r$overhead_monthly) && is.null(r$scenario_inputs)` check
      # mistakes that leftover data for "manual periods already entered" and
      # shows the read-only summary card instead of the actual Quick
      # Estimator form.
      r$overhead_monthly <- NULL
      r$income_monthly <- NULL
      r$scenario_inputs <- NULL
      updateNavbarPage(
        parent_session %||% session,
        "main_nav",
        selected = "edit"
      )
    }

    # Initialise empty transaction tibble for generic CSV paths (no row-level
    # transaction data -- edit tab shows the "no transactions" card instead).
    init_generic_state <- function() {
      if (is.null(r$transactions)) {
        r$transactions <- tibble::tibble(
          practice_id = character(0),
          date = as.Date(character(0)),
          week_start = as.Date(character(0)),
          month = integer(0),
          year = integer(0),
          full_account_name = character(0),
          account_name = character(0),
          description = character(0),
          amount = numeric(0),
          category = character(0),
          source = character(0)
        )
      }
    }

    # -- Path selection handlers --------------------------------------------
    observeEvent(input$btn_use_real, {
      path_chosen("upload")
    })

    observeEvent(input$btn_use_plan, {
      go_to_manual_entry()
    })

    observeEvent(input$btn_use_calculator, {
      path_chosen("calculator")
    })

    observeEvent(input$btn_manual_entry, {
      path_chosen("manual")
    })

    observeEvent(input$btn_back, {
      path_chosen(NULL)
    })

    # -- Next button: shown once the upload path has loaded data ---------------
    output$next_btn <- renderUI({
      if (!isTRUE(path_chosen() == "upload")) {
        return(NULL)
      }
      has_data <- !is.null(r$overhead_monthly) && nrow(r$overhead_monthly) > 0
      if (!has_data) {
        return(NULL)
      }
      div(
        class = "d-flex justify-content-end mt-3",
        actionButton(
          ns("btn_next_to_edit"),
          tagList(bs_icon("pencil-square"), " Next: Review & Edit"),
          class = "btn-primary"
        )
      )
    })

    observeEvent(input$btn_next_to_edit, {
      updateNavbarPage(
        parent_session %||% session,
        "main_nav",
        selected = "edit"
      )
    })

    # -- Calculator sub-module -----------------------------------------------
    calculator_result <- mod_calculator_server("calculator", r, parent_session)

    observeEvent(
      calculator_result$go_back(),
      {
        path_chosen(NULL)
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # -- Manual entry sub-module --------------------------------------------
    manual_result <- mod_manual_entry_server("manual", r, parent_session)

    observeEvent(
      manual_result$go_back(),
      {
        path_chosen("upload")
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # -- Upload & process: GnuCash or generic combined ---------------------
    observeEvent(input$btn_upload, {
      req(input$csv_file)

      sw <- input$software %||% "gnucash"

      warnings_caught <- list()
      tryCatch(
        withCallingHandlers(
          {
            if (sw == "gnucash") {
              ext <- tolower(tools::file_ext(input$csv_file$name))
              if (ext == "gnucash") {
                transactions <- directCareForecastR::ingest_gnucash_xml(
                  path = input$csv_file$datapath,
                  practice_id = r$practice_id
                )
              } else {
                transactions <- directCareForecastR::ingest_gnucash_csv(
                  path = input$csv_file$datapath,
                  practice_id = r$practice_id
                )
              }
              overhead <- directCareForecastR::filter_gnucash_overhead(
                transactions
              )
              income <- directCareForecastR::normalize_gnucash_income(
                transactions
              )
              r$transactions <- transactions
            } else if (sw == "quickbooks") {
              transactions <- directCareForecastR::ingest_quickbooks_csv(
                path = input$csv_file$datapath,
                practice_id = r$practice_id
              )
              overhead <- directCareForecastR::filter_gnucash_overhead(
                transactions
              )
              income <- directCareForecastR::normalize_gnucash_income(
                transactions
              )
              r$transactions <- transactions
            } else {
              # Generic combined file
              result <- directCareForecastR::ingest_csv_generic(
                path = input$csv_file$datapath,
                practice_id = r$practice_id,
                col_date = trimws(input$col_date %||% "date"),
                col_amount = trimws(input$col_amount %||% "amount"),
                col_type = trimws(input$col_type %||% "type"),
                overhead_pattern = trimws(
                  input$overhead_pattern %||% "expense"
                ),
                income_pattern = trimws(input$income_pattern %||% "income"),
                type = "both"
              )
              overhead <- result$overhead
              income <- result$income
              init_generic_state()
            }

            overhead_monthly <- directCareForecastR::summarize_overhead_monthly(
              overhead
            )
            income_monthly <- directCareForecastR::summarize_income_monthly(
              income
            )

            r$overhead <- overhead
            r$income <- income
            r$overhead_monthly <- overhead_monthly
            r$income_monthly <- income_monthly
            r$validation <- warnings_caught

            if (sw == "other") {
              loaded$overhead <- TRUE
              loaded$income <- TRUE
            }
          },
          warning = function(w) {
            warnings_caught[[length(warnings_caught) + 1L]] <<- w
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          showNotification(
            paste0("Upload failed: ", conditionMessage(e)),
            type = "error",
            duration = 8
          )
        }
      )

      if (!is.null(r$overhead_monthly)) {
        n_ovhd <- nrow(r$overhead %||% data.frame())
        n_inc <- nrow(r$income %||% data.frame())
        showNotification(
          paste0(
            "(done) Loaded ",
            n_ovhd,
            " overhead row",
            if (n_ovhd != 1L) "s" else "",
            " and ",
            n_inc,
            " income row",
            if (n_inc != 1L) "s" else "",
            " for ",
            r$practice_name
          ),
          type = "message",
          duration = 4
        )
      }
    })

    # -- Load overhead file (separate mode) ---------------------------------
    observeEvent(input$btn_load_overhead, {
      req(input$overhead_file)

      warnings_caught <- list()
      tryCatch(
        withCallingHandlers(
          {
            overhead <- directCareForecastR::ingest_csv_generic(
              path = input$overhead_file$datapath,
              practice_id = r$practice_id,
              col_date = trimws(input$ovhd_col_date %||% "date"),
              col_amount = trimws(input$ovhd_col_amount %||% "amount"),
              type = "overhead"
            )
            overhead_monthly <- directCareForecastR::summarize_overhead_monthly(
              overhead
            )
            r$overhead <- overhead
            r$overhead_monthly <- overhead_monthly
            r$validation <- c(r$validation %||% list(), warnings_caught)
            loaded$overhead <- TRUE
            init_generic_state()
          },
          warning = function(w) {
            warnings_caught[[length(warnings_caught) + 1L]] <<- w
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          showNotification(
            paste0("Overhead file failed: ", conditionMessage(e)),
            type = "error",
            duration = 8
          )
        }
      )

      if (loaded$overhead) {
        n <- nrow(r$overhead)
        showNotification(
          paste0(
            "(done) Loaded ",
            n,
            " overhead row",
            if (n != 1L) "s" else ""
          ),
          type = "message",
          duration = 4
        )
      }
    })

    # -- Load income file (separate mode) -----------------------------------
    observeEvent(input$btn_load_income, {
      req(input$income_file)

      warnings_caught <- list()
      tryCatch(
        withCallingHandlers(
          {
            income <- directCareForecastR::ingest_csv_generic(
              path = input$income_file$datapath,
              practice_id = r$practice_id,
              col_date = trimws(input$inc_col_date %||% "date"),
              col_amount = trimws(input$inc_col_amount %||% "amount"),
              type = "income"
            )
            income_monthly <- directCareForecastR::summarize_income_monthly(
              income
            )
            r$income <- income
            r$income_monthly <- income_monthly
            r$validation <- c(r$validation %||% list(), warnings_caught)
            loaded$income <- TRUE
            init_generic_state()
          },
          warning = function(w) {
            warnings_caught[[length(warnings_caught) + 1L]] <<- w
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          showNotification(
            paste0("Income file failed: ", conditionMessage(e)),
            type = "error",
            duration = 8
          )
        }
      )

      if (loaded$income) {
        n <- nrow(r$income)
        showNotification(
          paste0(
            "(done) Loaded ",
            n,
            " income row",
            if (n != 1L) "s" else ""
          ),
          type = "message",
          duration = 4
        )
      }
    })

    # -- Generic file inputs (combined vs separate toggle contents) ----------
    output$generic_file_inputs <- renderUI({
      arr <- input$file_arrangement %||% "combined"
      if (arr == "combined") {
        tagList(
          fileInput(
            ns("csv_file"),
            NULL,
            accept = ".csv",
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          ),
          layout_columns(
            col_widths = c(6, 6),
            textInput(ns("col_date"), "Date column", value = "date"),
            textInput(ns("col_amount"), "Amount column", value = "amount")
          ),
          layout_columns(
            col_widths = c(6, 6),
            textInput(ns("col_type"), "Type column", value = "type"),
            NULL
          ),
          layout_columns(
            col_widths = c(6, 6),
            textInput(
              ns("overhead_pattern"),
              "Overhead keyword",
              value = "expense"
            ),
            textInput(ns("income_pattern"), "Income keyword", value = "income")
          ),
          tags$p(
            class = "text-muted small",
            bs_icon("info-circle"),
            " The type column identifies each row as overhead or income. ",
            "Rows whose type value contains the overhead keyword go to ",
            "overhead; rows containing the income keyword go to income."
          ),
          div(class = "mt-2"),
          actionButton(
            ns("btn_upload"),
            "Upload & Process",
            icon = icon("upload"),
            class = "btn-primary w-100"
          )
        )
      } else {
        # Separate files: status cards live in the right panel; left panel
        # just needs a note directing the user there.
        tags$p(
          class = "text-muted small mt-1",
          bs_icon("arrow-right"),
          " Upload each file independently using the panels on the right."
        )
      }
    })

    # -- Placeholder copy ---------------------------------------------------
    output$mapping_placeholder <- renderUI({
      if (is.null(r$transactions)) {
        p(class = "text-muted", "Upload a CSV file to see the account mapping.")
      }
    })

    output$preview_placeholder <- renderUI({
      if (is.null(r$transactions) && is.null(r$overhead_monthly)) {
        p(class = "text-muted", "Upload a CSV file to preview your data.")
      }
    })

    # -- Validation badges --------------------------------------------------
    output$validation_badges <- renderUI({
      req(!is.null(r$overhead_monthly) || !is.null(r$transactions))

      flags <- r$validation %||% list()
      classes <- vapply(flags, \(w) class(w)[1], character(1))

      badges <- list()

      if (!is.null(r$transactions)) {
        badges[["rows"]] <- value_box(
          title = "Transactions loaded",
          value = nrow(r$transactions),
          theme = "primary",
          height = "80px"
        )
      }
      badges[["overhead"]] <- value_box(
        title = "Expense rows",
        value = nrow(r$overhead %||% data.frame()),
        theme = "secondary",
        height = "80px"
      )
      badges[["income"]] <- value_box(
        title = "Income rows",
        value = nrow(r$income %||% data.frame()),
        theme = "primary",
        height = "80px"
      )
      if (any(classes == "dcForecastR_refunds_detected")) {
        badges[["refunds"]] <- value_box(
          title = "Refunds flagged",
          value = sum(classes == "dcForecastR_refunds_detected"),
          theme = "warning",
          height = "80px"
        )
      }
      if (any(classes == "dcForecastR_future_dates")) {
        badges[["future"]] <- value_box(
          title = "Future-dated rows",
          value = sum(classes == "dcForecastR_future_dates"),
          theme = "danger",
          height = "80px"
        )
      }
      if (any(classes == "dcForecastR_implausible_old_dates")) {
        badges[["old_dates"]] <- value_box(
          title = "Pre-2000 dated rows",
          value = sum(classes == "dcForecastR_implausible_old_dates"),
          theme = "danger",
          height = "80px"
        )
      }
      if (any(classes == "dcForecastR_unmapped_accounts")) {
        badges[["unmapped"]] <- value_box(
          title = "Unmapped accounts",
          value = "See mapping",
          theme = "warning",
          height = "80px"
        )
      }

      layout_column_wrap(width = "160px", fill = FALSE, !!!badges)
    })

    # -- Account mapping table (GnuCash only) ------------------------------
    output$mapping_table <- DT::renderDataTable({
      req(r$transactions)
      r$transactions |>
        dplyr::count(account_name, category, name = "rows") |>
        dplyr::arrange(category, account_name) |>
        DT::datatable(
          rownames = FALSE,
          colnames = c("Account Name", "Category", "Rows"),
          options = list(pageLength = 15, dom = "ftp"),
          selection = "none"
        )
    })

    # -- Data preview table -------------------------------------------------
    output$preview_table <- DT::renderDataTable({
      # GnuCash path: show transaction rows
      if (!is.null(r$transactions) && nrow(r$transactions) > 0L) {
        return(
          r$transactions |>
            dplyr::select(
              date,
              account_name,
              full_account_name,
              description,
              amount,
              category
            ) |>
            DT::datatable(
              rownames = FALSE,
              options = list(pageLength = 10, dom = "ftp", scrollX = TRUE),
              selection = "none"
            )
        )
      }
      # Generic combined path: show overhead summary
      req(r$overhead_monthly)
      r$overhead_monthly |>
        DT::datatable(
          rownames = FALSE,
          options = list(pageLength = 10, dom = "ftp", scrollX = TRUE),
          selection = "none"
        )
    })
  })
}


# -- UI helpers (called from renderUI, not exported) -----------------------

.gnucash_controls_ui <- function(ns) {
  tagList(
    fileInput(
      ns("csv_file"),
      tagList(
        "CSV Export",
        tooltip(
          bs_icon("info-circle", title = "Expected file format"),
          placement = "right",
          tagList(
            tags$strong("GnuCash file formats"),
            tags$br(),
            tags$p(
              class = "mb-1",
              tags$strong(".gnucash"),
              " \u2014 upload your GnuCash ledger file directly (recommended)."
            ),
            tags$p(
              class = "mb-1",
              tags$strong(".csv"),
              " \u2014 use ",
              tags$em("File \u2192 Export \u2192 Export Transactions to CSV"),
              " in GnuCash. The file must include these columns:",
              tags$ul(
                class = "mb-0 mt-1 ps-3",
                tags$li(
                  tags$code("Date"),
                  " \u2014 transaction date (MM/DD/YYYY)"
                ),
                tags$li(
                  tags$code("Account Name"),
                  " \u2014 account (e.g. Expenses:Rent)"
                ),
                tags$li(
                  tags$code("Amount Num."),
                  " \u2014 signed numeric amount"
                )
              )
            )
          )
        )
      ),
      accept = c(".csv", ".gnucash"),
      buttonLabel = "Browse...",
      placeholder = "No file selected"
    ),
    accordion(
      open = FALSE,
      accordion_panel(
        title = "File format reference",
        icon = bs_icon("table"),
        tags$p(
          class = "small text-muted mb-2",
          "Your CSV should look like the example below. Extra columns are ignored."
        ),
        tags$div(
          class = "table-responsive",
          tags$table(
            class = "table table-sm table-bordered small mb-0",
            style = "font-family: monospace; font-size: 0.75rem;",
            tags$thead(
              tags$tr(
                tags$th("Date"),
                tags$th("Account Name"),
                tags$th("Amount Num.")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("01/15/2024"),
                tags$td("Expenses:Rent"),
                tags$td("1200.00")
              ),
              tags$tr(
                tags$td("01/15/2024"),
                tags$td("Income:Membership"),
                tags$td("3500.00")
              ),
              tags$tr(
                tags$td("02/01/2024"),
                tags$td("Expenses:Utilities"),
                tags$td("145.00")
              )
            )
          )
        ),
        tags$p(
          class = "small text-muted mt-2 mb-0",
          bs_icon("lightbulb"),
          " Both income and expense amounts are positive \u2014 the account ",
          "name (e.g. ",
          tags$code("Expenses:..."),
          " vs ",
          tags$code("Income:..."),
          ") determines how each row is classified."
        )
      )
    ),
    div(class = "mt-3"),
    actionButton(
      ns("btn_upload"),
      "Upload & Process",
      icon = icon("upload"),
      class = "btn-primary w-100"
    ),
    div(class = "my-2 text-center text-muted small", "\u2014 or \u2014"),
    actionButton(
      ns("btn_manual_entry"),
      "Enter Data Manually",
      icon = bs_icon("keyboard"),
      class = "btn-outline-secondary w-100"
    )
  )
}

.quickbooks_controls_ui <- function(ns) {
  tagList(
    tags$div(
      class = "alert alert-warning small py-2 px-3 mb-3",
      bs_icon("exclamation-triangle"),
      tags$strong(" Experimental — "),
      "built from QuickBooks Online's documented export format, but not yet ",
      "tested against a real QuickBooks export. Please double-check the ",
      "results (especially overhead vs. income totals) before relying on them."
    ),
    fileInput(
      ns("csv_file"),
      tagList(
        "CSV Export",
        tooltip(
          bs_icon("info-circle", title = "Expected file format"),
          placement = "right",
          tagList(
            tags$strong("QuickBooks Online export"),
            tags$br(),
            tags$p(
              class = "mb-1",
              "In QuickBooks Online: ",
              tags$em("Reports → Transaction List by Date → Export to CSV"),
              ". The file must include these columns:",
              tags$ul(
                class = "mb-0 mt-1 ps-3",
                tags$li(
                  tags$code("Date"),
                  " — transaction date"
                ),
                tags$li(
                  tags$code("Account"),
                  " — the account name (e.g. Rent Expense)"
                ),
                tags$li(
                  tags$code("Amount"),
                  " — signed amount (negative = money out, positive = ",
                  "money in)"
                )
              )
            )
          )
        )
      ),
      accept = ".csv",
      buttonLabel = "Browse...",
      placeholder = "No file selected"
    ),
    accordion(
      open = FALSE,
      accordion_panel(
        title = "File format reference",
        icon = bs_icon("table"),
        tags$p(
          class = "small text-muted mb-2",
          "Your CSV should look like the example below. Extra columns are ignored."
        ),
        tags$div(
          class = "table-responsive",
          tags$table(
            class = "table table-sm table-bordered small mb-0",
            style = "font-family: monospace; font-size: 0.75rem;",
            tags$thead(
              tags$tr(
                tags$th("Date"),
                tags$th("Account"),
                tags$th("Amount")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("01/15/2024"),
                tags$td("Rent Expense"),
                tags$td("-1200.00")
              ),
              tags$tr(
                tags$td("01/15/2024"),
                tags$td("Membership Fees"),
                tags$td("3500.00")
              ),
              tags$tr(
                tags$td("02/01/2024"),
                tags$td("Office Supplies"),
                tags$td("-145.00")
              )
            )
          )
        ),
        tags$p(
          class = "small text-muted mt-2 mb-0",
          bs_icon("lightbulb"),
          " Money leaving the practice (bills, checks, expenses) is negative; ",
          "money coming in (invoices, sales receipts, deposits) is positive ",
          "— that sign determines whether each row is overhead or income."
        )
      )
    ),
    div(class = "mt-3"),
    actionButton(
      ns("btn_upload"),
      "Upload & Process",
      icon = icon("upload"),
      class = "btn-primary w-100"
    ),
    div(class = "my-2 text-center text-muted small", "— or —"),
    actionButton(
      ns("btn_manual_entry"),
      "Enter Data Manually",
      icon = bs_icon("keyboard"),
      class = "btn-outline-secondary w-100"
    )
  )
}

.generic_controls_ui <- function(ns) {
  tagList(
    radioButtons(
      ns("file_arrangement"),
      label = tags$span(class = "fw-semibold", "File arrangement"),
      choiceNames = list(
        tagList(
          "Combined ",
          tags$span(
            class = "text-muted small",
            "\u2014 one file with both income and overhead"
          )
        ),
        tagList(
          "Separate ",
          tags$span(
            class = "text-muted small",
            "\u2014 one file per data type"
          )
        )
      ),
      choiceValues = list("combined", "separate"),
      selected = "combined"
    ),
    uiOutput(ns("generic_file_inputs"))
  )
}

# Rendered server-side so it can react to input$file_arrangement.
# Called from output$upload_controls via .generic_controls_ui().
# The actual renderUI is defined in the server below; this helper just
# produces the static wrapper that hosts it.

.file_status_card <- function(ns, side, loaded, n_rows) {
  label <- if (side == "overhead") "Overhead / Expenses" else "Revenue / Income"
  icon_name <- if (side == "overhead") "receipt" else "cash-coin"
  col_date_id <- if (side == "overhead") {
    ns("ovhd_col_date")
  } else {
    ns("inc_col_date")
  }
  col_amount_id <- if (side == "overhead") {
    ns("ovhd_col_amount")
  } else {
    ns("inc_col_amount")
  }
  file_id <- if (side == "overhead") ns("overhead_file") else ns("income_file")
  btn_id <- if (side == "overhead") {
    ns("btn_load_overhead")
  } else {
    ns("btn_load_income")
  }

  status_ui <- if (loaded) {
    tags$p(
      class = "text-success small mb-0",
      bs_icon("check-circle-fill"),
      paste0(
        " Loaded \u2014 ",
        n_rows,
        " period",
        if (n_rows != 1L) "s" else ""
      )
    )
  } else {
    tags$p(
      class = "text-muted small mb-0",
      bs_icon("hourglass"),
      " Not yet loaded"
    )
  }

  card(
    card_header(tagList(bs_icon(icon_name), " ", label)),
    card_body(
      fileInput(
        file_id,
        NULL,
        accept = ".csv",
        buttonLabel = "Browse...",
        placeholder = "No file selected"
      ),
      layout_columns(
        col_widths = c(6, 6),
        textInput(col_date_id, "Date column", value = "date", width = "100%"),
        textInput(
          col_amount_id,
          "Amount column",
          value = "amount",
          width = "100%"
        )
      ),
      actionButton(
        btn_id,
        paste("Load", label),
        icon = bs_icon("upload"),
        class = "btn-primary w-100 mt-1"
      ),
      div(class = "mt-2", status_ui)
    )
  )
}
