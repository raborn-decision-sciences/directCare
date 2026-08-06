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

    # -- Branded footer -------------------------------------------------------
    tags$div(
      class = "mt-5 pt-3 border-top d-flex align-items-center justify-content-center gap-3",
      style = "opacity:0.55;",
      tags$img(
        src = "www/logo-rds-alt.svg",
        height = "36px",
        alt = "Raborn Decision Sciences",
        class = "footer-logo"
      )
    )
  )
}


#' Upload Module Server
#'
#' @param id Module namespace ID.
#' @param r Shared `reactiveValues` object from `app_server`.
#' @param parent_session The top-level Shiny session for cross-tab navigation.
#' @param demo_mode Reactive (`reactiveVal`) from `app_server`, `TRUE` for a
#'   demo session. `NULL` (the default) is treated as always-`FALSE`, for
#'   test harnesses that don't set up demo mode at all.
#' @return A list of reactives for `app_server` to observe: `tour_historical_click`,
#'   `tour_plan_click`, `tour_calculator_click` -- one per landing-page
#'   "Take the tour" link (see `.tour_launch()` callers in `app_server.R`);
#'   `nav_footer`, the UI for the central sticky nav bar (delegates to the
#'   Calculator/Manual-Entry sub-modules' own `nav_footer` when one of those
#'   paths is active); `path_chosen`, this tab's currently active sub-path
#'   (`NULL`/`"upload"`/`"calculator"`/`"manual"`), read by `app_server.R`'s
#'   `output$main_scenario_banner` to pick the right banner instance.
#' @noRd
mod_upload_server <- function(id, r, parent_session = NULL, demo_mode = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Normalize a missing demo_mode (test harnesses) to an always-FALSE
    # reactive, so `demo_mode()` is always safe to call below.
    demo_mode <- demo_mode %||% reactiveVal(FALSE)

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
          # Gated at pro+ (bookkeeping upload is a paid feature, see
          # STRIPE_BILLING.md's v1 gating scope) -- demo sessions are
          # always exempt, since demo_mode() never represents a real
          # billable account. .has_paid_plan()/.locked_feature_card() live
          # in utils_billing.R. The "Take the tour" link stays available
          # even when locked -- the tour itself still works against demo
          # data regardless of plan_tier, and seeing the real workflow is
          # reasonable pre-purchase context, not something to gate.
          if (!isTRUE(demo_mode()) && !.has_paid_plan(r$plan_tier)) {
            .locked_feature_card(
              title = "Use Real Data",
              description = paste0(
                "Use your income and overhead data to analyse actual ",
                "revenue and expenses. Best for practices that already ",
                "use accounting software or maintain transaction records."
              ),
              ns = ns,
              extra = actionLink(
                ns("tour_historical"),
                tagList(bs_icon("play-circle"), " Take the tour"),
                class = "d-block text-center small text-muted mt-2"
              )
            )
          } else {
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
                ),
                actionLink(
                  ns("tour_historical"),
                  tagList(bs_icon("play-circle"), " Take the tour"),
                  class = "d-block text-center small text-muted mt-2"
                )
              )
            )
          },
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
              ),
              actionLink(
                ns("tour_plan"),
                tagList(bs_icon("play-circle"), " Take the tour"),
                class = "d-block text-center small text-muted mt-2"
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
              ),
              actionLink(
                ns("tour_calculator"),
                tagList(bs_icon("play-circle"), " Take the tour"),
                class = "d-block text-center small text-muted mt-2"
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
              uiOutput(ns("upload_controls"))
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
    # In demo mode, "Use Real Data" skips the file-input mechanics entirely
    # -- there's no real bookkeeping file to browse for, so loading the
    # bundled sample transactions directly and landing on Edit (mirroring
    # what a real upload's own "Next" button already does) is both simpler
    # and closes off the file-input/manual-entry paths as a side effect,
    # since path_chosen("upload") -- the only place either of those render
    # -- is never reached for a demo session. See utils_demo.R's
    # .load_demo_data() and app_server.R's demo-mode-aware tour-advance
    # right after this input's id for the tour-side half of this change.
    observeEvent(input$btn_use_real, {
      if (isTRUE(demo_mode())) {
        .load_demo_data(r)
        # Explains the skip explicitly -- without this, clicking "Use
        # Bookkeeping Data" and landing directly on Review & Edit (no file
        # picker in between) reads as a bug rather than a deliberate demo
        # shortcut.
        showNotification(
          paste0(
            "Demo mode: loaded sample bookkeeping data for you — ",
            "browsing your own file isn't available in demo mode."
          ),
          type = "message",
          duration = 6
        )
        updateNavbarPage(parent_session %||% session, "main_nav", selected = "edit")
      } else if (!.has_paid_plan(r$plan_tier)) {
        # Defense in depth: the button itself is already swapped for a
        # locked-feature card above when this condition holds, but this
        # guards the actual transition too, in case that ever drifts out
        # of sync or the id is triggered some other way (e.g. directly via
        # Shiny.setInputValue()) -- see the same reasoning noted on every
        # other gate in this app.
        .show_plans_modal()
      } else {
        path_chosen("upload")
      }
    })

    # Triggered from .locked_feature_card()'s "See plans" button, wherever
    # this module renders one (currently just the "Use Real Data" gate
    # above).
    observeEvent(input$btn_see_plans, {
      .show_plans_modal()
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

    # -- Consolidated nav bar: delegates to whichever sub-path is active ----
    # Returned for the central output$main_nav_footer (app_server.R) to
    # render -- see that file's comment for why this lives centrally rather
    # than in each module's own content. Save/Load (the forecast-scenario
    # widget, `extra`) stays visible even on the path-selection landing
    # cards (path_chosen() == NULL) -- it was always a global, unconditional
    # widget before this refactor, not something gated on having picked a
    # path yet -- just with no Back/Next since there's nothing to navigate
    # yet at that point.
    nav_footer <- reactive({
      path <- path_chosen()
      if (is.null(path)) {
        # No step/workflow indicator here -- none of the 4 Upload -> Edit
        # -> Summary -> Projections steps (or Calculator/Manual Entry)
        # apply yet, since no path has been chosen. Save/Load stays
        # available regardless.
        return(.tour_nav_footer(
          extra = directCareScenarios::mod_scenario_slots_ui("scenario")
        ))
      }
      if (path == "calculator") {
        return(calculator_result$nav_footer())
      }
      if (path == "manual") {
        return(manual_result$nav_footer())
      }
      # path == "upload": Next only appears once real data has loaded.
      has_data <- !is.null(r$overhead_monthly) && nrow(r$overhead_monthly) > 0
      .tour_nav_footer(
        current_step = 1L,
        back = actionButton(
          ns("btn_back"),
          "Back",
          icon = bs_icon("arrow-left"),
          class = "btn-outline-secondary"
        ),
        forward = if (has_data) {
          actionButton(
            ns("btn_next_to_edit"),
            tagList(bs_icon("pencil-square"), " Next: Review & Edit"),
            class = "btn-primary"
          )
        },
        extra = directCareScenarios::mod_scenario_slots_ui("scenario")
      )
    })

    # -- Upload & process: GnuCash or generic combined ---------------------
    observeEvent(input$btn_upload, {
      req(input$csv_file)

      sw <- input$software %||% "gnucash"

      warnings_caught <- list()
      # Tracks whether *this* upload attempt succeeded -- the success
      # notification below must not fire off stale r$overhead_monthly left
      # over from a previous successful upload in the same session (that
      # was silently reusing old state and showing a "(done) Loaded..."
      # toast right alongside the "Upload failed" one whenever a later
      # attempt errored out, e.g. a CSV missing the type column).
      upload_ok <- tryCatch(
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

            TRUE
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
          FALSE
        }
      )

      if (isTRUE(upload_ok) && !is.null(r$overhead_monthly)) {
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
      # See btn_upload's identical upload_ok comment above -- same fix,
      # same reason (this attempt's success notification must not fire off
      # a stale successful load from earlier in the session).
      load_ok <- tryCatch(
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
            TRUE
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
          FALSE
        }
      )

      if (isTRUE(load_ok)) {
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
      # See btn_upload's identical upload_ok comment above -- same fix,
      # same reason (this attempt's success notification must not fire off
      # a stale successful load from earlier in the session).
      load_ok <- tryCatch(
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
            TRUE
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
          FALSE
        }
      )

      if (isTRUE(load_ok)) {
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

    # -- Landing-page tour links: returned for app_server.R to observe -------
    # (in addition to the existing Help-modal tour buttons) -- app_server.R
    # owns .tour_launch()/the guide objects, not this module, so these are
    # plain click-count reactives for the caller to react to, matching the
    # existing calculator_result()/manual_result() sub-module return pattern
    # already used elsewhere in this file.
    list(
      tour_historical_click = reactive(input$tour_historical),
      tour_plan_click = reactive(input$tour_plan),
      tour_calculator_click = reactive(input$tour_calculator),
      nav_footer = nav_footer,
      path_chosen = path_chosen
    )
  })
}


# -- UI helpers (called from renderUI, not exported) -----------------------

.gnucash_controls_ui <- function(ns) {
  tagList(
    # Wrapped in an explicit id for the same reason overhead_model_wrap /
    # income_growth_wrap are (mod_projections.R): fileInput()'s underlying
    # <input type="file"> already carries Bootstrap's own `position:
    # absolute` styling (its standard "hide the native file input under a
    # custom Browse button" trick) -- when driver.js's own highlight()
    # tries to reposition that same already-absolutely-positioned element
    # for its spotlight effect, the two positioning schemes conflict and
    # shove the real element (and, cascading from it, the popover) tens
    # of thousands of pixels off-screen. Confirmed live via direct DOM
    # inspection: both the highlighted <input> and the popover ended up
    # at getBoundingClientRect() x/y around -99900, rendering the tour
    # step (utils_tours.R's tour_h2 "Choose a file" step) completely
    # invisible with no console error and no clear signal why. Targeting
    # this wrapper div (which driver.js has no reason to reposition)
    # instead of the raw input sidesteps the conflict entirely.
    tags$div(
      id = ns("csv_file_wrap"),
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
    )
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
                tags$th("Full Account Name"),
                tags$th("Account Name"),
                tags$th("Description"),
                tags$th("Amount Num.")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("01/15/2024"),
                tags$td("Expenses:Rent"),
                tags$td("Rent"),
                tags$td("Monthly rent"),
                tags$td("1200.00")
              ),
              tags$tr(
                tags$td("01/15/2024"),
                tags$td("Income:Membership"),
                tags$td("Membership"),
                tags$td("Membership fee"),
                tags$td("3500.00")
              ),
              tags$tr(
                tags$td("02/01/2024"),
                tags$td("Expenses:Utilities"),
                tags$td("Utilities"),
                tags$td("Electric bill"),
                tags$td("145.00")
              )
            )
          )
        ),
        tags$p(
          class = "small text-muted mt-2 mb-0",
          bs_icon("info-circle"),
          " All five columns above are required. GnuCash's own CSV export ",
          "includes them by default (plus others, which are ignored) — ",
          "column names are case-sensitive."
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
    accordion(
      open = FALSE,
      accordion_panel(
        title = "File format reference",
        icon = bs_icon("table"),
        tags$p(
          class = "small text-muted mb-2",
          "Your CSV should look like the example below. Extra columns are ",
          "ignored."
        ),
        tags$div(
          class = "table-responsive",
          tags$table(
            class = "table table-sm table-bordered small mb-0",
            style = "font-family: monospace; font-size: 0.75rem;",
            tags$thead(
              tags$tr(
                tags$th("date"),
                tags$th("amount"),
                tags$th("type")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("01/15/2024"),
                tags$td("1200.00"),
                tags$td("expense")
              ),
              tags$tr(
                tags$td("01/15/2024"),
                tags$td("3500.00"),
                tags$td("income")
              ),
              tags$tr(
                tags$td("02/01/2024"),
                tags$td("145.00"),
                tags$td("expense")
              )
            )
          )
        ),
        tags$p(
          class = "small text-muted mt-2 mb-1",
          bs_icon("exclamation-triangle"),
          " Column names must match ",
          tags$strong("exactly"),
          " (case-sensitive) what you type in the ",
          tags$em("Date column"),
          "/",
          tags$em("Amount column"),
          "/",
          tags$em("Type column"),
          " boxes below — a header of ",
          tags$code("Date"),
          " will not match a box set to ",
          tags$code("date"),
          ". If your export uses different capitalization (common with ",
          "Excel or Google Sheets exports), either edit the header row or ",
          "update the boxes to match it exactly."
        ),
        tags$p(
          class = "small text-muted mb-1",
          bs_icon("lightbulb"),
          " Dates: most common formats are recognized automatically (e.g. ",
          tags$code("01/15/2024"),
          ", ",
          tags$code("2024-01-15"),
          ", or ",
          tags$code("Jan 15 2024"),
          "). If a column has dates that aren't being read correctly, ",
          "try reformatting it to ",
          tags$code("YYYY-MM-DD"),
          "."
        ),
        tags$p(
          class = "small text-muted mb-0",
          bs_icon("lightbulb"),
          " Amounts should be positive for both income and expense rows ",
          "— the type column (not the sign) determines how each row ",
          "is classified."
        )
      )
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
