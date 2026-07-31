#' Manual Data Entry Sub-Module UI
#'
#' Structured form for entering aggregate-level historical Overhead and Income
#' data when the user does not have a bookkeeping CSV export. Produces the same
#' \code{r$overhead_monthly} / \code{r$income_monthly} output as the upload
#' path so all downstream modules (Summary, Projections) work identically.
#'
#' @param id Module namespace ID.
#' @noRd
mod_manual_entry_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Set Up / Overhead / Income as sub-tabs (matching the pattern already
    # used for Projections' forecast-type tabs) instead of three cards
    # stacked in one long scroll -- once the entry tables render, Setup +
    # Overhead + Income together left little breathing room and made simple
    # data entry feel cramped. navset_card_underline() is already a card, so
    # each nav_panel's content goes directly inside it, not wrapped in its
    # own card() -- same convention Projections' own sub-tabs already use.
    navset_card_underline(
      id = ns("manual_tabs"),
      nav_panel(
        "Set Up",
        value = "setup",
        icon = bs_icon("sliders"),
        layout_columns(
          col_widths = c(3, 9),
          div(
            tags$label(
              class = "form-label fw-semibold small text-uppercase text-muted",
              "Frequency"
            ),
            radioButtons(
              ns("manual_freq"),
              label = NULL,
              choices = c("Monthly" = "monthly", "Weekly" = "weekly"),
              selected = "monthly",
              inline = TRUE
            )
          ),
          uiOutput(ns("freq_setup_ui"))
        ),
        layout_columns(
          col_widths = c(4, 8),
          numericInput(
            ns("manual_n_periods"),
            "Number of Periods",
            value = 12L,
            min = 1L,
            max = 104L,
            step = 1L
          ),
          div(
            class = "d-flex align-items-end pb-1",
            actionButton(
              ns("btn_gen_manual"),
              tagList(bs_icon("table"), " Generate Entry Table"),
              class = "btn-primary"
            )
          )
        )
      ),
      nav_panel(
        "Overhead",
        value = "overhead",
        icon = bs_icon("dash-circle"),
        uiOutput(ns("overhead_panel_ui"))
      ),
      nav_panel(
        "Income",
        value = "income",
        icon = bs_icon("plus-circle"),
        uiOutput(ns("income_panel_ui"))
      )
    )
  )
}


#' Manual Data Entry Sub-Module Server
#'
#' @param id Module namespace ID.
#' @param r  Shared \code{reactiveValues} from \code{app_server}.
#' @param parent_session Top-level Shiny session for cross-tab navigation.
#' @return A list with \code{go_back}, a reactive that fires when the user
#'   clicks Back (used by the parent to reset \code{path_chosen}), and
#'   \code{nav_footer}, the UI for the central sticky nav bar when this
#'   sub-path is active.
#' @noRd
mod_manual_entry_server <- function(id, r, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Local reactive state --------------------------------------------------
    manual_ovhd <- reactiveVal(NULL) # data.frame: periods x overhead categories
    manual_inc <- reactiveVal(NULL) # data.frame: periods x income categories
    manual_meta <- reactiveVal(NULL) # list(dates, labels, freq)

    ovhd_proxy <- DT::dataTableProxy("ovhd_tbl", session = session)
    inc_proxy <- DT::dataTableProxy("inc_tbl", session = session)

    # -- Frequency-specific start-period inputs --------------------------------
    output$freq_setup_ui <- renderUI({
      freq <- input$manual_freq %||% "monthly"
      if (freq == "monthly") {
        layout_columns(
          col_widths = c(6, 6),
          selectInput(
            ns("manual_start_month"),
            "Start Month",
            choices = setNames(seq_len(12L), month.name),
            selected = 1L
          ),
          numericInput(
            ns("manual_start_year"),
            "Start Year",
            value = as.integer(format(Sys.Date(), "%Y")) - 1L,
            min = 2000L,
            max = 2100L,
            step = 1L
          )
        )
      } else {
        # Default: Monday of the week 52 weeks ago
        ago <- Sys.Date() - 365L
        start <- ago - (as.integer(format(ago, "%u")) - 1L)
        dateInput(
          ns("manual_start_date"),
          "Start Week (aligned to Monday)",
          value = start
        )
      }
    })

    # -- Generate initial zero-filled tables -----------------------------------
    observeEvent(input$btn_gen_manual, {
      freq <- input$manual_freq %||% "monthly"
      n <- max(1L, min(104L, as.integer(input$manual_n_periods %||% 12L)))

      if (freq == "monthly") {
        yr <- as.integer(
          input$manual_start_year %||%
            (as.integer(format(Sys.Date(), "%Y")) - 1L)
        )
        mo <- as.integer(input$manual_start_month %||% 1L)
        dates <- seq(
          as.Date(sprintf("%04d-%02d-01", yr, mo)),
          by = "month",
          length.out = n
        )
        labels <- format(dates, "%B %Y")
      } else {
        raw <- as.Date(input$manual_start_date %||% (Sys.Date() - 365L))
        start <- raw - (as.integer(format(raw, "%u")) - 1L) # snap to Monday
        dates <- seq(start, by = "7 days", length.out = n)
        labels <- paste0("Wk of ", format(dates, "%b %d, %Y"))
      }

      manual_meta(list(dates = dates, labels = labels, freq = freq))

      manual_ovhd(data.frame(
        Period = labels,
        Rent = 0,
        Payroll = 0,
        `EHR` = 0,
        Malpractice = 0,
        Supplies = 0,
        Other = 0,
        Total = 0,
        check.names = FALSE,
        stringsAsFactors = FALSE
      ))

      manual_inc(data.frame(
        Period = labels,
        Membership = 0,
        FFS = 0,
        `Other Income` = 0,
        Total = 0,
        check.names = FALSE,
        stringsAsFactors = FALSE
      ))

      # Jumps straight to the Overhead tab once a table exists -- the whole
      # point of Generate Entry Table is to start filling in data, and
      # without this the user would land back on Set Up (the tab they were
      # already on) and have to click Overhead themselves.
      bslib::nav_select("manual_tabs", selected = "overhead", session = session)
    })

    tables_ready <- reactive(!is.null(manual_ovhd()))

    # -- Render editable DT tables ---------------------------------------------
    output$overhead_panel_ui <- renderUI({
      if (!tables_ready()) {
        return(p(
          class = "text-muted",
          "Generate an entry table in the Set Up tab first."
        ))
      }
      tagList(
        tags$p(
          class = "small text-muted mb-2",
          bs_icon("info-circle"),
          " Click any value cell to edit. ",
          tags$strong("Rent"),
          " \u2014 facility costs; ",
          tags$strong("Payroll"),
          " \u2014 staff wages & benefits; ",
          tags$strong("EHR"),
          " \u2014 software & subscriptions; ",
          tags$strong("Malpractice"),
          " \u2014 insurance premiums; ",
          tags$strong("Supplies"),
          " \u2014 labs, consumables; ",
          tags$strong("Other"),
          " \u2014 miscellaneous."
        ),
        DT::dataTableOutput(ns("ovhd_tbl"))
      )
    })

    output$income_panel_ui <- renderUI({
      if (!tables_ready()) {
        return(p(
          class = "text-muted",
          "Generate an entry table in the Set Up tab first."
        ))
      }
      tagList(
        tags$p(
          class = "small text-muted mb-2",
          bs_icon("info-circle"),
          " Click any value cell to edit. ",
          tags$strong("Membership"),
          " \u2014 recurring monthly membership fees; ",
          tags$strong("FFS"),
          " \u2014 fee-for-service visits; ",
          tags$strong("Other Income"),
          " \u2014 grants, ancillary revenue."
        ),
        DT::dataTableOutput(ns("inc_tbl"))
      )
    })

    # Returned for the central output$main_nav_footer (app_server.R) to
    # render when Manual Entry is the active sub-path -- see mod_upload.R's
    # nav_footer reactive, which delegates here.
    nav_footer <- reactive({
      div(
        class = "tour-nav-footer justify-content-between",
        actionButton(
          ns("btn_back_manual"),
          tagList(bs_icon("arrow-left"), " Back"),
          class = "btn-outline-secondary"
        ),
        if (tables_ready()) {
          actionButton(
            ns("btn_submit_manual"),
            tagList(bs_icon("check2-circle"), " Submit Data"),
            class = "btn-success"
          )
        }
      )
    })

    output$ovhd_tbl <- DT::renderDT({
      req(manual_ovhd())
      DT::datatable(
        manual_ovhd(),
        rownames = FALSE,
        editable = list(target = "cell", disable = list(columns = c(0L, 7L))),
        selection = "none",
        options = list(
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          info = FALSE,
          dom = "t"
        )
      ) |>
        DT::formatCurrency(
          columns = c(
            "Rent",
            "Payroll",
            "EHR",
            "Malpractice",
            "Supplies",
            "Other",
            "Total"
          ),
          currency = "$",
          digits = 0
        )
    })

    output$inc_tbl <- DT::renderDT({
      req(manual_inc())
      DT::datatable(
        manual_inc(),
        rownames = FALSE,
        editable = list(target = "cell", disable = list(columns = c(0L, 4L))),
        selection = "none",
        options = list(
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          info = FALSE,
          dom = "t"
        )
      ) |>
        DT::formatCurrency(
          columns = c("Membership", "FFS", "Other Income", "Total"),
          currency = "$",
          digits = 0
        )
    })

    # -- Cell edit handlers ----------------------------------------------------
    # info$col is 0-indexed (JS); + 1L converts to R 1-indexed column position.
    # With rownames = FALSE: col 0 = Period, col 1 = first numeric, etc.

    observeEvent(input$ovhd_tbl_cell_edit, {
      info <- input$ovhd_tbl_cell_edit
      df <- manual_ovhd()
      df[info$row, info$col + 1L] <- DT::coerceValue(
        info$value,
        df[info$row, info$col + 1L]
      )
      df[["Total"]] <- rowSums(
        df[, c("Rent", "Payroll", "EHR", "Malpractice", "Supplies", "Other")],
        na.rm = TRUE
      )
      manual_ovhd(df)
      DT::replaceData(ovhd_proxy, df, resetPaging = FALSE, rownames = FALSE)
    })

    observeEvent(input$inc_tbl_cell_edit, {
      info <- input$inc_tbl_cell_edit
      df <- manual_inc()
      df[info$row, info$col + 1L] <- DT::coerceValue(
        info$value,
        df[info$row, info$col + 1L]
      )
      df[["Total"]] <- rowSums(
        df[, c("Membership", "FFS", "Other Income")],
        na.rm = TRUE
      )
      manual_inc(df)
      DT::replaceData(inc_proxy, df, resetPaging = FALSE, rownames = FALSE)
    })

    # -- Submit: assemble r$overhead_monthly / r$income_monthly ---------------
    observeEvent(input$btn_submit_manual, {
      meta <- manual_meta()
      ovhd <- manual_ovhd()
      inc <- manual_inc()
      req(meta, ovhd, inc)

      pid <- trimws(r$practice_id %||% "")

      if (meta$freq == "monthly") {
        yrs <- as.integer(format(meta$dates, "%Y"))
        mos <- as.integer(format(meta$dates, "%m"))

        r$overhead_monthly <- tibble::tibble(
          practice_id = pid,
          year = yrs,
          month = mos,
          total_overhead = ovhd[["Total"]],
          gross_overhead = ovhd[["Total"]],
          total_refunds = 0
        )
        r$income_monthly <- tibble::tibble(
          practice_id = pid,
          year = yrs,
          month = mos,
          total_revenue = inc[["Total"]]
        )
      } else {
        r$overhead_monthly <- tibble::tibble(
          practice_id = pid,
          week_start = meta$dates,
          total_overhead = ovhd[["Total"]],
          gross_overhead = ovhd[["Total"]],
          total_refunds = 0
        )
        r$income_monthly <- tibble::tibble(
          practice_id = pid,
          week_start = meta$dates,
          total_revenue = inc[["Total"]]
        )
      }

      # Signal non-upload, non-scenario workflow (0-row transactions tibble)
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
      r$overhead <- NULL
      r$income <- NULL
      r$validation <- list()

      showNotification(
        paste0(
          "\u2713 ",
          nrow(ovhd),
          " period",
          if (nrow(ovhd) != 1L) "s",
          " of data saved."
        ),
        type = "message",
        duration = 4
      )

      updateNavbarPage(
        parent_session %||% session,
        "main_nav",
        selected = "summary"
      )
    })

    # -- Return back-navigation signal to parent module ------------------------
    list(go_back = reactive(input$btn_back_manual), nav_footer = nav_footer)
  })
}
