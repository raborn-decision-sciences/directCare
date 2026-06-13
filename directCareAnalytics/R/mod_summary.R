#  Summary-tab helpers

# Pretty labels for overhead categories and income sources
.cat_labels <- c(
  rent = "Rent",
  staff = "Staff / Payroll",
  supplies = "Supplies",
  software = "Software",
  insurance = "Insurance",
  marketing = "Marketing",
  labs = "Labs",
  equipment = "Equipment",
  licenses = "Licenses",
  education = "Education",
  other = "Other"
)

# Income source: keyed on account_name (consistent for both CSV-imported and
# manually-entered income). CSV rows have source = "gnucash_csv" for all rows,
# so grouping by source would collapse everything into one bucket.
# account_name values: "Membership Fees", "Fee-for-Service", and anything else.
.src_labels <- c(
  "Membership Fees" = "Membership",
  "Fee-for-Service" = "Fee-for-Service"
)

.pretty_cat <- function(x) ifelse(x %in% names(.cat_labels), .cat_labels[x], x)
# Map account_name to a display label; anything unrecognised is kept as-is.
.pretty_src <- function(x) ifelse(x %in% names(.src_labels), .src_labels[x], x)

# Colour palettes (one per category / source)
.cat_palette <- c(
  "Rent" = "#1e3a5f",
  "Staff / Payroll" = "#4a90d9",
  "Supplies" = "#2d6a4f",
  "Software" = "#6baed6",
  "Insurance" = "#e9a825",
  "Marketing" = "#9ecae1",
  "Labs" = "#c0392b",
  "Equipment" = "#74c476",
  "Licenses" = "#fd8d3c",
  "Education" = "#756bb1",
  "Other" = "#969696"
)

# Keyed on the pretty labels produced by .pretty_src()
.src_palette <- c(
  "Membership" = "#1e3a5f",
  "Fee-for-Service" = "#4a90d9",
  "Other" = "#969696"
)


#' Summary Module UI
#'
#' Tab 3 -- side-by-side overhead and revenue summaries with optional
#' subcategory breakdowns.
#'
#' @param id Module namespace ID.
#' @noRd
mod_summary_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("content"))
}


#' Summary Module Server
#'
#' @param id Module namespace ID.
#' @param r Shared `reactiveValues` object from `app_server`.
#' @noRd
mod_summary_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #  Gate
    output$content <- renderUI({
      has_data <- !is.null(r$overhead_monthly) && nrow(r$overhead_monthly) > 0
      if (!has_data) {
        return(
          card(
            card_body(
              class = "text-center text-muted py-5",
              bs_icon("arrow-left-circle", size = "2em"),
              p(
                "Upload a CSV or add period summaries in the Review & Edit tab to view summaries."
              )
            )
          )
        )
      }

      layout_columns(
        col_widths = c(6, 6),

        #  Overhead card
        card(
          full_screen = TRUE,
          card_header(
            class = "d-flex justify-content-between align-items-center",
            tagList(bs_icon("receipt"), " Overhead"),
            # Only show the toggle when transaction-level data is available
            if (!is.null(r$overhead) && nrow(r$overhead) > 0L) {
              bslib::input_switch(
                ns("ovhd_by_cat"),
                "By category",
                value = FALSE
              )
            }
          ),
          card_body(
            layout_column_wrap(
              width = "140px",
              fill = FALSE,
              uiOutput(ns("ovhd_vboxes"))
            ),
            plotOutput(ns("ovhd_plot"), height = "280px"),
            tags$hr(),
            tags$p(
              class = "small fw-semibold text-muted mb-1",
              "Period detail"
            ),
            DT::dataTableOutput(ns("ovhd_table"))
          )
        ),

        #  Income card
        card(
          full_screen = TRUE,
          card_header(
            class = "d-flex justify-content-between align-items-center",
            tagList(bs_icon("cash-coin"), " Revenue"),
            # Only show the toggle when transaction-level data is available
            if (!is.null(r$income) && nrow(r$income) > 0L) {
              bslib::input_switch(ns("inc_by_src"), "By source", value = FALSE)
            }
          ),
          card_body(
            layout_column_wrap(
              width = "140px",
              fill = FALSE,
              uiOutput(ns("inc_vboxes"))
            ),
            plotOutput(ns("inc_plot"), height = "280px"),
            tags$hr(),
            tags$p(
              class = "small fw-semibold text-muted mb-1",
              "Period detail"
            ),
            DT::dataTableOutput(ns("inc_table"))
          )
        )
      )
    })

    #  Helpers: period column & frequency
    is_weekly <- reactive({
      req(r$overhead_monthly)
      "week_start" %in% names(r$overhead_monthly)
    })

    freq_label <- reactive(if (is_weekly()) "week" else "month")

    # Date range covered by r$overhead_monthly (used to filter raw transactions)
    active_range <- reactive({
      req(r$overhead_monthly)
      om <- .make_period_start(r$overhead_monthly)
      if (is_weekly()) {
        list(lo = min(om$period_start), hi = max(om$period_start) + 6L)
      } else {
        lo <- min(om$period_start)
        # last day of the latest month
        hi_month <- max(om$period_start)
        hi <- seq(hi_month, by = "month", length.out = 2L)[2L] - 1L
        list(lo = lo, hi = hi)
      }
    })

    # Format a period_start date as a short label
    fmt_period <- function(d) {
      if (is_weekly()) format(d, "%b %d '%y") else format(d, "%b %Y")
    }

    #  Overall period summaries
    ovhd_overall <- reactive({
      req(r$overhead_monthly)
      .make_period_start(r$overhead_monthly) |>
        dplyr::arrange(period_start) |>
        dplyr::select(period_start, total = total_overhead)
    })

    inc_overall <- reactive({
      req(r$income_monthly)
      if (nrow(r$income_monthly) == 0L) {
        return(NULL)
      }
      .make_period_start(r$income_monthly) |>
        dplyr::arrange(period_start) |>
        dplyr::select(period_start, total = total_revenue)
    })

    #  Subcategory period summaries
    ovhd_by_cat <- reactive({
      req(r$overhead, active_range())
      ar <- active_range()
      raw <- dplyr::filter(r$overhead, date >= ar$lo, date <= ar$hi)
      if (nrow(raw) == 0L) {
        return(NULL)
      }

      if (is_weekly()) {
        raw |>
          dplyr::group_by(period_start = week_start, category) |>
          dplyr::summarise(total = sum(amount), .groups = "drop")
      } else {
        raw |>
          dplyr::mutate(
            period_start = as.Date(paste(
              year,
              sprintf("%02d", month),
              "01",
              sep = "-"
            ))
          ) |>
          dplyr::group_by(period_start, category) |>
          dplyr::summarise(total = sum(amount), .groups = "drop")
      }
    })

    inc_by_src <- reactive({
      req(r$income, active_range())
      ar <- active_range()
      raw <- dplyr::filter(r$income, date >= ar$lo, date <= ar$hi)
      if (nrow(raw) == 0L) {
        return(NULL)
      }

      # Group by account_name, not source. CSV-imported income has
      # source = "gnucash_csv" for every row, so account_name is the only
      # reliable subcategory key for both import paths.
      if (is_weekly()) {
        raw |>
          dplyr::group_by(period_start = week_start, account_name) |>
          dplyr::summarise(total = sum(revenue), .groups = "drop")
      } else {
        raw |>
          dplyr::mutate(
            period_start = as.Date(paste(
              year,
              sprintf("%02d", month),
              "01",
              sep = "-"
            ))
          ) |>
          dplyr::group_by(period_start, account_name) |>
          dplyr::summarise(total = sum(revenue), .groups = "drop")
      }
    })

    #  Value boxes
    output$ovhd_vboxes <- renderUI({
      req(ovhd_overall())
      d <- ovhd_overall()
      n <- nrow(d)
      tot <- sum(d$total, na.rm = TRUE)
      avg <- if (n > 0) tot / n else 0
      tagList(
        value_box(
          title = "Total overhead",
          value = fmt_dollar(tot),
          theme = "secondary"
        ),
        value_box(
          title = paste("Avg per", freq_label()),
          value = fmt_dollar(avg),
          theme = "secondary"
        ),
        value_box(
          title = paste0(tools::toTitleCase(freq_label()), "s with data"),
          value = n,
          theme = "secondary"
        )
      )
    })

    output$inc_vboxes <- renderUI({
      if (is.null(inc_overall())) {
        return(p(
          class = "text-muted",
          bs_icon("info-circle"),
          " No income data found in this file."
        ))
      }
      d <- inc_overall()
      n <- nrow(d)
      tot <- sum(d$total, na.rm = TRUE)
      avg <- if (n > 0) tot / n else 0
      tagList(
        value_box(
          title = "Total revenue",
          value = fmt_dollar(tot),
          theme = "primary"
        ),
        value_box(
          title = paste("Avg per", freq_label()),
          value = fmt_dollar(avg),
          theme = "primary"
        ),
        value_box(
          title = paste0(tools::toTitleCase(freq_label()), "s with data"),
          value = n,
          theme = "primary"
        )
      )
    })

    #  Plots
    output$ovhd_plot <- renderPlot({
      req(ovhd_overall())
      by_cat <- isTRUE(input$ovhd_by_cat)

      if (by_cat && !is.null(ovhd_by_cat()) && nrow(ovhd_by_cat()) > 0) {
        d <- ovhd_by_cat() |>
          dplyr::mutate(label = .pretty_cat(category))
        p <- ggplot2::ggplot(
          d,
          ggplot2::aes(period_start, total, fill = label)
        ) +
          ggplot2::geom_col(width = if (is_weekly()) 5 else 25) +
          ggplot2::scale_fill_manual(
            values = .cat_palette,
            name = NULL,
            drop = FALSE
          ) +
          ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2)) +
          ggplot2::labs(
            x = NULL,
            y = "Overhead ($)",
            title = "Overhead by category"
          )
      } else {
        d <- ovhd_overall()
        p <- ggplot2::ggplot(d, ggplot2::aes(period_start, total)) +
          ggplot2::geom_col(
            fill = "#1e3a5f",
            width = if (is_weekly()) 5 else 25
          ) +
          ggplot2::geom_smooth(
            method = "lm",
            se = FALSE,
            colour = "#e9a825",
            linewidth = 0.8,
            formula = y ~ x
          ) +
          ggplot2::labs(
            x = NULL,
            y = "Overhead ($)",
            title = "Total overhead per period"
          )
      }

      p +
        ggplot2::scale_y_continuous(labels = fmt_dollar_format()) +
        ggplot2::scale_x_date(
          date_labels = if (is_weekly()) "%b '%y" else "%b\n%Y"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          legend.position = "bottom",
          plot.title = ggplot2::element_text(size = 12, face = "bold")
        )
    })

    output$inc_plot <- renderPlot({
      req(inc_overall())
      by_src <- isTRUE(input$inc_by_src)

      if (by_src && !is.null(inc_by_src()) && nrow(inc_by_src()) > 0) {
        d <- inc_by_src() |>
          dplyr::mutate(label = .pretty_src(account_name))
        p <- ggplot2::ggplot(
          d,
          ggplot2::aes(period_start, total, fill = label)
        ) +
          ggplot2::geom_col(width = if (is_weekly()) 5 else 25) +
          ggplot2::scale_fill_manual(
            values = .src_palette,
            name = NULL,
            drop = FALSE
          ) +
          ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1)) +
          ggplot2::labs(
            x = NULL,
            y = "Revenue ($)",
            title = "Revenue by source"
          )
      } else {
        d <- inc_overall()
        p <- ggplot2::ggplot(d, ggplot2::aes(period_start, total)) +
          ggplot2::geom_col(
            fill = "#4a90d9",
            width = if (is_weekly()) 5 else 25
          ) +
          ggplot2::geom_smooth(
            method = "lm",
            se = FALSE,
            colour = "#2d6a4f",
            linewidth = 0.8,
            formula = y ~ x
          ) +
          ggplot2::labs(
            x = NULL,
            y = "Revenue ($)",
            title = "Total revenue per period"
          )
      }

      p +
        ggplot2::scale_y_continuous(labels = fmt_dollar_format()) +
        ggplot2::scale_x_date(
          date_labels = if (is_weekly()) "%b '%y" else "%b\n%Y"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          legend.position = "bottom",
          plot.title = ggplot2::element_text(size = 12, face = "bold")
        )
    })

    #  Tables
    # Build a DT with period labels and dollar-formatted columns
    .make_dt <- function(df) {
      DT::datatable(
        df,
        rownames = FALSE,
        selection = "none",
        class = "compact stripe",
        options = list(
          pageLength = 12,
          dom = "tp", # table + pagination only
          scrollX = TRUE,
          columnDefs = list(list(className = "dt-right", targets = "_all"))
        )
      )
    }

    output$ovhd_table <- DT::renderDataTable({
      req(ovhd_overall())
      by_cat <- isTRUE(input$ovhd_by_cat)

      if (by_cat && !is.null(ovhd_by_cat()) && nrow(ovhd_by_cat()) > 0) {
        # Wide format: period | Cat1 | Cat2 | ... | Total
        wide <- ovhd_by_cat() |>
          dplyr::mutate(label = .pretty_cat(category)) |>
          dplyr::select(period_start, label, total) |>
          tidyr::pivot_wider(
            names_from = label,
            values_from = total,
            values_fill = 0
          ) |>
          dplyr::arrange(period_start) |>
          dplyr::mutate(
            Period = fmt_period(period_start),
            Total = rowSums(dplyr::across(where(is.numeric)), na.rm = TRUE),
            .keep = "unused"
          ) |>
          dplyr::relocate(Period) |>
          dplyr::mutate(dplyr::across(where(is.numeric), \(x) fmt_dollar(x)))
        .make_dt(wide)
      } else {
        # Overall: period | Total | Change
        d <- ovhd_overall() |>
          dplyr::mutate(
            Period = fmt_period(period_start),
            Total = fmt_dollar(total),
            Change = dplyr::case_when(
              is.na(dplyr::lag(total)) ~ "--",
              TRUE ~ fmt_dollar(
                total - dplyr::lag(total),
                style_negative = "parens"
              )
            ),
            .keep = "unused"
          )
        .make_dt(d)
      }
    })

    output$inc_table <- DT::renderDataTable({
      req(inc_overall())
      by_src <- isTRUE(input$inc_by_src)

      if (by_src && !is.null(inc_by_src()) && nrow(inc_by_src()) > 0) {
        wide <- inc_by_src() |>
          dplyr::mutate(label = .pretty_src(account_name)) |>
          dplyr::select(period_start, label, total) |>
          tidyr::pivot_wider(
            names_from = label,
            values_from = total,
            values_fill = 0
          ) |>
          dplyr::arrange(period_start) |>
          dplyr::mutate(
            Period = fmt_period(period_start),
            Total = rowSums(dplyr::across(where(is.numeric)), na.rm = TRUE),
            .keep = "unused"
          ) |>
          dplyr::relocate(Period) |>
          dplyr::mutate(dplyr::across(where(is.numeric), \(x) fmt_dollar(x)))
        .make_dt(wide)
      } else {
        d <- inc_overall() |>
          dplyr::mutate(
            Period = fmt_period(period_start),
            Total = fmt_dollar(total),
            Change = dplyr::case_when(
              is.na(dplyr::lag(total)) ~ "--",
              TRUE ~ fmt_dollar(
                total - dplyr::lag(total),
                style_negative = "parens"
              )
            ),
            .keep = "unused"
          )
        .make_dt(d)
      }
    })
  })
}
