#  Summary-tab helpers

# Explicit background/text color overrides for the Summary tab's plots,
# keyed off r$dark_mode. thematic::thematic_shiny() re-configures colors on
# toggle (see app_server.R), but its re-hook doesn't reliably reach a
# renderPlot() call made after the initial invocation -- confirmed
# empirically: the panel/plot background stays white and title/legend text
# stays theme_{minimal,void}()'s default dark grey/black in dark mode,
# invisible against the dark card behind it. Colors match the literal
# values already passed to thematic_shiny()'s bg/fg args, and (verified via
# computed styles) the actual rendered .card background/text color in each
# mode, so the plot blends into its card exactly as if thematic had worked.
.plot_theme_overrides <- function(is_dark) {
  bg <- if (is_dark) "#0F172A" else "#F8FAFC"
  fg <- if (is_dark) "#E2E8F0" else "#172033"
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = bg, colour = NA),
    panel.background = ggplot2::element_rect(fill = bg, colour = NA),
    legend.background = ggplot2::element_rect(fill = bg, colour = NA),
    legend.key = ggplot2::element_rect(fill = bg, colour = NA),
    text = ggplot2::element_text(colour = fg),
    axis.text = ggplot2::element_text(colour = fg),
    plot.title = ggplot2::element_text(colour = fg)
  )
}

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

# `overrides` is an optional named chr vector (slug/key -> custom label),
# typically r$category_labels / r$source_labels set via the "Customize
# Category Labels" editor in Review & Edit. Falls back to the built-in
# defaults, and ultimately to the raw key, when no override is present.
.pretty_cat <- function(x, overrides = NULL) {
  labs <- .cat_labels
  if (!is.null(overrides)) {
    labs[names(overrides)] <- overrides
  }
  ifelse(x %in% names(labs), labs[x], x)
}
# Map account_name to a display label; anything unrecognised is kept as-is.
.pretty_src <- function(x, overrides = NULL) {
  labs <- .src_labels
  if (!is.null(overrides)) {
    for (key in names(overrides)) {
      labs[[key]] <- overrides[[key]]
    }
  }
  ifelse(x %in% names(labs), labs[x], x)
}

# Colour palettes, keyed on the underlying slug/account_name (NOT the pretty
# label) so a custom display label still gets a stable, predictable color.
.cat_palette <- c(
  rent = "#1e3a5f",
  staff = "#4a90d9",
  supplies = "#2d6a4f",
  software = "#6baed6",
  insurance = "#e9a825",
  marketing = "#9ecae1",
  labs = "#c0392b",
  equipment = "#74c476",
  licenses = "#fd8d3c",
  education = "#756bb1",
  other = "#969696"
)

.src_palette <- c(
  "Membership Fees" = "#1e3a5f",
  "Fee-for-Service" = "#4a90d9"
)

.fallback_color <- "#969696"

# Build a `scale_fill_manual`-ready palette named by the *current* display
# labels (after overrides) for the given raw keys, so renaming a category or
# source doesn't break color matching.
.build_palette <- function(keys, base_palette, pretty_fn, overrides) {
  keys <- unique(keys)
  cols <- ifelse(
    keys %in% names(base_palette),
    base_palette[keys],
    .fallback_color
  )
  stats::setNames(cols, pretty_fn(keys, overrides))
}


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
#' @param parent_session The top-level Shiny session for cross-tab navigation.
#' @noRd
mod_summary_server <- function(id, r, parent_session = NULL) {
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

      tagList(
        layout_columns(
          col_widths = c(6, 6),

          #  Overhead card
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tagList(bs_icon("receipt"), " Overhead"),
              # Only show the toggles when transaction-level data is available
              if (!is.null(r$overhead) && nrow(r$overhead) > 0L) {
                tagList(
                  bslib::input_switch(
                    ns("ovhd_by_cat"),
                    "By category",
                    value = FALSE
                  ),
                  radioButtons(
                    ns("ovhd_chart_type"),
                    label = NULL,
                    choices = c("Bar" = "bar", "Pie" = "pie"),
                    selected = "bar",
                    inline = TRUE
                  ),
                  conditionalPanel(
                    condition = sprintf(
                      "input['%s'] == 'pie'",
                      ns("ovhd_chart_type")
                    ),
                    radioButtons(
                      ns("ovhd_pie_scope"),
                      label = NULL,
                      choices = c("Full data" = "full", "By period" = "period"),
                      selected = "full",
                      inline = TRUE
                    )
                  ),
                  conditionalPanel(
                    condition = sprintf(
                      "input['%s'] == 'pie' && input['%s'] == 'period'",
                      ns("ovhd_chart_type"),
                      ns("ovhd_pie_scope")
                    ),
                    uiOutput(ns("ovhd_pie_period_ui"), inline = TRUE)
                  )
                )
              }
            ),
            # Split into 3 card_body() sections, not 1: with everything in a
            # single fillable body, the plot's fixed height = "280px" never
            # changed in full_screen mode, so expanding the card just added
            # blank space around a still-tiny plot. min_height (a floor, not
            # a fixed size) on the plot's own body + height = "100%" lets it
            # actually grow to fill the much taller full_screen modal, while
            # fill = FALSE keeps the value boxes and table at their natural
            # size in both states.
            card_body(
              fill = FALSE,
              layout_column_wrap(
                width = "140px",
                fill = FALSE,
                uiOutput(ns("ovhd_vboxes"))
              )
            ),
            card_body(
              min_height = 280,
              plotOutput(ns("ovhd_plot"), height = "100%")
            ),
            card_body(
              fill = FALSE,
              max_height = 320,
              tags$hr(class = "mt-0"),
              uiOutput(ns("ovhd_table_caption")),
              DT::dataTableOutput(ns("ovhd_table"))
            )
          ),

          #  Income card
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tagList(bs_icon("cash-coin"), " Revenue"),
              # Only show the toggles when transaction-level data is available
              if (!is.null(r$income) && nrow(r$income) > 0L) {
                tagList(
                  bslib::input_switch(
                    ns("inc_by_src"),
                    "By source",
                    value = FALSE
                  ),
                  radioButtons(
                    ns("inc_chart_type"),
                    label = NULL,
                    choices = c("Bar" = "bar", "Pie" = "pie"),
                    selected = "bar",
                    inline = TRUE
                  ),
                  conditionalPanel(
                    condition = sprintf(
                      "input['%s'] == 'pie'",
                      ns("inc_chart_type")
                    ),
                    radioButtons(
                      ns("inc_pie_scope"),
                      label = NULL,
                      choices = c("Full data" = "full", "By period" = "period"),
                      selected = "full",
                      inline = TRUE
                    )
                  ),
                  conditionalPanel(
                    condition = sprintf(
                      "input['%s'] == 'pie' && input['%s'] == 'period'",
                      ns("inc_chart_type"),
                      ns("inc_pie_scope")
                    ),
                    uiOutput(ns("inc_pie_period_ui"), inline = TRUE)
                  )
                )
              }
            ),
            card_body(
              fill = FALSE,
              layout_column_wrap(
                width = "140px",
                fill = FALSE,
                uiOutput(ns("inc_vboxes"))
              )
            ),
            card_body(
              min_height = 280,
              plotOutput(ns("inc_plot"), height = "100%")
            ),
            card_body(
              fill = FALSE,
              max_height = 320,
              tags$hr(class = "mt-0"),
              uiOutput(ns("inc_table_caption")),
              DT::dataTableOutput(ns("inc_table"))
            )
          )
        ),
        div(
          class = "d-flex justify-content-between mt-3",
          actionButton(
            ns("btn_back_to_edit"),
            tagList(bs_icon("arrow-left"), " Back"),
            class = "btn-outline-secondary"
          ),
          actionButton(
            ns("btn_next_to_projections"),
            tagList(bs_icon("graph-up-arrow"), " Next: Projections"),
            class = "btn-primary"
          )
        )
      )
    })
    outputOptions(output, "content", suspendWhenHidden = FALSE)

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

    #  Period selectors for "By period" pie mode
    output$ovhd_pie_period_ui <- renderUI({
      req(ovhd_by_cat())
      periods <- sort(unique(ovhd_by_cat()$period_start), decreasing = TRUE)
      choices <- stats::setNames(as.character(periods), fmt_period(periods))
      selectInput(
        ns("ovhd_pie_period"),
        NULL,
        choices = choices,
        selected = isolate(input$ovhd_pie_period) %||% choices[1]
      )
    })

    output$inc_pie_period_ui <- renderUI({
      req(inc_by_src())
      periods <- sort(unique(inc_by_src()$period_start), decreasing = TRUE)
      choices <- stats::setNames(as.character(periods), fmt_period(periods))
      selectInput(
        ns("inc_pie_period"),
        NULL,
        choices = choices,
        selected = isolate(input$inc_pie_period) %||% choices[1]
      )
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

    #  Pie-chart helper: totals-by-group summed across the full range
    .pie_plot <- function(d, palette, title, is_dark = FALSE) {
      d <- d |>
        dplyr::group_by(label) |>
        dplyr::summarise(total = sum(total, na.rm = TRUE), .groups = "drop") |>
        dplyr::filter(total > 0) |>
        dplyr::arrange(dplyr::desc(label))
      d$pct <- d$total / sum(d$total)

      ggplot2::ggplot(d, ggplot2::aes(x = "", y = total, fill = label)) +
        ggplot2::geom_col(width = 1, colour = "white") +
        ggplot2::coord_polar(theta = "y") +
        ggplot2::scale_fill_manual(
          values = palette,
          name = NULL,
          drop = FALSE
        ) +
        ggplot2::geom_text(
          ggplot2::aes(label = scales::percent(pct, accuracy = 1)),
          position = ggplot2::position_stack(vjust = 0.5),
          size = 3.5,
          colour = "white",
          fontface = "bold"
        ) +
        ggplot2::labs(title = title) +
        # theme_void() blanks the panel/background entirely (by design, for
        # maps and similar) -- fine on its own, but combined with thematic's
        # re-hook not reliably reaching a post-initial renderPlot() (see
        # .plot_theme_overrides() above), the title/legend text it leaves
        # behind defaulted to ggplot2's built-in dark grey/black, invisible
        # against the dark card behind the (still-transparent) plot in dark
        # mode. .plot_theme_overrides() gives it a real, theme-matched
        # background plus readable text instead of relying on transparency.
        ggplot2::theme_void(base_size = 12) +
        ggplot2::theme(
          legend.position = "bottom",
          plot.title = ggplot2::element_text(
            size = 12,
            face = "bold",
            hjust = 0.5
          )
        ) +
        .plot_theme_overrides(is_dark)
    }

    #  Plots
    output$ovhd_plot <- renderPlot({
      r$dark_mode # dependency only -- forces a redraw when the theme toggles
      req(ovhd_overall())
      is_dark <- identical(r$dark_mode, "dark")
      by_cat <- isTRUE(input$ovhd_by_cat)
      is_pie <- by_cat && identical(input$ovhd_chart_type, "pie")
      has_cat_data <- !is.null(ovhd_by_cat()) && nrow(ovhd_by_cat()) > 0

      overrides <- r$category_labels
      pal <- .build_palette(
        ovhd_by_cat()$category,
        .cat_palette,
        .pretty_cat,
        overrides
      )

      if (is_pie && has_cat_data) {
        d <- ovhd_by_cat() |>
          dplyr::mutate(label = .pretty_cat(category, overrides))
        scope <- input$ovhd_pie_scope %||% "full"
        title <- "Overhead by category"
        if (identical(scope, "period")) {
          sel <- input$ovhd_pie_period
          if (!is.null(sel) && nzchar(sel)) {
            d <- dplyr::filter(d, period_start == as.Date(sel))
            title <- paste0(title, " — ", fmt_period(as.Date(sel)))
          }
        }
        return(.pie_plot(d, pal, title, is_dark))
      }

      if (by_cat && has_cat_data) {
        d <- ovhd_by_cat() |>
          dplyr::mutate(label = .pretty_cat(category, overrides))
        p <- ggplot2::ggplot(
          d,
          ggplot2::aes(period_start, total, fill = label)
        ) +
          ggplot2::geom_col(width = if (is_weekly()) 5 else 25) +
          ggplot2::scale_fill_manual(
            values = pal,
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
        ) +
        .plot_theme_overrides(is_dark)
    })

    output$inc_plot <- renderPlot({
      r$dark_mode # dependency only -- forces a redraw when the theme toggles
      req(inc_overall())
      is_dark <- identical(r$dark_mode, "dark")
      by_src <- isTRUE(input$inc_by_src)
      is_pie <- by_src && identical(input$inc_chart_type, "pie")
      has_src_data <- !is.null(inc_by_src()) && nrow(inc_by_src()) > 0

      overrides <- r$source_labels
      pal <- .build_palette(
        inc_by_src()$account_name,
        .src_palette,
        .pretty_src,
        overrides
      )

      if (is_pie && has_src_data) {
        d <- inc_by_src() |>
          dplyr::mutate(label = .pretty_src(account_name, overrides))
        scope <- input$inc_pie_scope %||% "full"
        title <- "Revenue by source"
        if (identical(scope, "period")) {
          sel <- input$inc_pie_period
          if (!is.null(sel) && nzchar(sel)) {
            d <- dplyr::filter(d, period_start == as.Date(sel))
            title <- paste0(title, " — ", fmt_period(as.Date(sel)))
          }
        }
        return(.pie_plot(d, pal, title, is_dark))
      }

      if (by_src && has_src_data) {
        d <- inc_by_src() |>
          dplyr::mutate(label = .pretty_src(account_name, overrides))
        p <- ggplot2::ggplot(
          d,
          ggplot2::aes(period_start, total, fill = label)
        ) +
          ggplot2::geom_col(width = if (is_weekly()) 5 else 25) +
          ggplot2::scale_fill_manual(
            values = pal,
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
        ) +
        .plot_theme_overrides(is_dark)
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

    # Aggregate `d` (columns: label, total) down to one row per label,
    # matching whatever slice of data the pie chart is currently showing
    # (the full range, or a single selected period).
    .grouped_totals_dt <- function(d, label_header) {
      tot_all <- sum(d$total, na.rm = TRUE)
      out <- d |>
        dplyr::group_by(label) |>
        dplyr::summarise(Total = sum(total, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(Total))
      out[["% of Total"]] <- if (tot_all != 0) {
        scales::percent(out$Total / tot_all, accuracy = 1)
      } else {
        "—"
      }
      out$Total <- fmt_dollar(out$Total)
      names(out)[names(out) == "label"] <- label_header
      .make_dt(out)
    }

    output$ovhd_table_caption <- renderUI({
      by_cat <- isTRUE(input$ovhd_by_cat)
      is_pie <- by_cat && identical(input$ovhd_chart_type, "pie")
      scope <- input$ovhd_pie_scope %||% "full"
      label <- if (is_pie && identical(scope, "period")) {
        sel <- input$ovhd_pie_period
        if (!is.null(sel) && nzchar(sel)) {
          paste0("Category totals — ", fmt_period(as.Date(sel)))
        } else {
          "Category totals"
        }
      } else if (is_pie) {
        "Category totals (full range)"
      } else {
        "Period detail"
      }
      tags$p(class = "small fw-semibold text-muted mb-1", label)
    })

    output$inc_table_caption <- renderUI({
      by_src <- isTRUE(input$inc_by_src)
      is_pie <- by_src && identical(input$inc_chart_type, "pie")
      scope <- input$inc_pie_scope %||% "full"
      label <- if (is_pie && identical(scope, "period")) {
        sel <- input$inc_pie_period
        if (!is.null(sel) && nzchar(sel)) {
          paste0("Source totals — ", fmt_period(as.Date(sel)))
        } else {
          "Source totals"
        }
      } else if (is_pie) {
        "Source totals (full range)"
      } else {
        "Period detail"
      }
      tags$p(class = "small fw-semibold text-muted mb-1", label)
    })

    output$ovhd_table <- DT::renderDataTable({
      req(ovhd_overall())
      by_cat <- isTRUE(input$ovhd_by_cat)

      if (by_cat && !is.null(ovhd_by_cat()) && nrow(ovhd_by_cat()) > 0) {
        is_pie <- identical(input$ovhd_chart_type, "pie")
        if (is_pie) {
          d <- ovhd_by_cat() |>
            dplyr::mutate(label = .pretty_cat(category, r$category_labels))
          if (identical(input$ovhd_pie_scope %||% "full", "period")) {
            sel <- input$ovhd_pie_period
            if (!is.null(sel) && nzchar(sel)) {
              d <- dplyr::filter(d, period_start == as.Date(sel))
            }
          }
          return(.grouped_totals_dt(d, "Category"))
        }

        # Bar mode -- wide format: period | Cat1 | Cat2 | ... | Total
        wide <- ovhd_by_cat() |>
          dplyr::mutate(label = .pretty_cat(category, r$category_labels)) |>
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
        is_pie <- identical(input$inc_chart_type, "pie")
        if (is_pie) {
          d <- inc_by_src() |>
            dplyr::mutate(label = .pretty_src(account_name, r$source_labels))
          if (identical(input$inc_pie_scope %||% "full", "period")) {
            sel <- input$inc_pie_period
            if (!is.null(sel) && nzchar(sel)) {
              d <- dplyr::filter(d, period_start == as.Date(sel))
            }
          }
          return(.grouped_totals_dt(d, "Source"))
        }

        wide <- inc_by_src() |>
          dplyr::mutate(label = .pretty_src(account_name, r$source_labels)) |>
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

    observeEvent(input$btn_back_to_edit, {
      updateNavbarPage(
        parent_session %||% session,
        "main_nav",
        selected = "edit"
      )
    })

    observeEvent(input$btn_next_to_projections, {
      updateNavbarPage(
        parent_session %||% session,
        "main_nav",
        selected = "projections"
      )
    })
  })
}
