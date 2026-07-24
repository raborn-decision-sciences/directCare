# -- Results-tab helpers -------------------------------------------------------

# interpret_*() functions return plain text with "\n\n" paragraph breaks
# (not HTML -- a deliberate directCarePlanR design choice since Typst, not
# a live UI, was its near-term consumer). Wrap each paragraph in <p> for
# display here; this is display formatting, an app-side concern.
.paragraphs_to_html <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(NULL)
  }
  paragraphs <- trimws(strsplit(text, "\n\n", fixed = TRUE)[[1]])
  paragraphs <- paragraphs[nzchar(paragraphs)]
  tagList(lapply(paragraphs, tags$p))
}

.fmt_dollar <- function(x) scales::dollar(x, accuracy = 1)

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
#'
#' @noRd
mod_results_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$content <- renderUI({
      if (is.null(r$projections)) {
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
          card(
            card_header(bsicons::bs_icon("geo-alt"), " Market Context"),
            card_body(
              tags$p(tags$strong("Location: "), r$market_context$geography$county_name, ", ", r$market_context$geography$state_abb),
              tags$p(tags$strong("Population: "), format(r$market_context$population_income$population, big.mark = ",")),
              tags$p(tags$strong("Median household income: "), .fmt_dollar(r$market_context$population_income$median_household_income)),
              tags$p(tags$strong("Uninsured rate: "), scales::percent(r$market_context$uninsured$uninsured_rate, accuracy = 0.1)),
              tags$p(tags$strong("Physician density: "), round(r$market_context$physician_density$physician_density_per_10k, 1), " per 10,000 population"),
              tags$p(tags$strong("Known nearby direct care practices: "), nrow(r$market_context$landscape))
            )
          ),
          card(
            full_screen = TRUE,
            card_header(bsicons::bs_icon("graph-up-arrow"), " Scenario Projections"),
            card_body(
              plotOutput(ns("projection_plot"), height = "320px")
            )
          )
        ),
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
        card(
          card_header(bsicons::bs_icon("lightbulb"), " Interpretation"),
          card_body(
            tags$h6("Revenue"),
            .paragraphs_to_html(r$interpretations$revenue),
            tags$h6(class = "mt-3", "Projections"),
            .paragraphs_to_html(r$interpretations$projection),
            tags$h6(class = "mt-3", "Capital"),
            .paragraphs_to_html(r$interpretations$capital)
          )
        ),
        div(
          class = "d-flex justify-content-end mt-3 mb-4",
          downloadButton(
            ns("dl_report"),
            tagList(bsicons::bs_icon("file-earmark-pdf"), " Download Report"),
            class = "btn-primary"
          )
        )
      )
    })

    output$projection_plot <- renderPlot({
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
        rownames = FALSE
      ) |>
        DT::formatCurrency(c("membership_revenue", "fee_revenue", "total_revenue"), digits = 0)
    })

    output$startup_table <- DT::renderDT({
      req(r$capital$startup_costs)
      items <- r$capital$startup_costs$line_items
      df <- data.frame(item = names(items), amount = as.numeric(items))
      DT::datatable(df, options = list(dom = "t", paging = FALSE), rownames = FALSE) |>
        DT::formatCurrency("amount", digits = 0)
    })

    output$dl_report <- downloadHandler(
      filename = function() {
        safe_name <- gsub("[^A-Za-z0-9_-]", "-", r$practice_name %||% "practice")
        paste0("plan-", safe_name, "-", format(Sys.Date(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        data <- directCarePlanR::build_report_data(
          market_context = r$market_context,
          revenue = r$revenue,
          projections = r$projections,
          capital = r$capital,
          interpretations = r$interpretations,
          practice_name = r$practice_name
        )
        directCarePlanR::render_plan_report(data, file)
      }
    )
  })
}
