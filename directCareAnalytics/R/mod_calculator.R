#' Quick Calculator Module UI
#'
#' A simple revenue estimator that takes monthly overhead, a membership panel,
#' and a target income and returns break-even and target-achievement scenarios
#' without running a full forecast model.
#'
#' @param id Module namespace ID.
#' @noRd
mod_calculator_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      col_widths = c(5, 7),

      # -- Left: inputs -------------------------------------------------------
      card(
        card_header(
          class = "d-flex align-items-center gap-2",
          bs_icon("calculator"),
          " Quick Calculator",
          tooltip(
            bs_icon("info-circle", title = "About Quick Calculator"),
            paste0(
              "Enter your monthly overhead, membership details, and income ",
              "target. The calculator instantly shows your current net position ",
              "and what it takes to reach your goal — no forecast model needed."
            )
          )
        ),
        card_body(
          tags$h6(
            class = "fw-semibold mb-3",
            bs_icon("receipt"),
            " Monthly Overhead"
          ),
          numericInput(
            ns("monthly_overhead"),
            "Total monthly overhead ($)",
            value = 0,
            min = 0,
            step = 100
          ),

          tags$hr(),

          tags$h6(
            class = "fw-semibold mb-1",
            bs_icon("people"),
            " Membership Panel"
          ),
          tags$p(
            class = "small text-muted mb-2",
            "Add one or more membership tiers below. Click “Add Tier” to ",
            "include additional age groups or plan levels."
          ),
          uiOutput(ns("tier_ui")),
          actionButton(
            ns("btn_add_tier"),
            tagList(bs_icon("plus-circle"), " Add Tier"),
            class = "btn-outline-secondary btn-sm mt-1"
          ),

          tags$hr(),

          numericInput(
            ns("target_income"),
            tagList(
              "Monthly net income target ($)",
              tooltip(
                bs_icon("info-circle", title = "Income target"),
                "The net take-home income you want each month after overhead."
              )
            ),
            value = 5000,
            min = 0,
            step = 500
          )
        )
      ),

      # -- Right: results -----------------------------------------------------
      card(
        card_header(
          class = "d-flex align-items-center gap-2",
          bs_icon("graph-up"),
          " Results"
        ),
        card_body(
          uiOutput(ns("results_ui"))
        )
      )
    ),

    div(
      class = "d-flex justify-content-start mt-3",
      actionButton(
        ns("btn_back"),
        tagList(bs_icon("arrow-left"), " Back"),
        class = "btn-outline-secondary"
      )
    )
  )
}


#' Quick Calculator Module Server
#'
#' @param id Module namespace ID.
#' @param r Shared `reactiveValues` object from `app_server`.
#' @param parent_session The top-level Shiny session for cross-tab navigation.
#' @noRd
mod_calculator_server <- function(id, r, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Dynamic tier UI --------------------------------------------------------
    n_tiers <- reactiveVal(1L)

    observeEvent(input$btn_add_tier, {
      n_tiers(n_tiers() + 1L)
    })

    output$tier_ui <- renderUI({
      n <- n_tiers()
      lapply(seq_len(n), function(i) {
        saved_label <- isolate(input[[paste0("tier_label_", i)]]) %||% ""
        saved_members <- isolate(input[[paste0("tier_members_", i)]]) %||% 0
        saved_fee <- isolate(input[[paste0("tier_fee_", i)]]) %||% 0
        div(
          class = "border rounded p-2 mb-2",
          if (n > 1L) {
            tags$div(
              class = "d-flex justify-content-between align-items-center mb-1",
              tags$span(
                class = "small fw-semibold text-muted",
                paste("Tier", i)
              ),
              if (i > 1L) {
                actionButton(
                  ns(paste0("btn_remove_tier_", i)),
                  bs_icon("x", title = paste("Remove tier", i)),
                  class = "btn-outline-danger btn-sm py-0 px-1"
                )
              }
            )
          },
          layout_columns(
            col_widths = c(4, 4, 4),
            textInput(
              ns(paste0("tier_label_", i)),
              if (i == 1L && n == 1L) "Label (optional)" else "Label",
              value = saved_label,
              placeholder = "e.g. Adult"
            ),
            numericInput(
              ns(paste0("tier_members_", i)),
              "Members",
              value = saved_members,
              min = 0,
              step = 1
            ),
            numericInput(
              ns(paste0("tier_fee_", i)),
              "Monthly fee ($)",
              value = saved_fee,
              min = 0,
              step = 5
            )
          )
        )
      })
    })

    # Remove tier i: shift inputs down (simplest: just decrement n_tiers when
    # last tier is removed; for middle tiers we reset n_tiers to force re-render)
    observe({
      n <- n_tiers()
      lapply(seq_len(n), function(i) {
        if (i > 1L) {
          btn_id <- paste0("btn_remove_tier_", i)
          observeEvent(
            input[[btn_id]],
            {
              n_tiers(max(1L, n_tiers() - 1L))
            },
            ignoreInit = TRUE,
            once = TRUE
          )
        }
      })
    })

    # -- Computed totals --------------------------------------------------------
    .nn <- function(x) {
      v <- x %||% 0
      if (length(v) != 1L || !is.finite(v)) 0 else v
    }

    tier_total_members <- reactive({
      n <- n_tiers()
      sum(vapply(
        seq_len(n),
        \(i) .nn(input[[paste0("tier_members_", i)]]),
        numeric(1)
      ))
    })

    tier_total_revenue <- reactive({
      n <- n_tiers()
      sum(vapply(
        seq_len(n),
        function(i) {
          .nn(input[[paste0("tier_members_", i)]]) *
            .nn(input[[paste0("tier_fee_", i)]])
        },
        numeric(1)
      ))
    })

    avg_fee_per_member <- reactive({
      m <- tier_total_members()
      if (m > 0) tier_total_revenue() / m else 0
    })

    # -- Results UI -------------------------------------------------------------
    output$results_ui <- renderUI({
      ovhd <- .nn(input$monthly_overhead)
      rev <- tier_total_revenue()
      net <- rev - ovhd
      target <- .nn(input$target_income)
      members <- tier_total_members()
      avg_fee <- avg_fee_per_member()

      # Scenarios
      members_for_breakeven <- if (avg_fee > 0) {
        ceiling(ovhd / avg_fee)
      } else {
        NA_integer_
      }
      members_for_target <- if (avg_fee > 0) {
        ceiling((ovhd + target) / avg_fee)
      } else {
        NA_integer_
      }
      fee_for_breakeven <- if (members > 0) {
        ovhd / members
      } else {
        NA_real_
      }
      fee_for_target <- if (members > 0) {
        (ovhd + target) / members
      } else {
        NA_real_
      }

      tagList(
        # -- KPI row ----------------------------------------------------------
        layout_column_wrap(
          width = "200px",
          fill = FALSE,
          value_box(
            "Membership revenue",
            fmt_dollar(rev),
            theme = "primary",
            height = "90px"
          ),
          value_box(
            "Monthly overhead",
            fmt_dollar(ovhd),
            theme = "secondary",
            height = "90px"
          ),
          value_box(
            "Net surplus / deficit",
            fmt_dollar(net),
            theme = if (net >= 0) "success" else "warning",
            height = "90px"
          ),
          value_box(
            "Panel size",
            if (members > 0) members else "—",
            theme = "info",
            height = "90px"
          )
        ),

        tags$hr(),

        # -- Break-even scenarios ---------------------------------------------
        tags$h6(
          class = "fw-semibold mb-2 mt-3",
          bs_icon("bullseye"),
          " Break-even Scenarios"
        ),
        tags$p(
          class = "small text-muted mb-2",
          "Two ways to reach break-even (revenue = overhead):"
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            class = "border-primary",
            card_body(
              class = "p-3",
              tags$p(
                class = "small text-muted mb-1",
                bs_icon("people-fill"),
                " Members needed (at current average fee)"
              ),
              tags$p(
                class = "fs-4 fw-bold mb-0 text-primary",
                if (!is.na(members_for_breakeven)) {
                  members_for_breakeven
                } else {
                  "—"
                }
              ),
              if (!is.na(members_for_breakeven) && members > 0) {
                tags$p(
                  class = "small mb-0",
                  style = if (members >= members_for_breakeven) {
                    "color:#16A34A;"
                  } else {
                    "color:#DC2626;"
                  },
                  if (members >= members_for_breakeven) {
                    paste0(
                      bs_icon("check-circle"),
                      " Already there! (",
                      members,
                      " current)"
                    )
                  } else {
                    paste0(
                      members_for_breakeven - members,
                      " more needed"
                    )
                  }
                )
              }
            )
          ),
          card(
            class = "border-secondary",
            card_body(
              class = "p-3",
              tags$p(
                class = "small text-muted mb-1",
                bs_icon("cash"),
                " Fee per member needed (at current panel)"
              ),
              tags$p(
                class = "fs-4 fw-bold mb-0",
                if (!is.na(fee_for_breakeven)) {
                  fmt_dollar(fee_for_breakeven)
                } else {
                  "—"
                }
              ),
              if (!is.na(fee_for_breakeven) && avg_fee > 0) {
                tags$p(
                  class = "small mb-0",
                  style = if (avg_fee >= fee_for_breakeven) {
                    "color:#16A34A;"
                  } else {
                    "color:#DC2626;"
                  },
                  if (avg_fee >= fee_for_breakeven) {
                    paste0(
                      bs_icon("check-circle"),
                      " Current avg fee covers it"
                    )
                  } else {
                    paste0(
                      "Increase avg fee by ",
                      fmt_dollar(fee_for_breakeven - avg_fee)
                    )
                  }
                )
              }
            )
          )
        ),

        # -- Income target scenarios ------------------------------------------
        if (target > 0) {
          tagList(
            tags$h6(
              class = "fw-semibold mb-2 mt-3",
              bs_icon("flag"),
              paste0(" Scenarios to reach ", fmt_dollar(target), "/mo target")
            ),
            layout_columns(
              col_widths = c(6, 6),
              card(
                class = "border-primary",
                card_body(
                  class = "p-3",
                  tags$p(
                    class = "small text-muted mb-1",
                    bs_icon("people-fill"),
                    " Members needed (at current avg fee)"
                  ),
                  tags$p(
                    class = "fs-4 fw-bold mb-0 text-primary",
                    if (!is.na(members_for_target)) {
                      members_for_target
                    } else {
                      "—"
                    }
                  ),
                  if (!is.na(members_for_target) && members > 0) {
                    tags$p(
                      class = "small mb-0",
                      style = if (members >= members_for_target) {
                        "color:#16A34A;"
                      } else {
                        "color:#DC2626;"
                      },
                      if (members >= members_for_target) {
                        paste0(bs_icon("check-circle"), " Target already met!")
                      } else {
                        paste0(members_for_target - members, " more needed")
                      }
                    )
                  }
                )
              ),
              card(
                class = "border-secondary",
                card_body(
                  class = "p-3",
                  tags$p(
                    class = "small text-muted mb-1",
                    bs_icon("cash"),
                    " Fee per member needed (at current panel)"
                  ),
                  tags$p(
                    class = "fs-4 fw-bold mb-0",
                    if (!is.na(fee_for_target)) {
                      fmt_dollar(fee_for_target)
                    } else {
                      "—"
                    }
                  ),
                  if (!is.na(fee_for_target) && avg_fee > 0) {
                    tags$p(
                      class = "small mb-0",
                      style = if (avg_fee >= fee_for_target) {
                        "color:#16A34A;"
                      } else {
                        "color:#DC2626;"
                      },
                      if (avg_fee >= fee_for_target) {
                        paste0(
                          bs_icon("check-circle"),
                          " Current avg fee reaches target"
                        )
                      } else {
                        paste0(
                          "Increase avg fee by ",
                          fmt_dollar(fee_for_target - avg_fee)
                        )
                      }
                    )
                  }
                )
              )
            )
          )
        }
      )
    })

    # -- Back button ------------------------------------------------------------
    go_back <- reactiveVal(NULL)

    observeEvent(input$btn_back, {
      go_back(runif(1))
    })

    return(list(go_back = go_back))
  })
}
