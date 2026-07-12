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
          tags$div(
            class = "d-flex justify-content-between align-items-center mb-1",
            tags$h6(
              class = "fw-semibold mb-0",
              bs_icon("receipt"),
              " Monthly Overhead"
            ),
            bslib::input_switch(
              ns("ovhd_multi"),
              "Multiple sources",
              value = FALSE
            )
          ),
          uiOutput(ns("overhead_input_ui")),

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

          tags$h6(
            class = "fw-semibold mb-1",
            bs_icon("cash-coin"),
            " Additional Income"
          ),
          tags$p(
            class = "small text-muted mb-2",
            "Optional. Add fee-for-service, grants, or any other revenue ",
            "sources — break a broad source into multiple rows if you want ",
            "a finer split."
          ),
          uiOutput(ns("income_source_ui")),

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

    # -- Computed totals (shared helper needed by overhead UI below) ------------
    .nn <- function(x) {
      v <- x %||% 0
      if (length(v) != 1L || !is.finite(v)) 0 else v
    }

    # -- Dynamic overhead-source UI ----------------------------------------------
    n_ovhd_items <- reactiveVal(1L)

    observeEvent(input$btn_add_ovhd_item, {
      n_ovhd_items(n_ovhd_items() + 1L)
    })

    output$overhead_input_ui <- renderUI({
      if (!isTRUE(input$ovhd_multi)) {
        return(numericInput(
          ns("monthly_overhead"),
          "Total monthly overhead ($)",
          value = isolate(input$monthly_overhead) %||% 0,
          min = 0,
          step = 100
        ))
      }

      n <- n_ovhd_items()
      tagList(
        lapply(seq_len(n), function(i) {
          saved_label <- isolate(input[[paste0("ovhd_item_label_", i)]]) %||%
            ""
          saved_amount <- isolate(input[[paste0("ovhd_item_amount_", i)]]) %||%
            0
          div(
            class = "border rounded p-2 mb-2",
            tags$div(
              class = "d-flex justify-content-between align-items-center mb-1",
              tags$span(
                class = "small fw-semibold text-muted",
                paste("Source", i)
              ),
              if (i > 1L) {
                actionButton(
                  ns(paste0("btn_remove_ovhd_item_", i)),
                  bs_icon("x", title = paste("Remove source", i)),
                  class = "btn-outline-danger btn-sm py-0 px-1"
                )
              }
            ),
            layout_columns(
              col_widths = c(6, 6),
              textInput(
                ns(paste0("ovhd_item_label_", i)),
                "Label (optional)",
                value = saved_label,
                placeholder = "e.g. Rent"
              ),
              numericInput(
                ns(paste0("ovhd_item_amount_", i)),
                "Amount ($)",
                value = saved_amount,
                min = 0,
                step = 100
              )
            )
          )
        }),
        actionButton(
          ns("btn_add_ovhd_item"),
          tagList(bs_icon("plus-circle"), " Add Source"),
          class = "btn-outline-secondary btn-sm mt-1"
        )
      )
    })

    observe({
      n <- n_ovhd_items()
      lapply(seq_len(n), function(i) {
        if (i > 1L) {
          btn_id <- paste0("btn_remove_ovhd_item_", i)
          observeEvent(
            input[[btn_id]],
            {
              n_ovhd_items(max(1L, n_ovhd_items() - 1L))
            },
            ignoreInit = TRUE,
            once = TRUE
          )
        }
      })
    })

    total_overhead <- reactive({
      if (isTRUE(input$ovhd_multi)) {
        n <- n_ovhd_items()
        sum(vapply(
          seq_len(n),
          \(i) .nn(input[[paste0("ovhd_item_amount_", i)]]),
          numeric(1)
        ))
      } else {
        .nn(input$monthly_overhead)
      }
    })

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

    # -- Dynamic additional-income-source UI -------------------------------------
    n_income_items <- reactiveVal(2L)
    .income_defaults <- c("Fee-for-Service", "Other Income")

    observeEvent(input$btn_add_income_item, {
      n_income_items(n_income_items() + 1L)
    })

    output$income_source_ui <- renderUI({
      n <- n_income_items()
      if (n == 0L) {
        return(tagList(
          tags$p(
            class = "small text-muted mb-1",
            "No additional income sources."
          ),
          actionButton(
            ns("btn_add_income_item"),
            tagList(bs_icon("plus-circle"), " Add Source"),
            class = "btn-outline-secondary btn-sm mt-1"
          )
        ))
      }

      tagList(
        lapply(seq_len(n), function(i) {
          saved_label <- isolate(input[[paste0("inc_item_label_", i)]]) %||%
            (if (i <= length(.income_defaults)) .income_defaults[i] else "")
          saved_amount <- isolate(input[[paste0("inc_item_amount_", i)]]) %||%
            0
          div(
            class = "border rounded p-2 mb-2",
            tags$div(
              class = "d-flex justify-content-between align-items-center mb-1",
              tags$span(
                class = "small fw-semibold text-muted",
                paste("Source", i)
              ),
              actionButton(
                ns(paste0("btn_remove_income_item_", i)),
                bs_icon("x", title = paste("Remove source", i)),
                class = "btn-outline-danger btn-sm py-0 px-1"
              )
            ),
            layout_columns(
              col_widths = c(6, 6),
              textInput(
                ns(paste0("inc_item_label_", i)),
                "Label",
                value = saved_label,
                placeholder = "e.g. Lab markup"
              ),
              numericInput(
                ns(paste0("inc_item_amount_", i)),
                "Amount ($/mo)",
                value = saved_amount,
                min = 0,
                step = 50
              )
            )
          )
        }),
        actionButton(
          ns("btn_add_income_item"),
          tagList(bs_icon("plus-circle"), " Add Source"),
          class = "btn-outline-secondary btn-sm mt-1"
        )
      )
    })

    observe({
      n <- n_income_items()
      lapply(seq_len(n), function(i) {
        btn_id <- paste0("btn_remove_income_item_", i)
        observeEvent(
          input[[btn_id]],
          {
            n_income_items(max(0L, n_income_items() - 1L))
          },
          ignoreInit = TRUE,
          once = TRUE
        )
      })
    })

    other_income_total <- reactive({
      n <- n_income_items()
      if (n == 0L) {
        return(0)
      }
      sum(vapply(
        seq_len(n),
        \(i) .nn(input[[paste0("inc_item_amount_", i)]]),
        numeric(1)
      ))
    })

    # -- Computed totals --------------------------------------------------------
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
      ovhd <- total_overhead()
      other_inc <- other_income_total()
      rev <- tier_total_revenue() + other_inc
      net <- rev - ovhd
      target <- .nn(input$target_income)
      members <- tier_total_members()
      avg_fee <- avg_fee_per_member()

      # Other income already covers part of overhead/target, so only the
      # remainder needs to come from membership dues.
      net_needed_breakeven <- max(0, ovhd - other_inc)
      net_needed_target <- max(0, ovhd + target - other_inc)

      # Scenarios
      members_for_breakeven <- if (avg_fee > 0) {
        ceiling(net_needed_breakeven / avg_fee)
      } else {
        NA_integer_
      }
      members_for_target <- if (avg_fee > 0) {
        ceiling(net_needed_target / avg_fee)
      } else {
        NA_integer_
      }
      fee_for_breakeven <- if (members > 0) {
        net_needed_breakeven / members
      } else {
        NA_real_
      }
      fee_for_target <- if (members > 0) {
        net_needed_target / members
      } else {
        NA_real_
      }

      tagList(
        # -- KPI row ----------------------------------------------------------
        layout_column_wrap(
          width = "200px",
          fill = FALSE,
          value_box(
            "Total revenue",
            fmt_dollar(rev),
            theme = "primary",
            height = "90px"
          ),
          value_box(
            "Other income",
            fmt_dollar(other_inc),
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
        if (other_inc > 0) {
          tags$p(
            class = "small text-muted mb-2 fst-italic",
            bs_icon("info-circle"),
            paste0(
              " ",
              fmt_dollar(other_inc),
              "/mo in other income is already applied to overhead below — ",
              "the scenarios show what membership dues need to cover the rest."
            )
          )
        },
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
                    tagList(
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
                    tagList(
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
                        tagList(bs_icon("check-circle"), " Target already met!")
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
                        tagList(
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
