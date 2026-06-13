#' Projections Module UI
#'
#' Tab 3 -- Sub-tabs for Break-even, Revenue Forecast, and Income Target.
#' Shared options sidebar controls method, horizon, and confidence level.
#' Each sub-tab shows value boxes, a forecast plot, interpretation text,
#' and a download button for the branded report.
#'
#' @param id Module namespace ID.
#' @noRd
mod_projections_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("content"))
}


#' Projections Module Server
#'
#' @param id Module namespace ID.
#' @param r Shared `reactiveValues` object from `app_server`.
#' @noRd
mod_projections_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Gate: require uploaded data ------------------------------------------
    output$content <- renderUI({
      has_data <- !is.null(r$overhead_monthly) && nrow(r$overhead_monthly) > 0
      if (!has_data) {
        return(
          card(
            card_body(
              class = "text-center text-muted py-5",
              bs_icon("arrow-left-circle", size = "2em"),
              p(
                "Upload a CSV or add period summaries in the Review & Edit tab to generate projections."
              )
            )
          )
        )
      }

      page_sidebar(
        sidebar = sidebar(
          title = "Forecast Options",
          # -- Shared controls --------------------------------------------
          selectInput(
            ns("method"),
            "Method",
            choices = c(
              "Linear regression" = "linear",
              "ETS (exponential smoothing)" = "ets",
              "ARIMA" = "arima"
            ),
            selected = "linear"
          ),
          uiOutput(ns("method_hint")),
          sliderInput(
            ns("horizon"),
            if (is_weekly()) {
              "Forecast horizon (weeks)"
            } else {
              "Forecast horizon (months)"
            },
            min = 4,
            max = if (is_weekly()) 84 else 36,
            value = if (is_weekly()) 52 else 12,
            step = 4
          ),
          sliderInput(
            ns("confidence"),
            "Confidence level",
            min = 0.80,
            max = 0.99,
            value = 0.95,
            step = 0.01
          ),
          hr(),
          # -- Growth-rate / overhead-model assumptions ------------------
          tags$p(
            class = "small fw-semibold mb-0",
            bs_icon("graph-up-arrow"),
            " Assumptions"
          ),
          tags$p(
            class = "small text-muted",
            "Applied on top of the fitted model. Defaults leave the historical trend unchanged."
          ),
          sliderInput(
            ns("income_growth"),
            "Income growth (%/yr)",
            min = -60,
            max = 60,
            value = 0,
            step = 0.5
          ),
          selectInput(
            ns("overhead_model"),
            "Overhead projection",
            choices = list(
              "Statistical" = c(
                "Fitted trend (+ growth %)" = "trend"
              ),
              "Flat -- data-driven" = c(
                "Historical average" = "avg_full",
                "Recent average (last 6 mo)" = "avg_recent",
                "Historical maximum (stress test)" = "max_full",
                "Recent maximum (last 6 mo)" = "max_recent"
              ),
              "Flat -- manual" = c(
                "Custom monthly value" = "custom"
              )
            ),
            selected = "trend"
          ),
          uiOutput(ns("overhead_model_controls")),
          hr(),
          # -- Practice profile (optional) ------------------------------
          tags$p(
            class = "small fw-semibold mb-0",
            bs_icon("people"),
            " Practice Profile"
          ),
          tags$p(
            class = "small text-muted",
            "Enter your current panel size and monthly fee to unlock member-count",
            " projections: members needed to break even, members to hit your income",
            " target, and per-member per-month (PMPM) revenue. Leave at 0 to skip."
          ),
          numericInput(
            ns("panel_size"),
            "Panel size (members)",
            value = 0,
            min = 0,
            step = 1
          ),
          numericInput(
            ns("membership_fee"),
            "Monthly fee ($/member)",
            value = 0,
            min = 0,
            step = 5
          ),
          hr(),
          # -- Target-specific control -----------------------------------
          uiOutput(ns("target_input_ui")),
          # -- Run button ------------------------------------------------
          input_task_button(
            ns("btn_run"),
            "Run Forecast",
            icon = bsicons::bs_icon("play-fill"),
            class = "btn-primary w-100"
          ),
          uiOutput(ns("download_ui"))
        ),

        # -- Main area: forecast sub-tabs -----------------------------------
        navset_card_underline(
          id = ns("forecast_type"),
          nav_panel(
            "Break-even",
            value = "breakeven",
            uiOutput(ns("breakeven_ui"))
          ),
          nav_panel(
            "Revenue Forecast",
            value = "revenue",
            uiOutput(ns("revenue_ui"))
          ),
          nav_panel(
            "Income Target",
            value = "target",
            uiOutput(ns("target_ui"))
          )
        )
      )
    })

    # -- Method hint (plain-language guidance shown below the method selector) ---
    output$method_hint <- renderUI({
      n_periods <- if (!is.null(r$overhead_monthly)) {
        nrow(r$overhead_monthly)
      } else {
        0L
      }
      hint <- switch(
        input$method %||% "linear",
        linear = list(
          icon = "check2-circle",
          theme = "text-success",
          head = paste0(
            "Linear regression \u2014 ",
            n_periods,
            " observed period",
            if (n_periods != 1L) "s"
          ),
          body = paste0(
            "Fits a straight-line trend to your data. Best when you have ",
            "fewer than 20 periods or expect steady, predictable growth. ",
            "The simplest option and easiest to explain to others."
          )
        ),
        ets = list(
          icon = "arrow-repeat",
          theme = "text-info",
          head = paste0(
            "Exponential smoothing \u2014 ",
            n_periods,
            " observed period",
            if (n_periods != 1L) "s"
          ),
          body = paste0(
            "Weights recent periods more heavily than older ones, so the model ",
            "adapts as your practice evolves. A good middle ground for practices ",
            "with 20\u201340 periods of history where recent months are more ",
            "representative than the full record."
          )
        ),
        arima = list(
          icon = "activity",
          theme = if (n_periods < 30L) "text-warning" else "text-secondary",
          head = paste0(
            "ARIMA \u2014 ",
            n_periods,
            " observed period",
            if (n_periods != 1L) "s",
            if (n_periods < 30L) " (limited data)" else ""
          ),
          body = paste0(
            "Identifies patterns in how each period relates to prior ones. ",
            "Works best with 30 or more periods of history. ",
            if (n_periods < 30L) {
              "With your current history, linear or ETS will likely be more stable. "
            } else {
              ""
            },
            "Can capture subtle cycles but may over-fit with sparse data."
          )
        )
      )
      tags$div(
        class = paste("small mb-2", hint$theme),
        tags$span(
          class = "fw-semibold",
          bs_icon(hint$icon, size = "0.9em"),
          " ",
          hint$head
        ),
        tags$br(),
        hint$body
      )
    })

    # -- Show target income input only on the Target sub-tab -----------------
    output$target_input_ui <- renderUI({
      req(input$forecast_type)
      if (input$forecast_type == "target") {
        numericInput(
          ns("target_income"),
          if (is_weekly()) {
            "Weekly net income target ($)"
          } else {
            "Monthly net income target ($)"
          },
          value = 5000,
          min = 0,
          step = 500
        )
      }
    })

    # -- Frequency helper -----------------------------------------------------
    is_weekly <- reactive({
      !is.null(r$overhead_monthly) &&
        "week_start" %in% names(r$overhead_monthly)
    })

    # -- Practice profile helpers ---------------------------------------------
    # profile_ok() is TRUE only when both panel_size and membership_fee are
    # positive numbers -- gates all member-count value boxes and sentences.
    profile_ok <- reactive({
      ps <- input$panel_size
      mf <- input$membership_fee
      isTRUE(
        !is.null(ps) &&
          !is.na(ps) &&
          ps > 0 &&
          !is.null(mf) &&
          !is.na(mf) &&
          mf > 0
      )
    })

    # membership_fee is always entered as $/member/month.  When the data are
    # weekly the forecast quantities (current_revenue, current_overhead,
    # required_revenue_now) are in $/week, so dividing by the monthly fee
    # gives a nonsensical member count.  fee_per_period() converts the monthly
    # fee to the period unit actually used by the current forecast.
    fee_per_period <- reactive({
      mf <- input$membership_fee
      if (is.null(mf) || is.na(mf) || mf <= 0) {
        return(NA_real_)
      }
      if (is_weekly()) mf / 4.33 else mf
    })

    # Sync practice-profile inputs into shared reactive state so other modules
    # (summary, interpret) can read them without re-wiring.
    observe({
      ps <- input$panel_size
      mf <- input$membership_fee
      r$panel_size <- if (!is.null(ps) && !is.na(ps) && ps > 0) ps else NULL
      r$membership_fee <- if (!is.null(mf) && !is.na(mf) && mf > 0) mf else NULL
    })

    # -- Value text auto-shrink -----------------------------------------------
    # bslib value_box renders the value inside a paragraph styled with Bootstrap
    # fs-2 (~1.5 rem). Long strings wrap inside the fixed 90 px box height.
    # Wrapping the value in a span with an inline font-size overrides fs-2 via
    # CSS specificity, keeping everything on one or two lines.
    #
    # Calibrated thresholds (characters):
    #   <= 9  : default  (fs-2, ~1.5 rem) -- "Achieved", "$1,234", "12"
    #   10-14 : medium   (1.05 rem)       -- "Not in horizon", "Jan 2026"
    #   >= 15 : small    (0.88 rem)       -- "Achieved (at risk)"
    shrink_value <- function(x) {
      s <- as.character(x)
      n <- nchar(s)
      if (n >= 15) {
        tags$span(style = "font-size: 0.88rem; line-height: 1.25;", s)
      } else if (n >= 10) {
        tags$span(style = "font-size: 1.05rem; line-height: 1.3;", s)
      } else {
        s
      }
    }

    # -- Reactive forecast results --------------------------------------------
    # Each result is computed on demand when the run button is clicked.
    # Using bindEvent to avoid firing before the user explicitly requests it.

    # Helper: run a forecast call, surfacing data-volume warnings as UI
    # notifications and silently discarding all other expected warnings
    # (breakeven-not-reached, frequency-mismatch, etc.).
    # Uses withCallingHandlers for warnings (stack intact, muffle works) and
    # wraps that in tryCatch to catch errors after unwinding.
    run_forecast <- function(expr) {
      tryCatch(
        withCallingHandlers(
          expr,
          warning = function(w) {
            if (inherits(w, "dcForecastR_insufficient_data")) {
              showNotification(
                conditionMessage(w),
                type = "error",
                duration = 15
              )
            } else if (inherits(w, "dcForecastR_method_fallback")) {
              showNotification(
                conditionMessage(w),
                type = "warning",
                duration = 12
              )
            } else if (inherits(w, "dcForecastR_low_data_advisory")) {
              showNotification(
                conditionMessage(w),
                type = "message",
                duration = 10
              )
            }
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 8)
          NULL
        }
      )
    }

    # Income summary: use uploaded/manually entered income if available,
    # otherwise fall back to a proportional proxy so projections still run.
    income_summary <- reactive({
      req(r$overhead_monthly)
      if (!is.null(r$income_monthly) && nrow(r$income_monthly) > 0) {
        r$income_monthly
      } else {
        showNotification(
          paste0(
            "No income data found. Projections are based on a synthetic revenue proxy ",
            "(80% of overhead) and should be treated as rough estimates only. ",
            "Upload income data or use manual entry for accurate forecasts."
          ),
          type = "warning",
          duration = 10
        )
        tibble::tibble(
          practice_id = r$overhead_monthly$practice_id,
          year = r$overhead_monthly$year,
          month = r$overhead_monthly$month,
          total_revenue = r$overhead_monthly$total_overhead * 0.8
        )
      }
    })

    breakeven_result <- eventReactive(input$btn_run, {
      req(r$overhead_monthly)
      run_forecast(
        directCareForecastR::forecast_breakeven(
          income_summary = income_summary(),
          overhead_summary = r$overhead_monthly,
          method = input$method,
          horizon = input$horizon,
          confidence_level = input$confidence
        )
      )
    })

    revenue_result <- eventReactive(input$btn_run, {
      req(r$overhead_monthly)
      run_forecast(
        directCareForecastR::forecast_revenue(
          income_summary = income_summary(),
          method = input$method,
          horizon = input$horizon,
          confidence_level = input$confidence
        )
      )
    })

    target_result <- eventReactive(input$btn_run, {
      req(
        r$overhead_monthly,
        input$target_income,
        input$forecast_type == "target"
      )
      run_forecast(
        directCareForecastR::forecast_target(
          income_summary = income_summary(),
          overhead_summary = r$overhead_monthly,
          target_income = input$target_income,
          method = input$method,
          horizon = input$horizon,
          confidence_level = input$confidence
        )
      )
    })

    # -- Conditional overhead-model controls ---------------------------------
    output$overhead_model_controls <- renderUI({
      model <- input$overhead_model %||% "trend"
      if (model == "trend") {
        sliderInput(
          ns("overhead_growth"),
          "Overhead growth (%/yr)",
          min = -60,
          max = 60,
          value = 0,
          step = 0.5
        )
      } else if (model == "custom") {
        default_val <- if (
          !is.null(r$overhead_monthly) && nrow(r$overhead_monthly) > 0
        ) {
          round(mean(r$overhead_monthly$total_overhead, na.rm = TRUE))
        } else {
          0L
        }
        numericInput(
          ns("overhead_custom"),
          "Monthly overhead ($)",
          value = default_val,
          min = 0,
          step = 100
        )
      }
      # avg_full / avg_recent: computed from data, no extra input needed
    })

    # -- Flat-overhead helper -------------------------------------------------
    # Returns a single numeric (monthly $) when a flat model is selected, or
    # NULL for "trend" (the growth multiplier path in apply_growth_assumptions).
    compute_flat_overhead <- reactive({
      model <- input$overhead_model %||% "trend"
      ovhd <- r$overhead_monthly
      has_data <- !is.null(ovhd) && nrow(ovhd) > 0
      recent_n <- if (has_data) min(6L, nrow(ovhd)) else 0L

      if (model == "avg_full" && has_data) {
        mean(ovhd$total_overhead, na.rm = TRUE)
      } else if (model == "avg_recent" && has_data) {
        mean(tail(ovhd, recent_n)$total_overhead, na.rm = TRUE)
      } else if (model == "max_full" && has_data) {
        max(ovhd$total_overhead, na.rm = TRUE)
      } else if (model == "max_recent" && has_data) {
        max(tail(ovhd, recent_n)$total_overhead, na.rm = TRUE)
      } else if (model == "custom") {
        as.numeric(input$overhead_custom %||% 0)
      } else {
        NULL
      } # "trend": use growth multiplier
    })

    # -- Growth / overhead-adjusted reactives ---------------------------------
    # Re-fire whenever sliders or model selector changes, without re-running
    # the expensive forecast model.

    # Shared overhead parameters used by both adj_breakeven and adj_target.
    # Kept separate from adj_revenue so that revenue tab does not invalidate
    # when the overhead model or growth slider changes.
    overhead_assumptions <- reactive({
      model <- input$overhead_model %||% "trend"
      list(
        growth_pct = if (model == "trend") input$overhead_growth %||% 0 else 0,
        flat = compute_flat_overhead()
      )
    })

    adj_breakeven <- reactive({
      req(breakeven_result())
      oa <- overhead_assumptions()
      apply_growth_assumptions(
        breakeven_result(),
        income_growth_pct = input$income_growth %||% 0,
        overhead_growth_pct = oa$growth_pct,
        overhead_flat = oa$flat
      )
    })

    adj_revenue <- reactive({
      req(revenue_result())
      apply_growth_assumptions(
        revenue_result(),
        income_growth_pct = input$income_growth %||% 0
      )
    })

    adj_target <- reactive({
      req(target_result())
      oa <- overhead_assumptions()
      apply_growth_assumptions(
        target_result(),
        income_growth_pct = input$income_growth %||% 0,
        overhead_growth_pct = oa$growth_pct,
        overhead_flat = oa$flat,
        # Always pass target_income so apply_growth_assumptions can keep
        # fd$required_revenue consistent with the (possibly adjusted)
        # overhead forecast -- whether that's a flat model or the fitted
        # trend with a non-zero overhead growth rate.
        target_income_override = input$target_income
      )
    })

    # -- Break-even UI --------------------------------------------------------
    output$breakeven_ui <- renderUI({
      res <- adj_breakeven()
      if (is.null(res)) {
        return(p(
          class = "text-muted",
          "Click 'Run Forecast' to generate results."
        ))
      }

      sustained <- breakeven_is_sustained(res)
      already <- identical(res$periods_to_breakeven, 0L)

      bkevn_value <- if (already && isTRUE(sustained)) {
        "Achieved"
      } else if (already && isFALSE(sustained)) {
        "Achieved (at risk)"
      } else if (already) {
        "Achieved"
      } else if (is.na(res$breakeven_date)) {
        "Not in horizon"
      } else {
        format(res$breakeven_date, "%b %Y")
      }

      bkevn_theme <- if (already && isFALSE(sustained)) {
        "warning"
      } else if (already) {
        "success"
      } else if (is.na(res$breakeven_date)) {
        "danger"
      } else {
        "primary"
      }

      # Build value-box list; add member-count box when profile is complete
      bkevn_boxes <- list(
        value_box(
          title = "Break-even",
          value = shrink_value(bkevn_value),
          theme = bkevn_theme,
          height = "90px"
        ),
        value_box(
          title = if (is_weekly()) "Weeks away" else "Months away",
          value = if (is.na(res$periods_to_breakeven)) {
            "--"
          } else {
            res$periods_to_breakeven
          },
          theme = "secondary",
          height = "90px"
        ),
        value_box(
          title = "Current surplus / deficit",
          value = fmt_dollar(res$current_surplus_deficit),
          theme = if (res$current_surplus_deficit >= 0) {
            "success"
          } else {
            "warning"
          },
          height = "90px"
        )
      )
      if (profile_ok()) {
        # current_overhead_avg is the rolling mean of the last few overhead
        # periods (in per-income-period units). It is more reliable than the
        # raw single-period current_overhead, which is often $0 for weekly
        # data when monthly overhead lands only on month-start rows.
        current_ovhd <- res$current_overhead_avg %||%
          res$current_overhead %||%
          (res$current_revenue - res$current_surplus_deficit)
        members_bkevn <- ceiling(current_ovhd / fee_per_period())
        bkevn_boxes <- c(
          bkevn_boxes,
          list(
            value_box(
              title = "Members to break even",
              value = members_bkevn,
              theme = "info",
              height = "90px"
            )
          )
        )
      }

      tagList(
        do.call(
          layout_column_wrap,
          c(list(width = "180px", fill = FALSE), bkevn_boxes)
        ),
        div(
          style = "max-height: 620px; overflow-y: auto; padding-right: 2px;",
          card(
            full_screen = TRUE,
            card_header("Break-even Forecast"),
            card_body(plotOutput(
              ns("breakeven_plot"),
              height = "400px",
              width = "100%"
            ))
          ),
          card(
            card_header("Interpretation"),
            card_body(uiOutput(ns("breakeven_interpretation")))
          )
        ),
      )
    })

    output$breakeven_plot <- renderPlot(
      {
        req(adj_breakeven())
        plot_forecast_breakeven(
          adj_breakeven(),
          income_monthly = r$income_monthly,
          overhead_monthly = r$overhead_monthly
        )
      },
      res = 96,
      height = 400,
      width = function() {
        w <- session$clientData[[paste0(
          "output_",
          ns("breakeven_plot"),
          "_width"
        )]]
        max(w %||% 700L, 300L)
      }
    )

    output$breakeven_interpretation <- renderUI({
      req(adj_breakeven())
      HTML(interpret_breakeven(
        adj_breakeven(),
        r$practice_name,
        sustained = breakeven_is_sustained(adj_breakeven()),
        panel_size = r$panel_size,
        membership_fee = r$membership_fee,
        confidence_level = input$confidence
      ))
    })

    # -- Revenue UI -----------------------------------------------------------
    output$revenue_ui <- renderUI({
      res <- adj_revenue()
      if (is.null(res)) {
        return(p(
          class = "text-muted",
          "Click 'Run Forecast' to generate results."
        ))
      }

      rev_boxes <- list(
        value_box(
          title = if (is_weekly()) {
            "Current weekly revenue"
          } else {
            "Current monthly revenue"
          },
          value = fmt_dollar(res$current_revenue),
          theme = "primary",
          height = "90px"
        ),
        value_box(
          title = paste(
            "Projected in",
            input$horizon,
            if (is_weekly()) "weeks" else "months"
          ),
          value = fmt_dollar(tail(res$forecast_data$revenue_forecast, 1)),
          theme = "secondary",
          height = "90px"
        ),
        value_box(
          title = "Method",
          value = shrink_value(toupper(res$method)),
          theme = "info",
          height = "90px"
        )
      )
      if (profile_ok()) {
        pmpm_label <- if (is_weekly()) {
          "Revenue per member / week"
        } else {
          "Revenue per member / month"
        }
        pmpm_val <- res$current_revenue / input$panel_size
        # Compare per-period PMPM against per-period fee (not monthly fee directly).
        pmpm_theme <- if (pmpm_val >= fee_per_period() * 0.9) {
          "success"
        } else {
          "warning"
        }
        rev_boxes <- c(
          rev_boxes,
          list(
            value_box(
              title = pmpm_label,
              value = fmt_dollar(pmpm_val),
              theme = pmpm_theme,
              height = "90px"
            )
          )
        )
      }

      tagList(
        do.call(
          layout_column_wrap,
          c(list(width = "180px", fill = FALSE), rev_boxes)
        ),
        div(
          style = "max-height: 620px; overflow-y: auto; padding-right: 2px;",
          card(
            full_screen = TRUE,
            card_header("Revenue Forecast"),
            card_body(plotOutput(
              ns("revenue_plot"),
              height = "400px",
              width = "100%"
            ))
          ),
          card(
            card_header("Interpretation"),
            card_body(uiOutput(ns("revenue_interpretation")))
          )
        ),
      )
    })

    output$revenue_plot <- renderPlot(
      {
        req(adj_revenue())
        plot_forecast_revenue(adj_revenue(), income_monthly = r$income_monthly)
      },
      res = 96,
      height = 400,
      width = function() {
        w <- session$clientData[[paste0(
          "output_",
          ns("revenue_plot"),
          "_width"
        )]]
        max(w %||% 700L, 300L)
      }
    )

    output$revenue_interpretation <- renderUI({
      req(adj_revenue())
      HTML(interpret_revenue(
        adj_revenue(),
        r$practice_name,
        panel_size = r$panel_size,
        membership_fee = r$membership_fee,
        confidence_level = input$confidence
      ))
    })

    # -- Income Target UI -----------------------------------------------------
    output$target_ui <- renderUI({
      res <- adj_target()
      if (is.null(res)) {
        return(p(
          class = "text-muted",
          "Click 'Run Forecast' to generate results."
        ))
      }

      tgt_sustained <- target_is_sustained(res)
      tgt_already <- isTRUE(res$current_gap >= 0)

      tgt_value <- if (tgt_already && isFALSE(tgt_sustained)) {
        "Achieved (at risk)"
      } else if (tgt_already) {
        "Achieved"
      } else if (is.na(res$target_date)) {
        "Not in horizon"
      } else {
        format(res$target_date, "%b %Y")
      }

      tgt_theme <- if (tgt_already && isFALSE(tgt_sustained)) {
        "warning"
      } else if (tgt_already) {
        "success"
      } else if (is.na(res$target_date)) {
        "danger"
      } else {
        "success"
      }

      tgt_boxes <- list(
        value_box(
          title = "Target reached",
          value = shrink_value(tgt_value),
          theme = tgt_theme,
          height = "90px"
        ),
        value_box(
          title = "Revenue needed now",
          value = fmt_dollar(res$required_revenue_now),
          theme = "secondary",
          height = "90px"
        ),
        value_box(
          title = "Current gap",
          value = fmt_dollar(res$current_gap),
          theme = if (isTRUE(res$current_gap >= 0)) "success" else "warning",
          height = "90px"
        )
      )
      if (profile_ok()) {
        # Divide required revenue ($/period) by fee per period (not monthly fee)
        # to get a panel-size figure in consistent units.
        members_needed <- ceiling(res$required_revenue_now / fee_per_period())
        members_gap <- members_needed - input$panel_size
        tgt_boxes <- c(
          tgt_boxes,
          list(
            value_box(
              title = "Members needed",
              value = members_needed,
              theme = if (members_gap <= 0) "success" else "info",
              height = "90px"
            )
          )
        )
      }

      tagList(
        do.call(
          layout_column_wrap,
          c(list(width = "200px", fill = FALSE), tgt_boxes)
        ),
        div(
          style = "max-height: 620px; overflow-y: auto; padding-right: 2px;",
          card(
            full_screen = TRUE,
            card_header("Income Target Forecast"),
            card_body(plotOutput(
              ns("target_plot"),
              height = "400px",
              width = "100%"
            ))
          ),
          card(
            card_header("Interpretation"),
            card_body(uiOutput(ns("target_interpretation")))
          )
        ),
      )
    })

    output$target_plot <- renderPlot(
      {
        req(adj_target())
        plot_forecast_target(adj_target(), income_monthly = r$income_monthly)
      },
      res = 96,
      height = 400,
      width = function() {
        w <- session$clientData[[paste0(
          "output_",
          ns("target_plot"),
          "_width"
        )]]
        max(w %||% 700L, 300L)
      }
    )

    output$target_interpretation <- renderUI({
      req(adj_target())
      HTML(interpret_target(
        adj_target(),
        r$practice_name,
        input$target_income,
        sustained = target_is_sustained(adj_target()),
        panel_size = r$panel_size,
        membership_fee = r$membership_fee
      ))
    })

    # -- Sidebar download button (appears once any forecast has been run) ------
    output$download_ui <- renderUI({
      if (is.null(breakeven_result()) && is.null(revenue_result())) {
        return(NULL)
      }
      downloadButton(
        ns("dl_report"),
        tagList(bs_icon("file-earmark-pdf"), " Download Report"),
        class = "btn-outline-secondary w-100 mt-2"
      )
    })

    # -- Comprehensive PDF report handler -------------------------------------
    output$dl_report <- downloadHandler(
      filename = function() {
        safe_name <- gsub("[^A-Za-z0-9_-]", "-", r$practice_id %||% "practice")
        paste0(
          "dpc-report-",
          safe_name,
          "-",
          format(Sys.Date(), "%Y%m%d"),
          ".pdf"
        )
      },
      content = function(file) {
        # Collect interpretation text (HTML \u2192 plain text handled inside build_report_data)
        bkevn_text <- if (!is.null(adj_breakeven())) {
          interpret_breakeven(
            adj_breakeven(),
            r$practice_name,
            sustained = breakeven_is_sustained(adj_breakeven()),
            panel_size = r$panel_size,
            membership_fee = r$membership_fee,
            confidence_level = input$confidence
          )
        } else {
          NULL
        }

        rev_text <- if (!is.null(adj_revenue())) {
          interpret_revenue(
            adj_revenue(),
            r$practice_name,
            panel_size = r$panel_size,
            membership_fee = r$membership_fee,
            confidence_level = input$confidence
          )
        } else {
          NULL
        }

        tgt_text <- if (!is.null(adj_target())) {
          interpret_target(
            adj_target(),
            r$practice_name,
            input$target_income,
            sustained = target_is_sustained(adj_target()),
            panel_size = r$panel_size,
            membership_fee = r$membership_fee
          )
        } else {
          NULL
        }

        data <- build_report_data(
          r = r,
          inputs = list(
            method = input$method,
            horizon = input$horizon,
            confidence = input$confidence,
            target_income = input$target_income
          ),
          breakeven_res = adj_breakeven(),
          revenue_res = adj_revenue(),
          target_res = adj_target(),
          interpret_bkevn = bkevn_text,
          interpret_rev = rev_text,
          interpret_tgt = tgt_text
        )

        withProgress(message = "Generating PDF report\u2026", value = 0.5, {
          render_report_pdf(
            data,
            file,
            breakeven_res = adj_breakeven(),
            revenue_res = adj_revenue(),
            target_res = adj_target(),
            income_monthly = r$income_monthly,
            overhead_monthly = r$overhead_monthly
          )
        })
      }
    )
  })
}
