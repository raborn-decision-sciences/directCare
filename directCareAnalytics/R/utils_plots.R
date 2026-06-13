#' Forecast plot helpers
#'
#' Each function accepts the list returned by the corresponding
#' `directCareForecastR::forecast_*()` function and returns a ggplot2 object.
#' All plots share a consistent style so they render identically in the app
#' and in the downloaded report.
#'
#' Pass `income_monthly` and/or `overhead_monthly` (the summarized tibbles from
#' the upload pipeline) to overlay the historical observed series before the
#' forecast horizon begins.
#'
#' @noRd

# -- Shared theme --------------------------------------------------------------
plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(color = "grey40", size = 9),
      legend.position = "bottom",
      # Rotate x-axis date labels so they stay legible on long horizons
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      # Keep margins tight so the plot never overflows a small card
      plot.margin = ggplot2::margin(t = 6, r = 12, b = 10, l = 4)
    )
}

# Derive period_start (Date) from year/month columns (monthly) or week_start (weekly)
.make_period_start <- function(tbl) {
  if ("week_start" %in% names(tbl)) {
    dplyr::rename(tbl, period_start = week_start)
  } else {
    dplyr::mutate(
      tbl,
      period_start = as.Date(
        paste(year, sprintf("%02d", as.integer(month)), "01", sep = "-")
      )
    )
  }
}

# Vertical separator between observed history and forecast
.forecast_separator <- function(cutoff_date) {
  ggplot2::geom_vline(
    xintercept = cutoff_date,
    linetype = "dotted",
    colour = "grey50",
    linewidth = 0.6
  )
}


# -- Break-even plot -----------------------------------------------------------

#' @noRd
plot_forecast_breakeven <- function(
  result,
  income_monthly = NULL,
  overhead_monthly = NULL
) {
  fd <- result$forecast_data

  p <- ggplot2::ggplot(fd, ggplot2::aes(x = period_start)) +
    # Confidence ribbons (forecast only)
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = revenue_lower, ymax = revenue_upper),
      fill = "#4a90d9",
      alpha = 0.15
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = overhead_lower, ymax = overhead_upper),
      fill = "#c0392b",
      alpha = 0.15
    ) +
    # Forecast lines
    ggplot2::geom_line(
      ggplot2::aes(y = revenue_forecast, colour = "Revenue"),
      linewidth = 1.0,
      linetype = "dashed"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = overhead_forecast, colour = "Overhead"),
      linewidth = 1.0,
      linetype = "dashed"
    )

  # Observed history ----------------------------------------------------------
  has_income <- !is.null(income_monthly) && nrow(income_monthly) > 0
  has_overhead <- !is.null(overhead_monthly) && nrow(overhead_monthly) > 0

  if (has_income || has_overhead) {
    # Build a joined observed frame so both series share the same x-axis
    if (has_income) {
      obs_inc <- .make_period_start(income_monthly) |>
        dplyr::select(period_start, observed_revenue = total_revenue)
    }
    if (has_overhead) {
      obs_ovhd <- .make_period_start(overhead_monthly) |>
        dplyr::select(period_start, observed_overhead = total_overhead)
    }

    if (has_income && has_overhead) {
      obs <- dplyr::full_join(obs_inc, obs_ovhd, by = "period_start") |>
        dplyr::arrange(period_start)
    } else if (has_income) {
      obs <- obs_inc |> dplyr::arrange(period_start)
    } else {
      obs <- obs_ovhd |> dplyr::arrange(period_start)
    }

    cutoff <- max(obs$period_start, na.rm = TRUE)

    p <- p + .forecast_separator(cutoff)

    if (has_income) {
      p <- p +
        ggplot2::geom_line(
          data = obs,
          mapping = ggplot2::aes(
            x = period_start,
            y = observed_revenue,
            colour = "Revenue"
          ),
          linewidth = 1.1,
          na.rm = TRUE
        ) +
        ggplot2::geom_point(
          data = obs,
          mapping = ggplot2::aes(
            x = period_start,
            y = observed_revenue,
            colour = "Revenue"
          ),
          size = 2.2,
          na.rm = TRUE
        )
    }
    if (has_overhead) {
      p <- p +
        ggplot2::geom_line(
          data = obs,
          mapping = ggplot2::aes(
            x = period_start,
            y = observed_overhead,
            colour = "Overhead"
          ),
          linewidth = 1.1,
          na.rm = TRUE
        ) +
        ggplot2::geom_point(
          data = obs,
          mapping = ggplot2::aes(
            x = period_start,
            y = observed_overhead,
            colour = "Overhead"
          ),
          size = 2.2,
          na.rm = TRUE
        )
    }
  }

  p <- p +
    ggplot2::scale_colour_manual(
      values = c(Revenue = "#4a90d9", Overhead = "#c0392b"),
      name = NULL
    ) +
    ggplot2::scale_y_continuous(labels = fmt_dollar_format()) +
    ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
    ggplot2::labs(
      title = "Break-even Forecast",
      subtitle = paste(
        "Method:",
        toupper(result$method),
        "| Solid = observed, dashed = projected"
      ),
      x = NULL,
      y = if (identical(result$frequency, "weekly")) {
        "Weekly ($)"
      } else {
        "Monthly ($)"
      }
    ) +
    plot_theme()

  # Break-even marker ---------------------------------------------------------
  if (
    !is.na(result$breakeven_date) &&
      !is.na(result$periods_to_breakeven) &&
      result$periods_to_breakeven > 0L
  ) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = result$breakeven_date,
        linetype = "dotted",
        colour = "#2d6a4f",
        linewidth = 0.9
      ) +
      ggplot2::annotate(
        "label",
        x = result$breakeven_date,
        y = max(fd$revenue_upper, na.rm = TRUE),
        label = paste("Break-even\n", format(result$breakeven_date, "%b %Y")),
        fill = "#2d6a4f",
        colour = "white",
        size = 3.5,
        hjust = -0.05
      )
  }

  p
}


# -- Revenue forecast plot -----------------------------------------------------

#' @noRd
plot_forecast_revenue <- function(result, income_monthly = NULL) {
  fd <- result$forecast_data

  p <- ggplot2::ggplot(fd, ggplot2::aes(x = period_start)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = revenue_lower, ymax = revenue_upper),
      fill = "#4a90d9",
      alpha = 0.2
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = revenue_forecast),
      colour = "#1e3a5f",
      linewidth = 1.1,
      linetype = "dashed"
    )

  # Observed history ----------------------------------------------------------
  if (!is.null(income_monthly) && nrow(income_monthly) > 0) {
    obs <- .make_period_start(income_monthly) |>
      dplyr::select(period_start, observed_revenue = total_revenue) |>
      dplyr::arrange(period_start)

    cutoff <- max(obs$period_start, na.rm = TRUE)

    p <- p +
      .forecast_separator(cutoff) +
      ggplot2::geom_line(
        data = obs,
        mapping = ggplot2::aes(x = period_start, y = observed_revenue),
        colour = "#1e3a5f",
        linewidth = 1.2
      ) +
      ggplot2::geom_point(
        data = obs,
        mapping = ggplot2::aes(x = period_start, y = observed_revenue),
        colour = "#1e3a5f",
        size = 2.5
      )
  }

  p +
    ggplot2::scale_y_continuous(labels = fmt_dollar_format()) +
    ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
    ggplot2::labs(
      title = "Revenue Forecast",
      subtitle = paste(
        "Method:",
        toupper(result$method),
        "| Horizon:",
        nrow(fd),
        result$frequency,
        "periods",
        "| Solid = observed, dashed = projected"
      ),
      x = NULL,
      y = if (identical(result$frequency, "weekly")) {
        "Weekly Revenue ($)"
      } else {
        "Monthly Revenue ($)"
      }
    ) +
    plot_theme()
}


# -- Income target plot --------------------------------------------------------

#' @noRd
plot_forecast_target <- function(result, income_monthly = NULL) {
  fd <- result$forecast_data

  p <- ggplot2::ggplot(fd, ggplot2::aes(x = period_start)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = revenue_lower, ymax = revenue_upper),
      fill = "#4a90d9",
      alpha = 0.15
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = revenue_forecast, colour = "Revenue forecast"),
      linewidth = 1.0,
      linetype = "dashed"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = required_revenue, colour = "Required revenue"),
      linewidth = 1.1,
      linetype = "dashed"
    )

  # Observed history ----------------------------------------------------------
  if (!is.null(income_monthly) && nrow(income_monthly) > 0) {
    obs <- .make_period_start(income_monthly) |>
      dplyr::select(period_start, observed_revenue = total_revenue) |>
      dplyr::arrange(period_start)

    cutoff <- max(obs$period_start, na.rm = TRUE)

    p <- p +
      .forecast_separator(cutoff) +
      ggplot2::geom_line(
        data = obs,
        mapping = ggplot2::aes(
          x = period_start,
          y = observed_revenue,
          colour = "Revenue forecast"
        ),
        linewidth = 1.2
      ) +
      ggplot2::geom_point(
        data = obs,
        mapping = ggplot2::aes(
          x = period_start,
          y = observed_revenue,
          colour = "Revenue forecast"
        ),
        size = 2.5
      )
  }

  p <- p +
    ggplot2::scale_colour_manual(
      values = c(
        "Revenue forecast" = "#4a90d9",
        "Required revenue" = "#e9a825"
      ),
      name = NULL
    ) +
    ggplot2::scale_y_continuous(labels = fmt_dollar_format()) +
    ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
    ggplot2::labs(
      title = "Income Target Forecast",
      subtitle = paste(
        "Method:",
        toupper(result$method),
        "| Solid = observed, dashed = projected"
      ),
      x = NULL,
      y = if (identical(result$frequency, "weekly")) {
        "Weekly ($)"
      } else {
        "Monthly ($)"
      }
    ) +
    plot_theme()

  if (!is.na(result$target_date)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = result$target_date,
        linetype = "dotted",
        colour = "#2d6a4f",
        linewidth = 0.9
      ) +
      ggplot2::annotate(
        "label",
        x = result$target_date,
        y = max(fd$revenue_upper, na.rm = TRUE),
        label = paste("Target reached\n", format(result$target_date, "%b %Y")),
        fill = "#2d6a4f",
        colour = "white",
        size = 3.5,
        hjust = -0.05
      )
  }

  p
}
