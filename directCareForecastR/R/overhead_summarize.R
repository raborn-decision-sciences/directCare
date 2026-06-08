summarize_overhead_monthly <- function(overhead_tbl,
                                        include_refunds = TRUE) {
  if (!include_refunds) {
    overhead_tbl <- dplyr::filter(overhead_tbl, !is_refund)
  }

  overhead_tbl |>
    dplyr::group_by(practice_id, year, month) |>
    dplyr::summarise(
      total_overhead = sum(amount),
      gross_overhead = sum(amount[!is_refund]),
      total_refunds  = -sum(amount[is_refund]),
      .groups = "drop"
    )
  
}
