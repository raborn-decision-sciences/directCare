default_account_map <- function() {
  tibble::tibble(
    pattern     = c(
      "rent", "lease",
      "salary", "wages", "payroll", "staff",
      "supplies", "medical supplies", "office supplies",
      "software", "subscription", "emr", "ehr",
      "insurance", "malpractice", "liability",
      "marketing", "advertising",
      "utilities", "electric", "internet", "phone"
    ),
    category    = c(
      "rent", "rent",
      "staff", "staff", "staff", "staff",
      "supplies", "supplies", "supplies",
      "software", "software", "software", "software",
      "insurance", "insurance", "insurance",
      "marketing", "marketing",
      "other", "other", "other", "other"
    ),
    match_type  = "contains",   # same for all defaults; see map_accounts()
    ignore_case = TRUE
  )
}

map_accounts <- function(raw_tbl, account_map = default_account_map()) {

  stopifnot(
    is.data.frame(raw_tbl),
    all(c("account", "amount", "date") %in% names(raw_tbl))
  )

  matched <- purrr::map_chr(raw_tbl$account, function(acct) {
    for (i in seq_len(nrow(account_map))) {
      row        <- account_map[i, ]
      needle     <- row$pattern
      haystack   <- if (row$ignore_case) tolower(acct) else acct
      if (row$ignore_case) needle <- tolower(needle)

      matched <- switch(row$match_type,
        contains = grepl(needle, haystack, fixed = TRUE),
        exact    = haystack == needle,
        regex    = grepl(needle, haystack, perl = TRUE),
        FALSE
      )

      if (matched) return(row$category)
    }
    return(NA_character_)  # no match
  })

  unmatched <- raw_tbl$account[is.na(matched)]
  if (length(unmatched) > 0) {
    rlang::warn(
      paste0(
        "The following accounts could not be mapped and were assigned 'other': ",
        paste(unique(unmatched), collapse = ", ")
      ),
      class = "dcForecastR_unmapped_accounts",
      unmatched = unique(unmatched)   # attach data to the condition
    )
    matched[is.na(matched)] <- "other"
  }

  raw_tbl |>
    dplyr::mutate(category = matched)
}
