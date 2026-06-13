#' Default Account Category Map
#'
#' Returns a tibble of pattern-matching rules used by \code{map_accounts()} to
#' assign expense categories to GnuCash account names. Each row defines one
#' rule: a \code{pattern} to search for in the account name, the
#' \code{category} to assign when the pattern matches, the type of match to
#' perform (\code{match_type}), and whether to ignore case (\code{ignore_case}).
#'
#' Pass the result to \code{ingest_gnucash_csv()} as the \code{account_map}
#' argument to override or extend the defaults for a specific practice.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{pattern}{Character string to match against the account name}
#'     \item{category}{Category label assigned on a match: one of
#'       \code{"rent"}, \code{"staff"}, \code{"supplies"}, \code{"software"},
#'       \code{"insurance"}, \code{"marketing"}, \code{"labs"},
#'       \code{"equipment"}, \code{"licenses"}, \code{"education"}, or
#'       \code{"other"}}
#'     \item{match_type}{How to apply the pattern: \code{"contains"} (fixed
#'       substring), \code{"exact"} (full string equality), or \code{"regex"}
#'       (Perl-compatible regular expression)}
#'     \item{ignore_case}{Logical; whether matching is case-insensitive}
#'   }
#'
#' @export
#'
#' @examples
#' # Inspect the defaults
#' default_account_map()
#'
#' # Extend with a practice-specific rule
#' custom_map <- dplyr::bind_rows(
#'   tibble::tibble(
#'     pattern     = "lab",
#'     category    = "supplies",
#'     match_type  = "contains",
#'     ignore_case = TRUE
#'   ),
#'   default_account_map()
#' )
default_account_map <- function() {
  tibble::tibble(
    pattern = c(
      # Staff & payroll
      "salary",
      "wages",
      "payroll",
      "staff",

      # Medical / office supplies
      "supplies",
      "medical supplies",
      "office supplies",
      "recurrent",

      # Software & subscriptions (specific products first, then generic)
      "adobe",
      "bamboo",
      "blaze",
      "spruce",
      "software",
      "subscription",
      "emr",
      "ehr",

      # Insurance (must precede "equipment" so "Equipment Insurance" maps here,
      # and precede "rent" so nothing unexpected matches)
      "insurance",
      "malpractice",
      "liability",

      # Marketing
      "marketing",
      "advertising",
      "advertisement",
      "website",

      # Lab costs
      "lab",
      "clia",

      # Equipment (after insurance, before "rent" so "Equipment Rental"
      # maps here rather than to rent)
      "equipment",

      # Rent & occupancy (after equipment so "Equipment Rental" is caught above)
      "rent",
      "lease",

      # Licenses, certifications & business formation
      "license",
      "permit",
      "llc",

      # Continuing education & conferences
      "education",
      "conference",

      # Other / catch-all
      "utilities",
      "electric",
      "internet",
      "phone",
      "misc",
      "dining"
    ),
    category = c(
      "staff",
      "staff",
      "staff",
      "staff",
      "supplies",
      "supplies",
      "supplies",
      "supplies",
      "software",
      "software",
      "software",
      "software",
      "software",
      "software",
      "software",
      "software",
      "insurance",
      "insurance",
      "insurance",
      "marketing",
      "marketing",
      "marketing",
      "marketing",
      "labs",
      "labs",
      "equipment",
      "rent",
      "rent",
      "licenses",
      "licenses",
      "licenses",
      "education",
      "education",
      "other",
      "other",
      "other",
      "other",
      "other",
      "other"
    ),
    match_type = "contains",
    ignore_case = TRUE
  )
}


#' Map Account Names to Expense Categories
#'
#' Iterates over the rows of \code{account_map} in order, testing each
#' pattern against the \code{account} column of \code{raw_tbl}. The first
#' matching rule wins. Accounts that match no rule are assigned the category
#' \code{"other"} and a warning is issued listing the unmatched names.
#'
#' @param raw_tbl A data frame with at minimum columns \code{account},
#'   \code{amount}, and \code{date}, as produced by the initial
#'   \code{readr::read_csv()} call inside \code{ingest_gnucash_csv()}.
#' @param account_map A tibble of mapping rules as returned by
#'   \code{default_account_map()}. Columns required: \code{pattern},
#'   \code{category}, \code{match_type}, \code{ignore_case}.
#'
#' @return \code{raw_tbl} with a \code{category} column appended.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' raw <- readr::read_csv("gnucash_export.csv") |>
#'   dplyr::rename(account = "Account Name", amount = "Amount Num.", date = "Date")
#'
#' map_accounts(raw, default_account_map())
#' }
map_accounts <- function(raw_tbl, account_map = default_account_map()) {
  stopifnot(
    is.data.frame(raw_tbl),
    all(c("account", "amount", "date") %in% names(raw_tbl))
  )

  # Vectorized first-match-wins: iterate over rules (short list), apply each
  # rule to all still-unmatched accounts at once via grepl/==, then assign.
  # O(n_rules) R-level iterations instead of O(n_accounts x n_rules).
  matched <- rep(NA_character_, nrow(raw_tbl))
  accounts <- raw_tbl$account
  for (i in seq_len(nrow(account_map))) {
    pending <- which(is.na(matched))
    if (length(pending) == 0L) {
      break
    }
    row <- account_map[i, ]
    needle <- row$pattern
    haystack <- if (row$ignore_case) {
      tolower(accounts[pending])
    } else {
      accounts[pending]
    }
    if (row$ignore_case) {
      needle <- tolower(needle)
    }
    hits <- switch(
      row$match_type,
      contains = grepl(needle, haystack, fixed = TRUE),
      exact = haystack == needle,
      regex = grepl(needle, haystack, perl = TRUE),
      FALSE
    )
    matched[pending[hits]] <- row$category
  }

  unmatched <- raw_tbl$account[is.na(matched)]
  if (length(unmatched) > 0) {
    rlang::warn(
      paste0(
        "The following accounts could not be mapped and were assigned 'other': ",
        paste(unique(unmatched), collapse = ", ")
      ),
      class = "dcForecastR_unmapped_accounts",
      unmatched = unique(unmatched)
    )
    matched[is.na(matched)] <- "other"
  }

  raw_tbl |>
    dplyr::mutate(category = matched)
}
