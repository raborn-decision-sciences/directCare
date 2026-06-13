#' Ingest GnuCash CSV Export
#'
#' Reads a GnuCash CSV export, maps account names to internal expense
#' categories, validates the result, and returns a normalized transaction
#' tibble. The tibble contains both income and expense rows; use
#' \code{normalize_gnucash_income()} or \code{filter_gnucash_overhead()} to
#' split them before summarizing.
#'
#' @param path Character string specifying the path to the GnuCash CSV file.
#' @param practice_id Character or integer practice identifier added to every
#'   row of the output.
#' @param account_map A tibble of account mapping rules as returned by
#'   \code{default_account_map()}. Override to customize category assignments
#'   for a specific practice.
#'
#' @return A tibble with columns: \code{practice_id}, \code{date},
#'   \code{week_start}, \code{month}, \code{year}, \code{full_account_name},
#'   \code{account_name}, \code{description}, \code{amount}, \code{category},
#'   and \code{source}. The \code{is_refund} column is added downstream by
#'   \code{filter_gnucash_overhead()} and \code{normalize_gnucash_income()}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load all transactions and split into income and overhead streams
#' transactions <- ingest_gnucash_csv(
#'   path        = "path/to/gnucash_export.csv",
#'   practice_id = "practice_001"
#' )
#'
#' income   <- normalize_gnucash_income(transactions)
#' overhead <- filter_gnucash_overhead(transactions)
#' }
ingest_gnucash_csv <- function(
  path,
  practice_id,
  account_map = default_account_map()
) {
  raw_data <- readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::rename(
      account = "Account Name",
      amount = "Amount Num.",
      date = "Date"
    )
  mapped_data <- map_accounts(raw_data, account_map)
  normalize_gnucash_csv(mapped_data, practice_id, source = "gnucash_csv")
}


#' Ingest GnuCash XML File
#'
#' Reads a GnuCash XML ledger file (`.gnucash`), extracts all EXPENSE and
#' INCOME transaction splits, maps account names to internal expense
#' categories, and returns a normalized transaction tibble. The tibble contains
#' both income and expense rows; use \code{normalize_gnucash_income()} or
#' \code{filter_gnucash_overhead()} to split them before summarizing.
#'
#' GnuCash XML files may be stored either as plain XML or as gzip-compressed
#' XML. Both formats are handled transparently by the underlying
#' \pkg{xml2} / libxml2 parser.
#'
#' @param path Character string specifying the path to the GnuCash XML
#'   (`.gnucash`) file.
#' @param practice_id Character or integer practice identifier added to every
#'   row of the output.
#' @param account_map A tibble of account mapping rules as returned by
#'   \code{default_account_map()}. Override to customize category assignments
#'   for a specific practice.
#'
#' @return A tibble with columns: \code{practice_id}, \code{date},
#'   \code{week_start}, \code{month}, \code{year}, \code{full_account_name},
#'   \code{account_name}, \code{description}, \code{amount}, \code{category},
#'   and \code{source} (`"gnucash_xml"`). The \code{is_refund} column is added
#'   downstream by \code{filter_gnucash_overhead()} and
#'   \code{normalize_gnucash_income()}.
#'
#' @section Sign convention:
#' GnuCash stores split amounts as signed values from the account's perspective:
#' EXPENSE splits carry a positive amount when money flows out, and INCOME
#' splits carry a negative amount when money flows in (credits). This function
#' negates INCOME split amounts so that normal revenue appears as a positive
#' value, consistent with the rest of the package. Chargebacks (positive INCOME
#' splits) emerge as negative values and are flagged by
#' \code{validate_income()}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' transactions <- ingest_gnucash_xml(
#'   path        = "path/to/ledger.gnucash",
#'   practice_id = "practice_001"
#' )
#'
#' income   <- normalize_gnucash_income(transactions)
#' overhead <- filter_gnucash_overhead(transactions)
#' }
ingest_gnucash_xml <- function(
  path,
  practice_id,
  account_map = default_account_map()
) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    rlang::abort(
      paste0(
        "Package 'xml2' is required to read GnuCash XML files.\n",
        "Install it with: install.packages(\"xml2\")"
      ),
      class = "dcForecastR_missing_package"
    )
  }

  doc <- xml2::read_xml(path)
  ns <- .gnucash_ns()

  accounts <- .gnucash_accounts(doc, ns)
  name_cache <- new.env(hash = TRUE, parent = emptyenv())
  trn_nodes <- xml2::xml_find_all(doc, "//gnc:transaction", ns)

  if (length(trn_nodes) == 0L) {
    rlang::abort(
      "No transactions found in the GnuCash XML file.",
      class = "dcForecastR_no_data"
    )
  }

  rows <- vector("list", length(trn_nodes) * 4L)
  row_n <- 0L

  for (trn in trn_nodes) {
    date_raw <- xml2::xml_text(
      xml2::xml_find_first(trn, "trn:date-posted/ts:date", ns)
    )
    desc <- xml2::xml_text(xml2::xml_find_first(trn, "trn:description", ns))

    for (sp in xml2::xml_find_all(trn, "trn:splits/trn:split", ns)) {
      acct_guid <- xml2::xml_text(xml2::xml_find_first(sp, "split:account", ns))
      acct <- accounts[[acct_guid]]
      if (is.null(acct) || !acct$type %in% c("EXPENSE", "INCOME")) {
        next
      }

      amount <- .gnucash_amount(
        xml2::xml_text(xml2::xml_find_first(sp, "split:value", ns))
      )
      # INCOME splits are negative credits; negate so normal revenue is positive.
      if (acct$type == "INCOME") {
        amount <- -amount
      }

      row_n <- row_n + 1L
      rows[[row_n]] <- list(
        date = date_raw,
        full_account_name = .gnucash_full_name(acct_guid, accounts, name_cache),
        account = acct$name,
        description = desc,
        amount = amount
      )
    }
  }

  if (row_n == 0L) {
    rlang::abort(
      "No EXPENSE or INCOME splits found in the GnuCash XML file.",
      class = "dcForecastR_no_data"
    )
  }

  rows <- rows[seq_len(row_n)]

  raw_tbl <- tibble::tibble(
    date = vapply(rows, `[[`, character(1L), "date"),
    full_account_name = vapply(rows, `[[`, character(1L), "full_account_name"),
    account = vapply(rows, `[[`, character(1L), "account"),
    description = vapply(rows, `[[`, character(1L), "description"),
    amount = vapply(rows, `[[`, double(1L), "amount")
  )

  # Apply account mapping (adds `category` column)
  mapped_tbl <- map_accounts(raw_tbl, account_map)

  # XML timestamps: "2025-01-15 00:00:00 +0000" -- extract date portion only
  mapped_tbl |>
    dplyr::mutate(
      practice_id = practice_id,
      source = "gnucash_xml",
      date = lubridate::ymd(substr(date, 1L, 10L)),
      week_start = lubridate::floor_date(date, "week", week_start = 1L),
      month = lubridate::month(date),
      year = lubridate::year(date)
    ) |>
    dplyr::rename(account_name = "account") |>
    dplyr::relocate(
      practice_id,
      date,
      week_start,
      month,
      year,
      full_account_name,
      account_name,
      description,
      amount,
      category,
      source
    )
}


# Internal helpers ------------------------------------------------------------

# GnuCash XML namespace map used in all XPath queries.
.gnucash_ns <- function() {
  c(
    gnc = "http://www.gnucash.org/XML/gnc",
    act = "http://www.gnucash.org/XML/act",
    trn = "http://www.gnucash.org/XML/trn",
    ts = "http://www.gnucash.org/XML/ts",
    split = "http://www.gnucash.org/XML/split"
  )
}


# Parse all <gnc:account> elements into a named list keyed by GUID.
.gnucash_accounts <- function(doc, ns) {
  nodes <- xml2::xml_find_all(doc, "//gnc:account", ns)
  result <- vector("list", length(nodes))
  guids <- character(length(nodes))

  for (i in seq_along(nodes)) {
    n <- nodes[[i]]
    g <- xml2::xml_text(xml2::xml_find_first(n, "act:id", ns))
    result[[i]] <- list(
      guid = g,
      name = xml2::xml_text(xml2::xml_find_first(n, "act:name", ns)),
      type = xml2::xml_text(xml2::xml_find_first(n, "act:type", ns)),
      parent = xml2::xml_text(xml2::xml_find_first(n, "act:parent", ns))
    )
    guids[[i]] <- g
  }

  names(result) <- guids
  result
}


# Build the colon-separated full account name by traversing the parent chain.
# ROOT accounts contribute "" (empty string) as a sentinel that is excluded
# from the path. Results are memoised in `cache`.
.gnucash_full_name <- function(guid, accounts, cache) {
  if (exists(guid, envir = cache, inherits = FALSE)) {
    return(get(guid, envir = cache, inherits = FALSE))
  }

  acct <- accounts[[guid]]
  result <- if (is.null(acct) || acct$type == "ROOT") {
    ""
  } else if (is.na(acct$parent) || !acct$parent %in% names(accounts)) {
    acct$name
  } else {
    parent_full <- .gnucash_full_name(acct$parent, accounts, cache)
    if (nchar(parent_full) == 0L) {
      acct$name
    } else {
      paste0(parent_full, ":", acct$name)
    }
  }

  assign(guid, result, envir = cache, inherits = FALSE)
  result
}


# Parse a GnuCash rational amount ("120000/100") to a double.
.gnucash_amount <- function(x) {
  if (grepl("/", x, fixed = TRUE)) {
    parts <- strsplit(x, "/", fixed = TRUE)[[1L]]
    as.numeric(parts[[1L]]) / as.numeric(parts[[2L]])
  } else {
    as.numeric(x)
  }
}
