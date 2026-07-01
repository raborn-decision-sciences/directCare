#' Refresh the Cached External Market Data
#'
#' Downloads and pre-processes Census ACS, SAHIE, NPPES, and direct care
#' directory data, and writes it to the package's local cache. Intended to
#' be run periodically (e.g. annually, when Census/SAHIE data updates)
#' rather than on every call to the market context functions.
#'
#' @param sources Character vector of data sources to refresh. Defaults to
#'   all sources (`"acs"`, `"sahie"`, `"nppes"`, `"landscape"`).
#'
#' @return Invisibly, a character vector of the cache files written.
#'
#' @export
refresh_market_data_cache <- function(sources = c("acs", "sahie", "nppes", "landscape")) {
  rlang::abort(
    "refresh_market_data_cache() is not yet implemented.",
    class = "dcPlanR_not_implemented"
  )
}

#' Load a Cached Market Data Source
#'
#' Internal helper used by the `market_context.R` functions to read a
#' pre-processed data source from the local cache written by
#' [refresh_market_data_cache()].
#'
#' @param source Character string identifying the data source: one of
#'   `"acs"`, `"sahie"`, `"nppes"`, `"landscape"`.
#'
#' @return The cached data source object.
#'
#' @noRd
load_cached_source <- function(source) {
  rlang::abort(
    "load_cached_source() is not yet implemented.",
    class = "dcPlanR_not_implemented"
  )
}
