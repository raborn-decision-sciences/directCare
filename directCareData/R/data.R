#' County-Level Market Data
#'
#' One row per US county, combining Census ACS population and income
#' estimates, SAHIE uninsured population estimates, and NPPES-derived
#' physician counts. See [data_provenance] for the vintage of each
#' underlying source.
#'
#' @format A tibble with one row per county and the following columns:
#' \describe{
#'   \item{county_fips}{Character. 5-digit county FIPS code.}
#'   \item{county_name}{Character. County name.}
#'   \item{state_abb}{Character. 2-letter state abbreviation.}
#'   \item{state_fips}{Character. 2-digit state FIPS code.}
#'   \item{cbsa_fips}{Character. Core Based Statistical Area (metro/micro)
#'     FIPS code, \code{NA} if the county is not part of a CBSA.}
#'   \item{cbsa_title}{Character. CBSA name, \code{NA} if not part of a
#'     CBSA.}
#'   \item{population}{Integer. Total population (ACS 5-year estimate).}
#'   \item{median_household_income}{Numeric. Median household income in
#'     dollars (ACS 5-year estimate).}
#'   \item{uninsured_count}{Integer. Estimated uninsured population
#'     (SAHIE).}
#'   \item{uninsured_rate}{Numeric. Estimated uninsured rate, 0-1 (SAHIE).}
#'   \item{physician_count}{Integer. Count of physicians (NPPES, individual
#'     providers with an allopathic/osteopathic taxonomy code) with a
#'     practice address ZIP primarily located in this county.}
#'   \item{physician_density_per_10k}{Numeric. Physicians per 10,000
#'     population.}
#' }
#' @source U.S. Census Bureau American Community Survey (ACS) 5-year
#'   estimates; Small Area Health Insurance Estimates (SAHIE); CMS National
#'   Plan and Provider Enumeration System (NPPES).
"county_market_data"

#' ZIP Code to County Crosswalk
#'
#' Maps 5-digit ZIP codes to the county FIPS codes they overlap, with an
#' area-overlap fraction. A ZIP that spans multiple counties has one row
#' per overlapping county; exactly one row per ZIP has `is_primary = TRUE`.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{zip}{Character. 5-digit ZIP code.}
#'   \item{county_fips}{Character. 5-digit county FIPS code.}
#'   \item{overlap_pct}{Numeric. Fraction (0-1) of the ZIP's land area
#'     falling within this county.}
#'   \item{is_primary}{Logical. \code{TRUE} for the county with the
#'     largest overlap for this ZIP.}
#' }
#' @source U.S. Census Bureau 2020 ZCTA-to-County Relationship File.
"zip_county_crosswalk"

#' County to Metro Area (CBSA) Crosswalk
#'
#' Maps counties to their Core Based Statistical Area (CBSA), if any, and
#' provides a county-name lookup for name-based (as opposed to ZIP-based)
#' geography resolution.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{county_fips}{Character. 5-digit county FIPS code.}
#'   \item{county_name}{Character. County name.}
#'   \item{state_abb}{Character. 2-letter state abbreviation.}
#'   \item{cbsa_fips}{Character. CBSA FIPS code, \code{NA} if the county is
#'     not part of a CBSA.}
#'   \item{cbsa_title}{Character. CBSA name, \code{NA} if not part of a
#'     CBSA.}
#'   \item{metro_micro}{Character. \code{"metro"} or \code{"micro"},
#'     \code{NA} if not part of a CBSA.}
#' }
#' @source U.S. Census Bureau / OMB Core Based Statistical Area delineation
#'   file.
"county_cbsa_crosswalk"

#' Direct Care Practice Landscape
#'
#' A curated directory of existing direct care practices, used to
#' summarize local competition for a prospective practice location.
#' Currently ships as an empty, zero-row placeholder pending a real data
#' source.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{county_fips}{Character. 5-digit county FIPS code.}
#'   \item{practice_name}{Character. Practice name.}
#'   \item{practice_type}{Character. e.g. \code{"membership"},
#'     \code{"hybrid"}, \code{"fee_for_service"}.}
#'   \item{estimated_panel_size}{Integer. Estimated panel size, \code{NA}
#'     if unknown.}
#'   \item{city}{Character. Practice city.}
#'   \item{state_abb}{Character. 2-letter state abbreviation.}
#'   \item{source}{Character. Provenance of this specific row.}
#'   \item{as_of_date}{Date. Date this row's information was current as
#'     of, \code{NA} if unknown.}
#' }
#' @source Internally curated; no source identified yet.
"direct_care_landscape"

#' Data Provenance Metadata
#'
#' Records the vintage, retrieval date, and source URL for each underlying
#' data source bundled in this package.
#'
#' @format A named list with elements \code{acs}, \code{sahie},
#'   \code{nppes}, \code{geography_crosswalks}, and
#'   \code{direct_care_landscape}, each itself a list with:
#' \describe{
#'   \item{vintage}{Character. Human-readable description of the data
#'     vintage (e.g. \code{"2018-2022 ACS 5-year"}).}
#'   \item{retrieved_on}{Date. Date the data-raw pipeline retrieved this
#'     source.}
#'   \item{source_url}{Character. URL of the source.}
#' }
"data_provenance"
