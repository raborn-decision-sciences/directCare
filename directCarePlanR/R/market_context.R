#' Resolve a Geography to FIPS and Metro Identifiers
#'
#' Takes a county or ZIP code input and resolves it to the FIPS codes and
#' metro area identifiers used to look up Census, SAHIE, NPPES, and direct
#' care landscape data.
#'
#' @param location Character string: a 5-digit ZIP code or a county name.
#' @param state Optional two-letter state abbreviation, used to disambiguate
#'   county names that are not unique nationally. Ignored when `location`
#'   is a ZIP code.
#'
#' @return A list with `county_fips`, `state_fips`, and `metro_fips`
#'   (`NA` if the geography is not part of a metro area).
#'
#' @export
resolve_geography <- function(location, state = NULL) {
  rlang::abort(
    "resolve_geography() is not yet implemented.",
    class = "dcPlanR_not_implemented"
  )
}

#' Summarize Population and Income for a Geography
#'
#' Pulls population and household income summary statistics from the Census
#' American Community Survey (ACS) for the given geography.
#'
#' @param fips Character FIPS code, as returned by [resolve_geography()].
#'
#' @return A list with population and median household income figures for
#'   the geography.
#'
#' @export
get_population_income <- function(fips) {
  rlang::abort(
    "get_population_income() is not yet implemented.",
    class = "dcPlanR_not_implemented"
  )
}

#' Estimate the Uninsured Population for a Geography
#'
#' Pulls uninsured population estimates from the Small Area Health Insurance
#' Estimates (SAHIE) program for the given geography.
#'
#' @param fips Character FIPS code, as returned by [resolve_geography()].
#'
#' @return A list with uninsured population count and rate for the geography.
#'
#' @export
get_uninsured_estimate <- function(fips) {
  rlang::abort(
    "get_uninsured_estimate() is not yet implemented.",
    class = "dcPlanR_not_implemented"
  )
}

#' Estimate Physician Density for a Geography
#'
#' Derives a physician-per-population density figure from pre-processed
#' NPPES bulk data.
#'
#' @param fips Character FIPS code, as returned by [resolve_geography()].
#'
#' @return A list with physician count and density per capita for the
#'   geography.
#'
#' @export
get_physician_density <- function(fips) {
  rlang::abort(
    "get_physician_density() is not yet implemented.",
    class = "dcPlanR_not_implemented"
  )
}

#' Summarize the Direct Care Practice Landscape for a Geography
#'
#' Pulls existing direct care practices from an internally curated directory
#' for the given geography, to give a sense of local competition.
#'
#' @param fips Character FIPS code, as returned by [resolve_geography()].
#'
#' @return A data frame of nearby direct care practices, one row per
#'   practice.
#'
#' @export
get_direct_care_landscape <- function(fips) {
  rlang::abort(
    "get_direct_care_landscape() is not yet implemented.",
    class = "dcPlanR_not_implemented"
  )
}

#' Assemble a Complete Market Context for a Geography
#'
#' Resolves the geography and calls each of the market context data
#' functions, returning a single structured object describing the market a
#' prospective practice would launch into.
#'
#' @param location Character string: a 5-digit ZIP code or a county name.
#' @param state Optional two-letter state abbreviation, used to disambiguate
#'   county names that are not unique nationally. Ignored when `location`
#'   is a ZIP code.
#'
#' @return A list with `geography`, `population_income`, `uninsured`,
#'   `physician_density`, and `landscape` elements.
#'
#' @export
build_market_context <- function(location, state = NULL) {
  geography <- resolve_geography(location, state = state)

  list(
    geography = geography,
    population_income = get_population_income(geography$county_fips),
    uninsured = get_uninsured_estimate(geography$county_fips),
    physician_density = get_physician_density(geography$county_fips),
    landscape = get_direct_care_landscape(geography$county_fips)
  )
}
