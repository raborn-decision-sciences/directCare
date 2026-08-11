#' Cached, keyed indices for `resolve_geography()`'s two default
#' crosswalk/geo tables
#'
#' Package-level cache, built once (lazily, on first use) and reused for
#' the life of the R process -- named-list lookups (built via `split()`)
#' instead of the linear `df$col == x` vector-equality scan
#' `resolve_geography()` otherwise does on every call. Only ever consulted
#' when the caller uses the package's own default `crosswalk`/`geo` data
#' (detected via `missing()` in `resolve_geography()` itself) -- a
#' caller-supplied table (e.g. every test in this package, via its own
#' fixture data) always takes the original, unindexed path unchanged,
#' since there's no way to know a caller-supplied table is stable across
#' calls the way the package's own static data is.
#' @noRd
.geo_index_env <- new.env(parent = emptyenv())

#' @noRd
.zip_index <- function(crosswalk) {
  if (is.null(.geo_index_env$zip_index)) {
    .geo_index_env$zip_index <- split(seq_len(nrow(crosswalk)), crosswalk$zip)
  }
  .geo_index_env$zip_index
}

#' @noRd
.county_name_index <- function(geo) {
  if (is.null(.geo_index_env$county_name_index)) {
    normalized <- tolower(trimws(sub("\\s+county$", "", geo$county_name, ignore.case = TRUE)))
    .geo_index_env$county_name_index <- split(seq_len(nrow(geo)), normalized)
  }
  .geo_index_env$county_name_index
}

#' @noRd
.county_fips_index <- function(geo) {
  if (is.null(.geo_index_env$county_fips_index)) {
    idx <- seq_len(nrow(geo))
    names(idx) <- geo$county_fips
    .geo_index_env$county_fips_index <- idx
  }
  .geo_index_env$county_fips_index
}

#' Resolve a Geography to FIPS and Metro Identifiers
#'
#' Takes a county or ZIP code input and resolves it to the FIPS codes and
#' metro area identifiers used to look up Census, SAHIE, NPPES, and direct
#' care landscape data.
#'
#' @param location Character string: a 5-digit ZIP code or a county name
#'   (with or without a trailing "County").
#' @param state Optional two-letter state abbreviation, used to disambiguate
#'   county names that are not unique nationally. Ignored when `location`
#'   is a ZIP code.
#' @param crosswalk ZIP-to-county crosswalk to resolve against. Defaults to
#'   [directCareData::zip_county_crosswalk]; overridable for testing.
#' @param geo County-to-metro crosswalk (also used for county-name lookup)
#'   to resolve against. Defaults to [directCareData::county_cbsa_crosswalk];
#'   overridable for testing.
#'
#' @return A `dcPlanR_geography`-classed list with `county_fips`,
#'   `state_fips`, `metro_fips` (`NA` if the geography is not part of a
#'   metro area), `county_name`, and `state_abb`.
#'
#' @export
resolve_geography <- function(
  location,
  state = NULL,
  crosswalk = directCareData::zip_county_crosswalk,
  geo = directCareData::county_cbsa_crosswalk
) {
  # Only the package's own default tables are stable across calls (static
  # package data); a caller-supplied crosswalk/geo (every test in this
  # package, via its own fixture data) skips the cache entirely and keeps
  # the original per-call scan, unchanged.
  use_index <- missing(crosswalk) && missing(geo)

  if (!rlang::is_scalar_character(location) || is.na(location)) {
    rlang::abort(
      "`location` must be a single, non-missing character string.",
      class = "dcPlanR_invalid_fips"
    )
  }

  if (grepl("^[0-9]{5}$", location)) {
    matches <- if (use_index) {
      crosswalk[.zip_index(crosswalk)[[location]], , drop = FALSE]
    } else {
      crosswalk[crosswalk$zip == location, ]
    }
    if (nrow(matches) == 0L) {
      rlang::abort(
        paste0("No county found for ZIP code '", location, "'."),
        class = "dcPlanR_zip_not_found"
      )
    }
    if (nrow(matches) > 1L) {
      rlang::inform(
        paste0(
          "ZIP code '", location, "' spans multiple counties; using the ",
          "primary (largest-overlap) county."
        ),
        class = "dcPlanR_zip_multi_county"
      )
    }
    county_fips <- matches$county_fips[matches$is_primary][1]
  } else {
    normalized <- tolower(trimws(sub("\\s+county$", "", location, ignore.case = TRUE)))
    candidates <- if (use_index) {
      geo[.county_name_index(geo)[[normalized]], , drop = FALSE]
    } else {
      geo_normalized <- tolower(trimws(sub("\\s+county$", "", geo$county_name, ignore.case = TRUE)))
      geo[geo_normalized == normalized, ]
    }
    if (!is.null(state)) {
      candidates <- candidates[toupper(candidates$state_abb) == toupper(state), ]
    }
    if (nrow(candidates) == 0L) {
      rlang::abort(
        paste0("No county found matching '", location, "'."),
        class = "dcPlanR_county_not_found"
      )
    }
    if (nrow(candidates) > 1L) {
      rlang::abort(
        paste0(
          "'", location, "' matches counties in multiple states (",
          paste(unique(candidates$state_abb), collapse = ", "),
          "). Pass `state` to disambiguate."
        ),
        class = "dcPlanR_county_ambiguous"
      )
    }
    county_fips <- candidates$county_fips[1]
  }

  geo_row <- if (use_index) {
    geo[.county_fips_index(geo)[county_fips], , drop = FALSE]
  } else {
    geo[geo$county_fips == county_fips, ]
  }

  structure(
    list(
      county_fips = county_fips,
      state_fips = substr(county_fips, 1, 2),
      metro_fips = geo_row$cbsa_fips[1],
      county_name = geo_row$county_name[1],
      state_abb = geo_row$state_abb[1]
    ),
    class = "dcPlanR_geography"
  )
}

#' Validate a FIPS Code Argument
#'
#' @param fips Value to validate.
#'
#' @noRd
.validate_fips <- function(fips) {
  if (!rlang::is_scalar_character(fips) || is.na(fips) || !grepl("^[0-9]{5}$", fips)) {
    rlang::abort(
      "`fips` must be a single 5-digit character string.",
      class = "dcPlanR_invalid_fips"
    )
  }
  invisible(fips)
}

#' Look Up a County's Market Data Row
#'
#' @param fips Validated 5-digit county FIPS code.
#' @param data County-level market data to look up against.
#'
#' @noRd
.lookup_county <- function(fips, data) {
  row <- data[data$county_fips == fips, ]
  if (nrow(row) == 0L) {
    rlang::abort(
      paste0("No market data found for county FIPS '", fips, "'."),
      class = "dcPlanR_fips_not_found"
    )
  }
  row
}

#' Summarize Population and Income for a Geography
#'
#' Pulls population and household income summary statistics from the Census
#' American Community Survey (ACS) for the given geography.
#'
#' @param fips Character FIPS code, as returned by [resolve_geography()].
#' @param data County-level market data to look up against. Defaults to
#'   [directCareData::county_market_data]; overridable for testing.
#'
#' @return A `dcPlanR_population_income`-classed list with `county_fips`,
#'   `population`, and `median_household_income`.
#'
#' @export
get_population_income <- function(fips, data = directCareData::county_market_data) {
  .validate_fips(fips)
  row <- .lookup_county(fips, data)

  structure(
    list(
      county_fips = fips,
      population = row$population[1],
      median_household_income = row$median_household_income[1]
    ),
    class = "dcPlanR_population_income"
  )
}

#' Estimate the Uninsured Population for a Geography
#'
#' Pulls uninsured population estimates from the Small Area Health Insurance
#' Estimates (SAHIE) program for the given geography.
#'
#' @param fips Character FIPS code, as returned by [resolve_geography()].
#' @param data County-level market data to look up against. Defaults to
#'   [directCareData::county_market_data]; overridable for testing.
#'
#' @return A `dcPlanR_uninsured_estimate`-classed list with `county_fips`,
#'   `uninsured_count`, and `uninsured_rate`.
#'
#' @export
get_uninsured_estimate <- function(fips, data = directCareData::county_market_data) {
  .validate_fips(fips)
  row <- .lookup_county(fips, data)

  structure(
    list(
      county_fips = fips,
      uninsured_count = row$uninsured_count[1],
      uninsured_rate = row$uninsured_rate[1]
    ),
    class = "dcPlanR_uninsured_estimate"
  )
}

#' Estimate Physician Density for a Geography
#'
#' Derives a physician-per-population density figure from pre-processed
#' NPPES bulk data.
#'
#' @param fips Character FIPS code, as returned by [resolve_geography()].
#' @param data County-level market data to look up against. Defaults to
#'   [directCareData::county_market_data]; overridable for testing.
#'
#' @return A `dcPlanR_physician_density`-classed list with `county_fips`,
#'   `physician_count`, and `physician_density_per_10k`.
#'
#' @export
get_physician_density <- function(fips, data = directCareData::county_market_data) {
  .validate_fips(fips)
  row <- .lookup_county(fips, data)

  structure(
    list(
      county_fips = fips,
      physician_count = row$physician_count[1],
      physician_density_per_10k = row$physician_density_per_10k[1]
    ),
    class = "dcPlanR_physician_density"
  )
}

#' Summarize the Direct Care Practice Landscape for a Geography
#'
#' Pulls existing direct care practices from an internally curated directory
#' for the given geography, to give a sense of local competition. A county
#' with zero known practices is a valid result, not an error.
#'
#' @param fips Character FIPS code, as returned by [resolve_geography()].
#' @param data Practice landscape data to filter. Defaults to
#'   [directCareData::direct_care_landscape]; overridable for testing.
#'
#' @return A tibble of nearby direct care practices, one row per practice
#'   (zero rows if none are known for this county).
#'
#' @export
get_direct_care_landscape <- function(fips, data = directCareData::direct_care_landscape) {
  .validate_fips(fips)
  data[data$county_fips == fips, ]
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
#' @return A `dcPlanR_market_context`-classed list with `geography`,
#'   `population_income`, `uninsured`, `physician_density`, `landscape`,
#'   and `provenance` elements.
#'
#' @export
build_market_context <- function(location, state = NULL) {
  geography <- resolve_geography(location, state = state)

  structure(
    list(
      geography = geography,
      population_income = get_population_income(geography$county_fips),
      uninsured = get_uninsured_estimate(geography$county_fips),
      physician_density = get_physician_density(geography$county_fips),
      landscape = get_direct_care_landscape(geography$county_fips),
      provenance = directCareData::data_provenance
    ),
    class = "dcPlanR_market_context"
  )
}
