# Builds direct_care_landscape from a manual CSV export of the DPC
# Frontier interactive map (https://mapper.dpcfrontier.com/), a public
# directory of direct primary care and hybrid practices. Unlike 01-04,
# there is no stable programmatic API for this source -- the mapper is
# exported by hand, so the export is checked into data-raw/ as a dated
# snapshot rather than re-fetched at run time. To refresh: pull a new
# export from the mapper, save it as
# data-raw/dpc_frontier_export_<YYYY-MM-DD>.csv, and update
# `dpc_export_path`/`dpc_export_date` below.
#
# The export has only latitude/longitude, a practice-type classification
# (dpc_type), and an "onsite" employer-clinic flag. `onsite` is dropped
# on ingestion -- get_direct_care_landscape() and everything downstream
# of it (report.typ, mod_results.R) only ever call nrow() on this table
# today, so there's no consumer for that distinction yet. The export
# also has no practice name, city, panel size, or per-row date, so those
# columns are NA; the table is still fully functional for its only
# current use (a per-county competitor count), since
# get_direct_care_landscape() filters exclusively on county_fips.
#
# county_fips is derived by a point-in-polygon spatial join against
# Census county boundaries (tigris). The vintage year is hardcoded to
# match acs_year in 02_acs_population_income.R (rather than relying on
# that script having already been sourced) so FIPS codes stay consistent
# across tables that get joined/filtered on county_fips, and so this
# script still runs standalone -- check both if either vintage changes.

dpc_export_path <- "data-raw/dpc_frontier_export_2026-07-30.csv"
dpc_export_date <- as.Date("2026-07-30")
county_boundary_year <- 2022 # keep in sync with acs_year in 02_acs_population_income.R

dpc_raw <- readr::read_csv(
  dpc_export_path,
  col_types = readr::cols(
    latitude = readr::col_double(),
    longitude = readr::col_double(),
    dpc_type = readr::col_character(),
    onsite = readr::col_character()
  )
)

dpc_points <- sf::st_as_sf(
  dpc_raw,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

county_boundaries <- tigris::counties(
  cb = TRUE,
  resolution = "500k",
  year = county_boundary_year,
  progress_bar = FALSE
) |>
  sf::st_transform(4326) |>
  dplyr::select(county_fips = GEOID, state_abb = STUSPS)

dpc_joined <- sf::st_join(dpc_points, county_boundaries, join = sf::st_within)

n_unmatched <- sum(is.na(dpc_joined$county_fips))
if (n_unmatched > 0) {
  warning(
    n_unmatched,
    " practice location(s) did not fall within any county polygon ",
    "(likely invalid/offshore coordinates) and were dropped."
  )
}

direct_care_landscape <- dpc_joined |>
  sf::st_drop_geometry() |>
  dplyr::filter(!is.na(county_fips)) |>
  dplyr::transmute(
    county_fips,
    practice_name = NA_character_,
    practice_type = dpc_type,
    estimated_panel_size = NA_integer_,
    city = NA_character_,
    state_abb,
    source = "https://mapper.dpcfrontier.com/",
    as_of_date = dpc_export_date
  )

direct_care_landscape_provenance <- list(
  vintage = paste0("DPC Frontier mapper export, ", format(dpc_export_date)),
  retrieved_on = dpc_export_date,
  source_url = "https://mapper.dpcfrontier.com/"
)
