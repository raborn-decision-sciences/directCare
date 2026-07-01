# Orchestrates the full market data refresh: sources each source-specific
# script in order, joins their outputs into county_market_data, assembles
# data_provenance, and writes the results to data/*.rda via usethis::use_data().
#
# Run this from the package root (e.g. via devtools::load_all() or with
# the working directory set to directCareData/) after setting
# CENSUS_API_KEY. Re-running a single source's script and re-joining by
# hand is fine for a targeted refresh (e.g. a monthly NPPES-only update);
# this orchestrator is for a full rebuild.

if (!nzchar(Sys.getenv("CENSUS_API_KEY"))) {
  stop(
    "CENSUS_API_KEY is not set. Sign up for a free key at ",
    "https://api.census.gov/data/key_signup.html and set it as an ",
    "environment variable before running this script."
  )
}

tidycensus::census_api_key(Sys.getenv("CENSUS_API_KEY"), install = FALSE)

source("data-raw/01_geography_crosswalks.R")
source("data-raw/02_acs_population_income.R")
source("data-raw/03_sahie_uninsured.R")
source("data-raw/04_nppes_physicians.R")
source("data-raw/05_direct_care_landscape.R")

# acs_county enumerates every US county/equivalent; cbsa_delineation only
# covers the subset that participate in a CBSA. Join CBSA info onto the
# full ACS spine (not the other way around) so non-metro counties are
# retained with cbsa_fips/cbsa_title/metro_micro correctly NA, rather than
# silently dropped.
county_cbsa_crosswalk <- acs_county |>
  dplyr::select(county_fips, county_name, state_abb) |>
  dplyr::left_join(cbsa_delineation, by = "county_fips")

county_market_data <- acs_county |>
  dplyr::left_join(cbsa_delineation, by = "county_fips") |>
  dplyr::left_join(sahie_county, by = "county_fips") |>
  dplyr::left_join(nppes_county, by = "county_fips") |>
  dplyr::mutate(
    state_fips = substr(county_fips, 1, 2),
    physician_count = dplyr::coalesce(physician_count, 0L),
    physician_density_per_10k = dplyr::if_else(
      population > 0,
      physician_count / population * 10000,
      NA_real_
    )
  ) |>
  dplyr::select(
    county_fips,
    county_name,
    state_abb,
    state_fips,
    cbsa_fips,
    cbsa_title,
    population,
    median_household_income,
    uninsured_count,
    uninsured_rate,
    physician_count,
    physician_density_per_10k
  )

data_provenance <- list(
  acs = acs_provenance,
  sahie = sahie_provenance,
  nppes = nppes_provenance,
  geography_crosswalks = geography_crosswalks_provenance,
  direct_care_landscape = direct_care_landscape_provenance
)

cat("county_market_data:", nrow(county_market_data), "rows\n")
cat("zip_county_crosswalk:", nrow(zip_county_crosswalk), "rows\n")
cat("county_cbsa_crosswalk:", nrow(county_cbsa_crosswalk), "rows\n")
cat("direct_care_landscape:", nrow(direct_care_landscape), "rows\n")

usethis::use_data(
  county_market_data,
  zip_county_crosswalk,
  county_cbsa_crosswalk,
  direct_care_landscape,
  data_provenance,
  overwrite = TRUE
)
