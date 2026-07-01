# One-time, dev-only script: builds empty-but-correctly-typed placeholder
# versions of every exported dataset, so the package documents/installs/
# checks cleanly before the real data-raw pipeline (00_market_data.R) has
# been run. Running 00_market_data.R later overwrites these with real
# data via usethis::use_data(..., overwrite = TRUE) -- this script should
# not need to be run again after that.

county_market_data <- tibble::tibble(
  county_fips = character(),
  county_name = character(),
  state_abb = character(),
  state_fips = character(),
  cbsa_fips = character(),
  cbsa_title = character(),
  population = integer(),
  median_household_income = double(),
  uninsured_count = integer(),
  uninsured_rate = double(),
  physician_count = integer(),
  physician_density_per_10k = double()
)

zip_county_crosswalk <- tibble::tibble(
  zip = character(),
  county_fips = character(),
  overlap_pct = double(),
  is_primary = logical()
)

county_cbsa_crosswalk <- tibble::tibble(
  county_fips = character(),
  county_name = character(),
  state_abb = character(),
  cbsa_fips = character(),
  cbsa_title = character(),
  metro_micro = character()
)

# direct_care_landscape has no external dependency, so reuse its real
# script rather than redefining the same schema here.
source("data-raw/05_direct_care_landscape.R")

not_yet_generated <- list(
  vintage = "not yet generated",
  retrieved_on = as.Date(NA),
  source_url = NA_character_
)
data_provenance <- list(
  acs = not_yet_generated,
  sahie = not_yet_generated,
  nppes = not_yet_generated,
  geography_crosswalks = not_yet_generated,
  direct_care_landscape = not_yet_generated
)

usethis::use_data(
  county_market_data,
  zip_county_crosswalk,
  county_cbsa_crosswalk,
  direct_care_landscape,
  data_provenance,
  overwrite = TRUE
)
