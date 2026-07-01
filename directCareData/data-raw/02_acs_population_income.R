# Pulls county-level population and median household income from the
# Census American Community Survey (ACS) 5-year estimates via tidycensus.
# Requires CENSUS_API_KEY to be set and tidycensus::census_api_key() to
# have been called (done by the 00_market_data.R orchestrator).

library(dplyr)

acs_year <- 2022 # latest available 5-year ACS vintage at time of writing;
# bump this and re-run when a newer vintage is released.

acs_raw <- tidycensus::get_acs(
  geography = "county",
  variables = c(
    population = "B01003_001",
    median_household_income = "B19013_001"
  ),
  year = acs_year,
  survey = "acs5"
)

acs_county <- acs_raw |>
  select(GEOID, variable, estimate) |>
  tidyr::pivot_wider(names_from = variable, values_from = estimate) |>
  rename(county_fips = GEOID) |>
  mutate(population = as.integer(population))

acs_provenance <- list(
  vintage = paste0(acs_year - 4, "-", acs_year, " ACS 5-year"),
  retrieved_on = Sys.Date(),
  source_url = "https://www.census.gov/data/developers/data-sets/acs-5year.html"
)
