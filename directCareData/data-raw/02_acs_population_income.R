# Pulls county-level population and median household income from the
# Census American Community Survey (ACS) 5-year estimates via tidycensus.
# Requires CENSUS_API_KEY to be set and tidycensus::census_api_key() to
# have been called (done by the 00_market_data.R orchestrator).
#
# tidycensus::get_acs(geography = "county") enumerates every US county and
# county-equivalent (including DC and Puerto Rico municipios), so this
# script's output (acs_county) doubles as the authoritative full-county
# spine that 00_market_data.R joins everything else onto.

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

# base R's state.name/state.abb omits DC and territories; extend explicitly.
state_name_to_abb <- c(
  setNames(state.abb, state.name),
  "District of Columbia" = "DC",
  "Puerto Rico" = "PR"
)

acs_county <- acs_raw |>
  select(GEOID, NAME, variable, estimate) |>
  tidyr::pivot_wider(names_from = variable, values_from = estimate) |>
  rename(county_fips = GEOID) |>
  tidyr::separate(NAME, into = c("county_name", "state_name"), sep = ", ", extra = "merge") |>
  mutate(
    state_abb = unname(state_name_to_abb[state_name]),
    population = as.integer(population)
  ) |>
  select(-state_name)

acs_provenance <- list(
  vintage = paste0(acs_year - 4, "-", acs_year, " ACS 5-year"),
  retrieved_on = Sys.Date(),
  source_url = "https://www.census.gov/data/developers/data-sets/acs-5year.html"
)
