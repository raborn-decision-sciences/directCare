# Pulls county-level uninsured population estimates from the Census
# Small Area Health Insurance Estimates (SAHIE) API. tidycensus does not
# wrap SAHIE, so this hits the API directly with httr2, reusing
# CENSUS_API_KEY.
#
# SAHIE demographic filters (AGECAT/RACECAT/SEXCAT/IPRCAT, all uppercase
# in the API) select which population slice a row describes. Using 0 for
# each selects the broadest available slice: AGECAT=0 is "Under 65 years"
# (SAHIE's broadest age category -- it does not estimate uninsured rates
# for 65+, who are overwhelmingly covered by Medicare), RACECAT=0 is "All
# Races", SEXCAT=0 is "Both Sexes", IPRCAT=0 is "All Incomes". Verify
# these codes against the current SAHIE variable documentation
# (https://api.census.gov/data/timeseries/healthins/sahie/variables.html)
# before re-running, as SAHIE's category coding has changed across vintages.

library(dplyr)

sahie_year <- 2022 # latest available SAHIE vintage at time of writing.

sahie_resp <- httr2::request("https://api.census.gov/data/timeseries/healthins/sahie") |>
  httr2::req_url_query(
    get = "NAME,NUI_PT,PCTUI_PT",
    `for` = "county:*",
    `in` = "state:*",
    time = sahie_year,
    AGECAT = 0,
    RACECAT = 0,
    SEXCAT = 0,
    IPRCAT = 0,
    key = Sys.getenv("CENSUS_API_KEY")
  ) |>
  httr2::req_perform()

sahie_json <- httr2::resp_body_json(sahie_resp, simplifyVector = TRUE)
sahie_header <- sahie_json[1, ]
sahie_body <- as.data.frame(sahie_json[-1, ], stringsAsFactors = FALSE)
names(sahie_body) <- sahie_header

sahie_county <- sahie_body |>
  transmute(
    county_fips = paste0(state, county),
    uninsured_count = as.integer(NUI_PT),
    uninsured_rate = as.numeric(PCTUI_PT) / 100
  )

sahie_provenance <- list(
  vintage = paste0(sahie_year, " (under 65 years)"),
  retrieved_on = Sys.Date(),
  source_url = "https://www.census.gov/data/developers/data-sets/sahie-health-insurance.html"
)
