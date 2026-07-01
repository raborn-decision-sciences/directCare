# Pulls county-level uninsured population estimates from the Census
# Small Area Health Insurance Estimates (SAHIE) API. tidycensus does not
# wrap SAHIE, so this hits the API directly with httr2, reusing
# CENSUS_API_KEY.
#
# SAHIE demographic filters: AGECAT/RACECAT/SEXCAT/IPRCAT codes select
# which population slice a row describes. "0" in each of these categories
# means "all ages" / "all races" / "both sexes" / "all income levels" —
# the broadest, most comparable slice for a practice-planning market
# summary. Verify these codes against the current SAHIE variable
# documentation (https://www.census.gov/data/developers/data-sets/sahie-health-insurance.html)
# before re-running, as SAHIE's category coding has changed across vintages.

library(dplyr)

sahie_year <- 2022 # latest available SAHIE vintage at time of writing.

sahie_resp <- httr2::request("https://api.census.gov/data/timeseries/healthins/sahie") |>
  httr2::req_url_query(
    get = "NAME,NUI_PT,PCTUI_PT",
    `for` = "county:*",
    `in` = "state:*",
    time = sahie_year,
    agecat = 0,
    racecat = 0,
    sexcat = 0,
    iprcat = 0,
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
  vintage = as.character(sahie_year),
  retrieved_on = Sys.Date(),
  source_url = "https://www.census.gov/data/developers/data-sets/sahie-health-insurance.html"
)
