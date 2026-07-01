# Builds zip_county_crosswalk and cbsa_delineation.
#
# ZIP -> county: Census Bureau ZCTA-to-County Relationship File. Preferred
# over HUD's USPS ZIP crosswalk because it's a plain public URL with no
# account/login required, and it ships an area-overlap measure we can use
# to pick a "primary" county for ZIPs that span more than one.
#
# County -> metro: Census/OMB Core Based Statistical Area (CBSA)
# delineation file. Published periodically (most recently 2023); check
# https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html
# for the current file before re-running, as the exact URL/filename below
# may drift between OMB delineation vintages.
#
# Note: the delineation file only lists counties that ARE part of some
# CBSA (~1,900 of the ~3,140 US counties/equivalents) -- it is not a full
# county list. It's kept here as a lookup keyed by county_fips; the full
# county universe (all counties, with cbsa fields NA where not
# applicable) is assembled in 00_market_data.R from the ACS county pull,
# which does enumerate every county.

library(dplyr)

# -- ZIP -> county ------------------------------------------------------------

zcta_county_url <- paste0(
  "https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/",
  "tab20_zcta520_county20_natl.txt"
)

zcta_county_raw <- readr::read_delim(
  zcta_county_url,
  delim = "|",
  col_types = readr::cols(.default = "c")
)

# Column names as published in the 2020 ZCTA relationship file. Verify
# against the actual header if the Census file layout has changed.
zip_county_crosswalk <- zcta_county_raw |>
  transmute(
    zip = GEOID_ZCTA5_20,
    county_fips = GEOID_COUNTY_20,
    area_land_part = as.numeric(AREALAND_PART)
  ) |>
  filter(!is.na(zip), !is.na(county_fips)) |>
  group_by(zip) |>
  mutate(
    overlap_pct = area_land_part / sum(area_land_part),
    is_primary = row_number(desc(overlap_pct)) == 1
  ) |>
  ungroup() |>
  select(zip, county_fips, overlap_pct, is_primary)

# -- county -> CBSA/metro ------------------------------------------------------

cbsa_delineation_url <- paste0(
  "https://www2.census.gov/programs-surveys/metro-micro/geographies/",
  "reference-files/2023/delineation-files/list1_2023.xlsx"
)

cbsa_delineation_path <- withr::local_tempfile(fileext = ".xlsx")
utils::download.file(cbsa_delineation_url, cbsa_delineation_path, mode = "wb")

# The published delineation file has a couple of banner/title rows before
# the real header; skip count should be verified against the current file.
cbsa_raw <- readxl::read_excel(cbsa_delineation_path, skip = 2)

cbsa_delineation <- cbsa_raw |>
  transmute(
    county_fips = paste0(`FIPS State Code`, `FIPS County Code`),
    cbsa_fips = `CBSA Code`,
    cbsa_title = `CBSA Title`,
    metro_micro = dplyr::case_when(
      grepl("^Metro", `Metropolitan/Micropolitan Statistical Area`) ~ "metro",
      grepl("^Micro", `Metropolitan/Micropolitan Statistical Area`) ~ "micro",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(county_fips))

geography_crosswalks_provenance <- list(
  vintage = "2020 ZCTA relationship file; 2023 OMB CBSA delineation",
  retrieved_on = Sys.Date(),
  source_url = zcta_county_url
)
