# Builds zip_county_crosswalk and county_cbsa_crosswalk.
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
    is_primary = overlap_pct == max(overlap_pct)
  ) |>
  ungroup() |>
  # Guard against ties: if two counties tie for largest overlap, keep only
  # the first as primary so exactly one is_primary row exists per zip.
  group_by(zip) |>
  mutate(
    is_primary = is_primary & !duplicated(is_primary & overlap_pct == max(overlap_pct))
  ) |>
  ungroup() |>
  select(zip, county_fips, overlap_pct, is_primary)

# -- county -> CBSA/metro ------------------------------------------------------

cbsa_delineation_url <- paste0(
  "https://www2.census.gov/programs-surveys/metro-micro/geographies/",
  "reference-files/2023/delineation-files/list1_2023.xls"
)

cbsa_delineation_path <- withr::local_tempfile(fileext = ".xls")
utils::download.file(cbsa_delineation_url, cbsa_delineation_path, mode = "wb")

# The published delineation file has a few banner/title rows before the
# real header; skip count should be verified against the current file.
cbsa_raw <- readxl::read_excel(cbsa_delineation_path, skip = 2)

# base R's state.name/state.abb omits DC and territories; extend explicitly.
state_name_to_abb <- c(
  setNames(state.abb, state.name),
  "District of Columbia" = "DC",
  "Puerto Rico" = "PR"
)

county_cbsa_crosswalk <- cbsa_raw |>
  transmute(
    county_fips = paste0(`FIPS State Code`, `FIPS County Code`),
    county_name = `County/County Equivalent`,
    state_abb = unname(state_name_to_abb[`State Name`]),
    cbsa_fips = `CBSA Code`,
    cbsa_title = `CBSA Title`,
    metro_micro = tolower(`Metropolitan/Micropolitan Statistical Area`)
  ) |>
  mutate(
    metro_micro = dplyr::case_when(
      grepl("^metro", metro_micro) ~ "metro",
      grepl("^micro", metro_micro) ~ "micro",
      TRUE ~ NA_character_
    )
  )

geography_crosswalks_provenance <- list(
  vintage = "2020 ZCTA relationship file; 2023 OMB CBSA delineation",
  retrieved_on = Sys.Date(),
  source_url = zcta_county_url
)
