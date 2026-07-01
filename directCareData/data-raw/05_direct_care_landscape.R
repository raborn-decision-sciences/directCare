# Placeholder for the direct care practice landscape table. No real source
# has been identified yet, so this constructs a zero-row tibble with the
# finalized schema, ready to be filled in once a source is available.
#
# When a real source is identified, replace the body of this script with
# an ingestion step that produces a tibble with exactly this schema
# (same column names/types) -- no changes are needed anywhere else in the
# data-raw pipeline or in directCarePlanR's market_context.R, since
# get_direct_care_landscape() only ever filters this table by county_fips.

direct_care_landscape <- tibble::tibble(
  county_fips = character(),
  practice_name = character(),
  practice_type = character(),
  estimated_panel_size = integer(),
  city = character(),
  state_abb = character(),
  source = character(),
  as_of_date = as.Date(character())
)

direct_care_landscape_provenance <- list(
  vintage = "placeholder (no source identified yet)",
  retrieved_on = Sys.Date(),
  source_url = NA_character_
)
