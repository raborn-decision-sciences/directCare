# These tests validate schema and sanity ranges, never pinned real-world
# values, since the underlying Census/SAHIE/NPPES data refreshes
# independently of this package's logic. Tests that require real (non-
# placeholder) data skip gracefully until the data-raw pipeline has been
# run (Phase B, pending CENSUS_API_KEY); schema-only checks that already
# hold for the empty placeholder run unconditionally.

skip_if_no_market_data <- function() {
  skip_if_not(
    nrow(county_market_data) > 0L,
    "county_market_data is still the empty placeholder; run data-raw/00_market_data.R"
  )
}

test_that("county_market_data has the expected schema", {
  skip_if_no_market_data()

  expect_s3_class(county_market_data, "data.frame")
  expect_true(all(
    c(
      "county_fips", "county_name", "state_abb", "state_fips",
      "cbsa_fips", "cbsa_title", "population", "median_household_income",
      "uninsured_count", "uninsured_rate", "physician_count",
      "physician_density_per_10k"
    ) %in%
      names(county_market_data)
  ))
  expect_true(nrow(county_market_data) > 3000L)
  expect_false(anyDuplicated(county_market_data$county_fips) > 0)
})

test_that("county_market_data values are within sane ranges", {
  skip_if_no_market_data()

  expect_true(all(county_market_data$population >= 0, na.rm = TRUE))
  expect_true(all(county_market_data$uninsured_rate >= 0 & county_market_data$uninsured_rate <= 1, na.rm = TRUE))
  expect_true(all(county_market_data$physician_count >= 0, na.rm = TRUE))
  expect_true(all(county_market_data$physician_density_per_10k >= 0, na.rm = TRUE))
})

test_that("zip_county_crosswalk has exactly one primary county per zip", {
  skip_if_not(
    nrow(zip_county_crosswalk) > 0L,
    "zip_county_crosswalk is still the empty placeholder; run data-raw/00_market_data.R"
  )

  primary_counts <- tapply(
    zip_county_crosswalk$is_primary,
    zip_county_crosswalk$zip,
    sum
  )
  expect_true(all(primary_counts == 1))
})

test_that("direct_care_landscape has the placeholder schema", {
  expect_s3_class(direct_care_landscape, "data.frame")
  expect_equal(
    names(direct_care_landscape),
    c(
      "county_fips", "practice_name", "practice_type",
      "estimated_panel_size", "city", "state_abb", "source", "as_of_date"
    )
  )
  expect_equal(nrow(direct_care_landscape), 0L)
})

test_that("data_provenance has the expected structure", {
  expect_named(
    data_provenance,
    c("acs", "sahie", "nppes", "geography_crosswalks", "direct_care_landscape")
  )
  for (source in data_provenance) {
    expect_named(source, c("vintage", "retrieved_on", "source_url"))
  }
})
