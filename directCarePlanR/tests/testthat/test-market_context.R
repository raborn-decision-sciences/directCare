# Synthetic fixtures, injected via each function's `data =`/`crosswalk =`/
# `geo =` argument, so these tests are deterministic and independent of
# directCareData's real (Census-derived) contents.

fixture_market_data <- tibble::tibble(
  county_fips = c("11111", "22222", "33333", "44444"),
  county_name = c("Alpha County", "Beta County", "Washington County", "Washington County"),
  state_abb = c("AA", "BB", "MT", "OR"),
  state_fips = c("11", "22", "33", "44"),
  cbsa_fips = c("99999", NA_character_, NA_character_, NA_character_),
  cbsa_title = c("Alpha Metro Area", NA_character_, NA_character_, NA_character_),
  population = c(100000L, 5000L, 20000L, 30000L),
  median_household_income = c(60000, 40000, 50000, 55000),
  uninsured_count = c(5000L, 500L, 1000L, 1500L),
  uninsured_rate = c(0.05, 0.1, 0.05, 0.05),
  physician_count = c(50L, 0L, 10L, 15L),
  physician_density_per_10k = c(5, 0, 5, 5)
)

fixture_crosswalk <- tibble::tibble(
  zip = c("10001", "20002", "20002"),
  county_fips = c("11111", "11111", "22222"),
  overlap_pct = c(1, 0.6, 0.4),
  is_primary = c(TRUE, TRUE, FALSE)
)

fixture_geo <- fixture_market_data[
  c("county_fips", "county_name", "state_abb", "cbsa_fips", "cbsa_title")
]

fixture_landscape <- tibble::tibble(
  county_fips = "11111",
  practice_name = "Example DPC",
  practice_type = "membership",
  estimated_panel_size = 400L,
  city = "Example City",
  state_abb = "AA",
  source = "manual research",
  as_of_date = as.Date("2026-01-01")
)

# -- resolve_geography --------------------------------------------------------

test_that("resolve_geography() resolves a single-county ZIP", {
  geo <- resolve_geography("10001", crosswalk = fixture_crosswalk, geo = fixture_geo)
  expect_s3_class(geo, "dcPlanR_geography")
  expect_equal(geo$county_fips, "11111")
  expect_equal(geo$metro_fips, "99999")
  expect_equal(geo$state_abb, "AA")
})

test_that("resolve_geography() picks the primary county for a multi-county ZIP", {
  expect_message(
    geo <- resolve_geography("20002", crosswalk = fixture_crosswalk, geo = fixture_geo),
    class = "dcPlanR_zip_multi_county"
  )
  expect_equal(geo$county_fips, "11111")
})

test_that("resolve_geography() resolves a county name with or without 'County'", {
  geo1 <- resolve_geography("Alpha", crosswalk = fixture_crosswalk, geo = fixture_geo)
  geo2 <- resolve_geography("Alpha County", crosswalk = fixture_crosswalk, geo = fixture_geo)
  expect_equal(geo1$county_fips, "11111")
  expect_equal(geo2$county_fips, "11111")
})

test_that("resolve_geography() disambiguates a county name with state", {
  geo <- resolve_geography(
    "Washington",
    state = "MT",
    crosswalk = fixture_crosswalk,
    geo = fixture_geo
  )
  expect_equal(geo$county_fips, "33333")
})

test_that("resolve_geography() errors on an ambiguous county name without state", {
  expect_error(
    resolve_geography("Washington", crosswalk = fixture_crosswalk, geo = fixture_geo),
    class = "dcPlanR_county_ambiguous"
  )
})

test_that("resolve_geography() errors on an unknown ZIP", {
  expect_error(
    resolve_geography("00000", crosswalk = fixture_crosswalk, geo = fixture_geo),
    class = "dcPlanR_zip_not_found"
  )
})

test_that("resolve_geography() errors on an unknown county name", {
  expect_error(
    resolve_geography("Nonexistent", crosswalk = fixture_crosswalk, geo = fixture_geo),
    class = "dcPlanR_county_not_found"
  )
})

test_that("resolve_geography() errors on invalid input", {
  expect_error(resolve_geography(NA_character_), class = "dcPlanR_invalid_fips")
  expect_error(resolve_geography(c("a", "b")), class = "dcPlanR_invalid_fips")
})

# -- get_population_income / get_uninsured_estimate / get_physician_density --

test_that("get_population_income() returns the matching row's fields", {
  result <- get_population_income("11111", data = fixture_market_data)
  expect_s3_class(result, "dcPlanR_population_income")
  expect_equal(result$population, 100000L)
  expect_equal(result$median_household_income, 60000)
})

test_that("get_uninsured_estimate() returns the matching row's fields", {
  result <- get_uninsured_estimate("22222", data = fixture_market_data)
  expect_s3_class(result, "dcPlanR_uninsured_estimate")
  expect_equal(result$uninsured_count, 500L)
  expect_equal(result$uninsured_rate, 0.1)
})

test_that("get_physician_density() returns the matching row's fields", {
  result <- get_physician_density("33333", data = fixture_market_data)
  expect_s3_class(result, "dcPlanR_physician_density")
  expect_equal(result$physician_count, 10L)
  expect_equal(result$physician_density_per_10k, 5)
})

test_that("county-data accessors error on a well-formed but unknown FIPS", {
  expect_error(
    get_population_income("99999", data = fixture_market_data),
    class = "dcPlanR_fips_not_found"
  )
  expect_error(
    get_uninsured_estimate("99999", data = fixture_market_data),
    class = "dcPlanR_fips_not_found"
  )
  expect_error(
    get_physician_density("99999", data = fixture_market_data),
    class = "dcPlanR_fips_not_found"
  )
})

test_that("county-data accessors error on a malformed FIPS", {
  expect_error(get_population_income("abc"), class = "dcPlanR_invalid_fips")
  expect_error(get_population_income(123), class = "dcPlanR_invalid_fips")
  expect_error(get_population_income(NA_character_), class = "dcPlanR_invalid_fips")
})

# -- get_direct_care_landscape -------------------------------------------------

test_that("get_direct_care_landscape() returns known practices for a county", {
  result <- get_direct_care_landscape("11111", data = fixture_landscape)
  expect_equal(nrow(result), 1L)
  expect_equal(result$practice_name, "Example DPC")
})

test_that("get_direct_care_landscape() returns zero rows, not an error, for a county with none", {
  result <- get_direct_care_landscape("22222", data = fixture_landscape)
  expect_equal(nrow(result), 0L)
})

test_that("get_direct_care_landscape() errors on a malformed FIPS", {
  expect_error(get_direct_care_landscape("abc"), class = "dcPlanR_invalid_fips")
})

# -- build_market_context (real directCareData defaults) ---------------------

test_that("build_market_context() assembles all expected elements", {
  skip_if_not(
    nrow(directCareData::county_market_data) > 0L,
    "directCareData market data is still the empty placeholder"
  )

  # Fulton County, GA (FIPS 13121) is a stable reference county.
  ctx <- build_market_context("30309")
  expect_s3_class(ctx, "dcPlanR_market_context")
  expect_named(
    ctx,
    c("geography", "population_income", "uninsured", "physician_density", "landscape", "provenance")
  )
  expect_equal(ctx$geography$county_fips, "13121")
})

test_that("resolve_geography() agrees between ZIP and county-name lookup", {
  skip_if_not(
    nrow(directCareData::county_cbsa_crosswalk) > 0L,
    "directCareData geography crosswalks are still the empty placeholder"
  )

  by_zip <- resolve_geography("30309")
  by_name <- resolve_geography("Fulton", state = "GA")
  expect_equal(by_zip$county_fips, by_name$county_fips)
  expect_equal(by_zip$county_fips, "13121")
})

# -- resolve_geography()'s cached index (default crosswalk/geo only) --------

test_that("resolve_geography() builds and reuses one cached index across calls with default args", {
  skip_if_not(
    nrow(directCareData::county_cbsa_crosswalk) > 0L,
    "directCareData geography crosswalks are still the empty placeholder"
  )
  rm(list = ls(.geo_index_env), envir = .geo_index_env)

  expect_null(.geo_index_env$zip_index)
  resolve_geography("30309")
  first_index <- .geo_index_env$zip_index
  expect_false(is.null(first_index))

  resolve_geography("30309")
  expect_identical(.geo_index_env$zip_index, first_index)
})

test_that("resolve_geography() with a caller-supplied crosswalk/geo never touches the cache", {
  rm(list = ls(.geo_index_env), envir = .geo_index_env)

  resolve_geography("10001", crosswalk = fixture_crosswalk, geo = fixture_geo)

  expect_null(.geo_index_env$zip_index)
  expect_null(.geo_index_env$county_name_index)
  expect_null(.geo_index_env$county_fips_index)
})

test_that("resolve_geography() errors correctly via the cached (default-args) path", {
  skip_if_not(
    nrow(directCareData::county_cbsa_crosswalk) > 0L,
    "directCareData geography crosswalks are still the empty placeholder"
  )
  rm(list = ls(.geo_index_env), envir = .geo_index_env)

  expect_error(resolve_geography("00000"), class = "dcPlanR_zip_not_found")
  expect_error(resolve_geography("Nonexistent County"), class = "dcPlanR_county_not_found")
})
