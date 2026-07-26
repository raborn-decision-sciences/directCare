# Unit tests for R/utils_globals.R helpers

test_that(".slugify lower-cases and dash-separates a practice name", {
  expect_equal(.slugify("Riverside Direct Care"), "riverside-direct-care")
})

test_that(".slugify collapses punctuation and repeated separators", {
  expect_equal(
    .slugify("O'Brien's  Family--Health!!"),
    "o-brien-s-family-health"
  )
})

test_that(".slugify trims leading/trailing dashes", {
  expect_equal(.slugify("  --Riverside DPC--  "), "riverside-dpc")
})

test_that(".slugify returns an empty string for NULL or blank input", {
  expect_equal(.slugify(NULL), "")
  expect_equal(.slugify("   "), "")
})
