# Unit tests for utils_billing.R's .tier_badge()

test_that(".tier_badge labels each real tier with its own color", {
  free <- as.character(.tier_badge("free"))
  starter <- as.character(.tier_badge("starter"))
  pro <- as.character(.tier_badge("pro"))

  expect_match(free, "text-bg-secondary")
  expect_match(free, "Free")

  expect_match(starter, "text-bg-info")
  expect_match(starter, "Starter")

  expect_match(pro, "text-bg-warning")
  expect_match(pro, "Pro")
})

test_that(".tier_badge falls back to the Free treatment for NULL/empty/NA input", {
  for (val in list(NULL, "", NA_character_)) {
    badge <- as.character(.tier_badge(val))
    expect_match(badge, "text-bg-secondary")
    expect_match(badge, "Free")
  }
})

test_that(".tier_badge treats an unrecognized non-empty value as Starter, matching .has_paid_plan()'s deliberately permissive 'any future paid tier' design", {
  badge <- as.character(.tier_badge("advisor"))
  expect_match(badge, "text-bg-info")
  expect_match(badge, "Starter")
})
