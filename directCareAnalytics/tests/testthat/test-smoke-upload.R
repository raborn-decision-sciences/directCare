# Smoke tests: CSV upload flow (monthly and weekly data)

test_that("monthly CSV upload produces validation badges", {
  skip_on_cran()
  app <- launch_app()
  on.exit(app$stop())

  fill_practice_details(app)
  choose_upload_path(app)
  upload_csv(app, demo_csv_monthly())

  # Transactions loaded badge should be present and non-zero
  html <- app$get_html("#upload-validation_badges")
  expect_true(grepl("Transactions loaded", html))
  expect_false(grepl(">0<", html)) # badge value should not be zero
})

test_that("monthly CSV upload populates the account mapping table", {
  skip_on_cran()
  app <- launch_app()
  on.exit(app$stop())

  fill_practice_details(app)
  choose_upload_path(app)
  upload_csv(app, demo_csv_monthly())

  # Mapping table should have rows (placeholder text should be gone)
  html <- app$get_html("#upload-mapping_table")
  expect_true(nchar(html) > 100)
})

test_that("weekly CSV upload produces validation badges", {
  skip_on_cran()
  app <- launch_app()
  on.exit(app$stop())

  fill_practice_details(app)
  choose_upload_path(app)
  upload_csv(app, demo_csv_weekly())

  html <- app$get_html("#upload-validation_badges")
  expect_true(grepl("Transactions loaded", html))
})

test_that("Summary tab shows overhead data after monthly upload", {
  skip_on_cran()
  app <- launch_app()
  on.exit(app$stop())

  fill_practice_details(app)
  choose_upload_path(app)
  upload_csv(app, demo_csv_monthly())

  app$click(selector = "a[data-value='summary']")
  app$wait_for_idle()

  html <- app$get_html("#summary-ovhd_vboxes")
  expect_true(grepl("Total overhead", html))
})

test_that("Summary tab shows 'No income data' placeholder when upload has no income", {
  skip_on_cran()
  app <- launch_app()
  on.exit(app$stop())

  fill_practice_details(app)
  choose_upload_path(app)
  upload_csv(app, demo_csv_expenses_only())

  app$click(selector = "a[data-value='summary']")
  app$wait_for_idle()

  html <- app$get_html("#summary-inc_vboxes")
  expect_true(grepl("No income data", html))
})

test_that("Projections tab renders break-even card after monthly upload", {
  skip_on_cran()
  app <- launch_app()
  on.exit(app$stop())

  fill_practice_details(app)
  choose_upload_path(app)
  upload_csv(app, demo_csv_monthly())

  # Navigate to projections first so the sidebar inputs are rendered
  app$click(selector = "a[data-value='projections']")
  app$wait_for_idle()

  app$set_inputs(
    `projections-panel_size` = 50,
    `projections-membership_fee` = 100
  )
  app$click(selector = "#projections-btn_run")
  app$wait_for_idle(timeout = 20000)

  html <- app$get_html("#projections-breakeven_ui")
  expect_true(nchar(html) > 50)
})
