# Smoke tests: onboarding flow (Step 1 + Step 2 path selection)

test_that("app loads with Upload tab active", {
  skip_on_cran()
  app <- launch_app()
  on.exit(app$stop())

  expect_equal(app$get_value(input = "main_nav"), "upload")
})

test_that("path choice is hidden until practice details are filled", {
  skip_on_cran()
  app <- launch_app()
  on.exit(app$stop())

  # With empty fields, main_content should show the prompt, not choice cards
  html <- app$get_html("#upload-main_content")
  expect_false(grepl("btn_use_real", html))
})

test_that("path choice cards appear after filling practice details", {
  skip_on_cran()
  app <- launch_app()
  on.exit(app$stop())

  fill_practice_details(app)

  html <- app$get_html("#upload-main_content")
  expect_true(grepl("btn_use_real", html))
  expect_true(grepl("btn_use_plan", html))
})

test_that("'Plan My Practice' navigates to the Edit tab", {
  skip_on_cran()
  app <- launch_app()
  on.exit(app$stop())

  fill_practice_details(app)
  choose_plan_path(app)

  expect_equal(app$get_value(input = "main_nav"), "edit")
})

test_that("'Upload Bookkeeping Data' reveals the upload controls", {
  skip_on_cran()
  app <- launch_app()
  on.exit(app$stop())

  fill_practice_details(app)
  choose_upload_path(app)

  html <- app$get_html("#upload-main_content")
  expect_true(grepl("btn_upload", html))
  expect_true(grepl("btn_back", html))
})

test_that("Back button returns to path selection", {
  skip_on_cran()
  app <- launch_app()
  on.exit(app$stop())

  fill_practice_details(app)
  choose_upload_path(app)
  app$click(selector = "#upload-btn_back")
  app$wait_for_idle()

  html <- app$get_html("#upload-main_content")
  expect_true(grepl("btn_use_real", html))
})

test_that("'Enter Data Manually' navigates to the Edit tab", {
  skip_on_cran()
  app <- launch_app()
  on.exit(app$stop())

  fill_practice_details(app)
  choose_upload_path(app)
  app$click(selector = "#upload-btn_manual_entry")
  app$wait_for_idle()

  expect_equal(app$get_value(input = "main_nav"), "edit")
})
