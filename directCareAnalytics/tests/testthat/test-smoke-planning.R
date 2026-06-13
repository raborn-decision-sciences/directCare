# Smoke tests: Quick Estimator / practice planning flow

test_that("Quick Estimator form renders on the Edit tab after choosing Plan path", {
  skip_on_cran()
  app <- launch_app()
  on.exit(app$stop())

  fill_practice_details(app)
  choose_plan_path(app)

  html <- app$get_html("#edit-content")
  expect_true(grepl("est_rent", html))
  expect_true(grepl("btn_generate", html))
})

test_that("Generate button produces a Scenario Ready card", {
  skip_on_cran()
  app <- launch_app()
  on.exit(app$stop())

  fill_practice_details(app, name = "River DPC", id = "river-dpc")
  choose_plan_path(app)

  app$set_inputs(
    `edit-est_rent` = 1200,
    `edit-est_other_overhead` = 150,
    `edit-est_ehr` = 200,
    `edit-est_panel_size` = 75,
    `edit-est_monthly_fee` = 99,
    `edit-est_n_months` = 12
  )
  app$click(selector = "#edit-btn_generate")
  app$wait_for_idle(timeout = 15000)

  html <- app$get_html("#edit-content")
  expect_true(grepl("Scenario Ready", html, ignore.case = TRUE))
})

test_that("'Go to Projections' navigates to the Projections tab", {
  skip_on_cran()
  app <- launch_app()
  on.exit(app$stop())

  fill_practice_details(app)
  choose_plan_path(app)

  app$set_inputs(
    `edit-est_rent` = 1000,
    `edit-est_panel_size` = 60,
    `edit-est_monthly_fee` = 89,
    `edit-est_n_months` = 12
  )
  app$click(selector = "#edit-btn_generate")
  app$wait_for_idle(timeout = 15000)

  app$click(selector = "#edit-btn_go_projections")
  app$wait_for_idle()

  expect_equal(app$get_value(input = "main_nav"), "projections")
})

test_that("Revise Estimates repopulates form with previous values", {
  skip_on_cran()
  app <- launch_app()
  on.exit(app$stop())

  fill_practice_details(app)
  choose_plan_path(app)

  app$set_inputs(
    `edit-est_rent` = 1500,
    `edit-est_panel_size` = 80,
    `edit-est_monthly_fee` = 110,
    `edit-est_n_months` = 12
  )
  app$click(selector = "#edit-btn_generate")
  app$wait_for_idle(timeout = 15000)

  app$click(selector = "#edit-btn_restart_estimator")
  app$wait_for_idle()

  expect_equal(app$get_value(input = "edit-est_rent"), 1500)
  expect_equal(app$get_value(input = "edit-est_panel_size"), 80)
  expect_equal(app$get_value(input = "edit-est_monthly_fee"), 110)
})

test_that("Projections tab renders after Quick Estimator scenario", {
  skip_on_cran()
  app <- launch_app()
  on.exit(app$stop())

  fill_practice_details(app)
  choose_plan_path(app)

  app$set_inputs(
    `edit-est_rent` = 1200,
    `edit-est_panel_size` = 70,
    `edit-est_monthly_fee` = 95,
    `edit-est_n_months` = 12
  )
  app$click(selector = "#edit-btn_generate")
  app$wait_for_idle(timeout = 15000)

  # Navigate via navbar directly
  app$click(selector = "a[data-value='projections']")
  app$wait_for_idle()

  app$click(selector = "#projections-btn_run")
  app$wait_for_idle(timeout = 20000)

  html <- app$get_html("#projections-breakeven_ui")
  expect_true(nchar(html) > 50)
})
