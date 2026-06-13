# Shared helpers for shinytest2 smoke tests.
# Loaded automatically by testthat before any test file.

library(shinytest2)

# Launch the app and return an AppDriver.
# Tests are responsible for calling app$stop() in their teardown.
launch_app <- function(width = 1400, height = 900, timeout = 15000) {
  AppDriver$new(
    app = directCareAnalytics::run_app(),
    name = "directCareAnalytics",
    width = width,
    height = height,
    timeout = timeout,
    options = list(shiny.testmode = TRUE)
  )
}

# Fill the practice details inputs (Step 1) and wait for reactives to settle.
fill_practice_details <- function(
  app,
  name = "Smoke Test Practice",
  id = "smoke-test"
) {
  app$set_inputs(
    `upload-practice_name` = name,
    `upload-practice_id` = id
  )
  app$wait_for_idle()
}

# Choose the "Upload Bookkeeping Data" path and wait.
choose_upload_path <- function(app) {
  app$click(selector = "#upload-btn_use_real")
  app$wait_for_idle()
}

# Choose the "Plan My Practice" path and wait.
choose_plan_path <- function(app) {
  app$click(selector = "#upload-btn_use_plan")
  app$wait_for_idle()
}

# Upload a CSV and process it; returns after the notification has appeared.
upload_csv <- function(app, csv_path) {
  app$upload_file(`upload-csv_file` = csv_path)
  app$click(selector = "#upload-btn_upload")
  app$wait_for_idle(timeout = 20000)
}

# Path to the bundled demo fixtures.
demo_csv_monthly <- function() {
  system.file("extdata", "demo-gnucash.csv", package = "directCareAnalytics")
}
demo_csv_weekly <- function() {
  system.file(
    "extdata",
    "demo-gnucash-weekly.csv",
    package = "directCareAnalytics"
  )
}
demo_csv_expenses_only <- function() {
  system.file(
    "extdata",
    "demo-gnucash-expenses-only.csv",
    package = "directCareAnalytics"
  )
}
