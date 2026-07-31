# Shared helpers for shinytest2 smoke tests.
# Loaded automatically by testthat before any test file.

library(shinytest2)

# Pumps the `later` event loop until an shiny::ExtendedTask (e.g.
# mod_projections.R's forecast_task) finishes running, for use inside
# testServer() after triggering an invoke(). testServer() doesn't run a live
# app event loop, so nothing else would ever service the background mirai
# worker's completion callback -- without this, task$result() stays stuck
# throwing its pre-completion shiny.silent.error forever.
wait_for_task <- function(task, timeout = 10) {
  t0 <- Sys.time()
  while (task$status() == "running") {
    later::run_now(timeoutSecs = 0.1)
    if (as.numeric(Sys.time() - t0, units = "secs") > timeout) {
      stop("Task did not complete within ", timeout, "s")
    }
  }
  # The task settling invalidates its dependent outputs, but testServer()
  # only actually re-renders them (updating output$x$html) on its own
  # explicit flush -- normally triggered by session$setInputs(). Since
  # nothing here calls setInputs() again, force that flush directly so
  # output$... reads immediately after wait_for_task() see the new render.
  shiny:::flushReact()
}

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
