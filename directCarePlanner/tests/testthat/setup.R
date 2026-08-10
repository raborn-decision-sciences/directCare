# Shared test helpers.
# Loaded automatically by testthat before any test file.

# Pumps the `later` event loop until a shiny::ExtendedTask (e.g.
# mod_results.R's report_task) finishes running, for use inside
# testServer() after triggering an invoke(). testServer() doesn't run a live
# app event loop, so nothing else would ever service the background mirai
# worker's completion callback -- without this, task$result() stays stuck
# throwing its pre-completion shiny.silent.error forever. Matches
# directCareAnalytics's tests/testthat/setup.R identical helper.
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
