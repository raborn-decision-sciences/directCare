# Unit tests for mod_scenario_slots_server()'s has_access()/on_access_denied()
# gating (the module's other behavior -- save/load/overwrite flows -- isn't
# covered here; this module has no prior test file at all, and adding full
# coverage of the pre-existing behavior is out of scope for what this pass
# touches). All DB calls are mocked -- these never touch a real Postgres
# instance, matching test-scenarios.R's own convention.

test_that("save_click and load_click proceed normally when has_access() is TRUE (the default)", {
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      data.frame(id = integer(0), label = character(0), updated_at = character(0))
    },
    dbDisconnect = function(conn) invisible(NULL),
    .package = "DBI"
  )
  denied_called <- FALSE

  shiny::testServer(
    mod_scenario_slots_server,
    args = list(
      id = "scenario",
      table = "plan_scenarios",
      get_con = function() structure(list(), class = "mock_con"),
      practice_id = function() 1L,
      get_inputs_for_save = function() list(a = 1),
      get_dirty_signal = function() list(a = 1),
      on_load = function(x) NULL,
      on_access_denied = function() denied_called <<- TRUE
    ),
    {
      session$setInputs(load_click = 1)
      expect_false(denied_called)

      session$setInputs(save_click = 1)
      expect_false(denied_called)
    }
  )
})

test_that("save_click and load_click call on_access_denied() and never touch the DB when has_access() is FALSE", {
  denied_count <- 0L

  shiny::testServer(
    mod_scenario_slots_server,
    args = list(
      id = "scenario",
      table = "plan_scenarios",
      get_con = function() stop("get_con() should not be called when access is denied"),
      practice_id = function() 1L,
      get_inputs_for_save = function() list(a = 1),
      get_dirty_signal = function() list(a = 1),
      on_load = function(x) NULL,
      has_access = function() FALSE,
      on_access_denied = function() denied_count <<- denied_count + 1L
    ),
    {
      session$setInputs(load_click = 1)
      expect_equal(denied_count, 1L)

      session$setInputs(save_click = 1)
      expect_equal(denied_count, 2L)
    }
  )
})
