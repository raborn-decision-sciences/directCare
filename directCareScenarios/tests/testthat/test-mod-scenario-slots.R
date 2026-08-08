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

test_that("delete_confirm deletes the scenario, clears a matching banner, and refreshes the slot list", {
  query_calls <- 0L
  execute_calls <- 0L
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      query_calls <<- query_calls + 1L
      if (query_calls == 1L) {
        data.frame(
          id = c(1L, 2L), label = c("A", "B"),
          updated_at = c("2026-01-01", "2026-01-02"), stringsAsFactors = FALSE
        )
      } else {
        data.frame(id = 2L, label = "B", updated_at = "2026-01-02", stringsAsFactors = FALSE)
      }
    },
    dbExecute = function(conn, statement, params) {
      execute_calls <<- execute_calls + 1L
      expect_match(statement, "DELETE FROM plan_scenarios WHERE id = \\$1 AND practice_id = \\$2")
      expect_equal(params, list(1L, 1L))
      1L
    },
    dbDisconnect = function(conn) invisible(NULL),
    .package = "DBI"
  )

  shiny::testServer(
    mod_scenario_slots_server,
    args = list(
      id = "scenario",
      table = "plan_scenarios",
      get_con = function() structure(list(), class = "mock_con"),
      practice_id = function() 1L,
      get_inputs_for_save = function() list(a = 1),
      get_dirty_signal = function() list(a = 1),
      on_load = function(x) NULL
    ),
    {
      session$setInputs(load_click = 1)
      loaded_label("A") # simulate "A" currently being the viewed scenario
      session$setInputs(load_choice = "1")
      session$setInputs(delete_click = 1)
      session$setInputs(delete_confirm = 1)

      expect_equal(execute_calls, 1L)
      expect_equal(query_calls, 2L) # initial load_click + post-delete refresh
      expect_null(loaded_label()) # banner cleared -- the deleted slot was the one being viewed
      expect_equal(load_slots()$id, 2L) # refreshed to the one remaining slot
    }
  )
})

test_that("delete_confirm leaves the banner alone when the deleted slot wasn't the one being viewed", {
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      data.frame(
        id = c(1L, 2L), label = c("A", "B"),
        updated_at = c("2026-01-01", "2026-01-02"), stringsAsFactors = FALSE
      )
    },
    dbExecute = function(conn, statement, params) 1L,
    dbDisconnect = function(conn) invisible(NULL),
    .package = "DBI"
  )

  shiny::testServer(
    mod_scenario_slots_server,
    args = list(
      id = "scenario",
      table = "plan_scenarios",
      get_con = function() structure(list(), class = "mock_con"),
      practice_id = function() 1L,
      get_inputs_for_save = function() list(a = 1),
      get_dirty_signal = function() list(a = 1),
      on_load = function(x) NULL
    ),
    {
      session$setInputs(load_click = 1)
      loaded_label("B") # viewing "B", but about to delete "A"
      session$setInputs(load_choice = "1")
      session$setInputs(delete_click = 1)
      session$setInputs(delete_confirm = 1)

      expect_equal(loaded_label(), "B")
    }
  )
})

test_that("delete_confirm shows an error and doesn't crash when the row is already gone", {
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      data.frame(id = 1L, label = "A", updated_at = "2026-01-01", stringsAsFactors = FALSE)
    },
    dbExecute = function(conn, statement, params) 0L, # no row matched
    dbDisconnect = function(conn) invisible(NULL),
    .package = "DBI"
  )

  shiny::testServer(
    mod_scenario_slots_server,
    args = list(
      id = "scenario",
      table = "plan_scenarios",
      get_con = function() structure(list(), class = "mock_con"),
      practice_id = function() 1L,
      get_inputs_for_save = function() list(a = 1),
      get_dirty_signal = function() list(a = 1),
      on_load = function(x) NULL
    ),
    {
      session$setInputs(load_click = 1)
      session$setInputs(load_choice = "1")
      expect_no_error({
        session$setInputs(delete_click = 1)
        session$setInputs(delete_confirm = 1)
      })
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
