# Unit tests for R/scenarios.R. All DB calls are mocked -- these never
# touch a real Postgres instance. jsonlite/serialization round-trips are
# exercised for real (not mocked), since they're deterministic and pure.

test_that("scenario_list rejects an unknown table", {
  expect_error(scenario_list("mock_con", "not_a_table", 1L), "Unknown scenario table")
})

test_that("scenario_list selects the JSONB-table column set", {
  captured <- NULL
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      captured <<- list(statement = statement, params = params)
      data.frame(id = 1L, label = "Baseline", updated_at = "2026-01-01")
    },
    .package = "DBI"
  )

  result <- scenario_list("mock_con", "plan_scenarios", 7L)

  expect_equal(captured$params, list(7L))
  expect_match(captured$statement, "FROM plan_scenarios WHERE practice_id = \\$1")
  expect_false(grepl("source_filename", captured$statement))
  expect_equal(result$label, "Baseline")
})

test_that("scenario_list includes source_filename for dca_forecast_scenarios", {
  captured <- NULL
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      captured <<- list(statement = statement, params = params)
      data.frame(id = 1L, label = "Q1", source_filename = "gnucash.csv", updated_at = "2026-01-01")
    },
    .package = "DBI"
  )

  scenario_list("mock_con", "dca_forecast_scenarios", 7L)

  expect_match(captured$statement, "source_filename")
})

test_that("scenario_save_json rejects a non-JSONB table", {
  expect_error(
    scenario_save_json("mock_con", "dca_forecast_scenarios", 1L, "Baseline", list(a = 1)),
    "Unknown scenario table"
  )
})

test_that("scenario_save_json inserts when there's room and no label collision", {
  captured <- list()
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      if (grepl("^SELECT id FROM", statement)) {
        return(data.frame(id = integer(0)))
      }
      if (grepl("^SELECT count", statement)) {
        return(data.frame(n = 1L))
      }
      captured <<- list(statement = statement, params = params)
      data.frame(id = 42L)
    },
    .package = "DBI"
  )

  result <- scenario_save_json(
    "mock_con", "plan_scenarios", 7L, "Optimistic",
    list(panel_size = 300, fee = 100)
  )

  expect_true(result$ok)
  expect_equal(result$id, 42L)
  expect_match(captured$statement, "INSERT INTO plan_scenarios")
  expect_equal(captured$params[[1]], 7L)
  expect_equal(captured$params[[2]], "Optimistic")
  expect_equal(jsonlite::fromJSON(captured$params[[3]], simplifyVector = FALSE)$panel_size, 300)
})

test_that("scenario_save_json reports label_collision without checking the cap", {
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      if (grepl("^SELECT id FROM", statement)) {
        return(data.frame(id = 3L))
      }
      stop("cap check should not run when a label collision already exists")
    },
    .package = "DBI"
  )

  result <- scenario_save_json("mock_con", "plan_scenarios", 7L, "Baseline", list(a = 1))

  expect_false(result$ok)
  expect_equal(result$reason, "label_collision")
})

test_that("scenario_save_json reports cap_reached at 3 existing rows", {
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      if (grepl("^SELECT id FROM", statement)) {
        return(data.frame(id = integer(0)))
      }
      data.frame(n = 3L)
    },
    .package = "DBI"
  )

  result <- scenario_save_json("mock_con", "plan_scenarios", 7L, "Fourth Slot", list(a = 1))

  expect_false(result$ok)
  expect_equal(result$reason, "cap_reached")
})

test_that("scenario_save_json with overwrite_id issues an UPDATE by id, skipping the slot check", {
  captured <- NULL
  local_mocked_bindings(
    dbGetQuery = function(...) stop("dbGetQuery() should not be called on overwrite"),
    dbExecute = function(conn, statement, params) {
      captured <<- list(statement = statement, params = params)
      1L
    },
    .package = "DBI"
  )

  result <- scenario_save_json(
    "mock_con", "plan_scenarios", 7L, "Renamed Slot",
    list(panel_size = 250), overwrite_id = 9L
  )

  expect_true(result$ok)
  expect_equal(result$id, 9L)
  expect_match(captured$statement, "UPDATE plan_scenarios SET label")
  expect_equal(captured$params[[1]], "Renamed Slot")
  expect_equal(captured$params[[3]], 9L)
})

test_that("scenario_load_json decodes the stored JSON and returns NULL when not found", {
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      data.frame(
        label = "Baseline",
        inputs = jsonlite::toJSON(list(panel_size = 300, tiers = list("A", "B")), auto_unbox = TRUE),
        stringsAsFactors = FALSE
      )
    },
    .package = "DBI"
  )

  result <- scenario_load_json("mock_con", "plan_scenarios", 1L)

  expect_equal(result$label, "Baseline")
  expect_equal(result$inputs$panel_size, 300)
  expect_equal(result$inputs$tiers, list("A", "B"))

  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) data.frame(),
    .package = "DBI"
  )
  expect_null(scenario_load_json("mock_con", "plan_scenarios", 999L))
})

test_that("scenario_save_forecast serializes the bundle and inserts", {
  captured <- NULL
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      if (grepl("^SELECT id FROM", statement)) {
        return(data.frame(id = integer(0)))
      }
      if (grepl("^SELECT count", statement)) {
        return(data.frame(n = 0L))
      }
      captured <<- list(statement = statement, params = params)
      data.frame(id = 5L)
    },
    .package = "DBI"
  )

  bundle <- list(transactions = data.frame(amount = c(1, 2, 3)), inputs = list(method = "linear"))
  result <- scenario_save_forecast("mock_con", 7L, "Q1 Snapshot", bundle, source_filename = "gnucash.csv")

  expect_true(result$ok)
  expect_equal(result$id, 5L)
  expect_match(captured$statement, "INSERT INTO dca_forecast_scenarios")
  expect_equal(captured$params[[2]], "Q1 Snapshot")
  expect_equal(captured$params[[3]], "gnucash.csv")

  roundtripped <- unserialize(memDecompress(captured$params[[4]][[1]], type = "xz"))
  expect_equal(roundtripped$inputs$method, "linear")
  expect_equal(roundtripped$transactions$amount, c(1, 2, 3))
})

test_that("scenario_save_forecast with overwrite_id issues an UPDATE by id", {
  captured <- NULL
  local_mocked_bindings(
    dbGetQuery = function(...) stop("dbGetQuery() should not be called on overwrite"),
    dbExecute = function(conn, statement, params) {
      captured <<- list(statement = statement, params = params)
      1L
    },
    .package = "DBI"
  )

  result <- scenario_save_forecast(
    "mock_con", 7L, "Renamed", list(a = 1), overwrite_id = 11L
  )

  expect_true(result$ok)
  expect_equal(result$id, 11L)
  expect_match(captured$statement, "UPDATE dca_forecast_scenarios\\s+SET label")
  expect_equal(captured$params[[4]], 11L)
})

test_that("scenario_load_forecast round-trips a real serialized payload and returns NULL when not found", {
  bundle <- list(transactions = data.frame(amount = c(10, 20)), inputs = list(method = "ets"))
  payload <- memCompress(serialize(bundle, connection = NULL), type = "xz")

  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) {
      data.frame(
        label = "Q1 Snapshot",
        source_filename = "gnucash.csv",
        payload = I(list(payload)),
        stringsAsFactors = FALSE
      )
    },
    .package = "DBI"
  )

  result <- scenario_load_forecast("mock_con", 1L)

  expect_equal(result$label, "Q1 Snapshot")
  expect_equal(result$source_filename, "gnucash.csv")
  expect_equal(result$bundle$inputs$method, "ets")
  expect_equal(result$bundle$transactions$amount, c(10, 20))

  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params) data.frame(),
    .package = "DBI"
  )
  expect_null(scenario_load_forecast("mock_con", 999L))
})

test_that("scenario_delete rejects an unknown table", {
  expect_error(scenario_delete("mock_con", "not_a_table", 7L, 1L), "Unknown scenario table")
})

test_that("scenario_delete scopes the DELETE by both id and practice_id, returns TRUE when a row matched", {
  captured <- NULL
  local_mocked_bindings(
    dbExecute = function(conn, statement, params) {
      captured <<- list(statement = statement, params = params)
      1L
    },
    .package = "DBI"
  )

  result <- scenario_delete("mock_con", "plan_scenarios", 7L, 42L)

  expect_true(result)
  expect_match(captured$statement, "DELETE FROM plan_scenarios WHERE id = \\$1 AND practice_id = \\$2")
  expect_equal(captured$params, list(42L, 7L))
})

test_that("scenario_delete returns FALSE when no row matched", {
  local_mocked_bindings(
    dbExecute = function(conn, statement, params) 0L,
    .package = "DBI"
  )

  result <- scenario_delete("mock_con", "dca_forecast_scenarios", 7L, 999L)

  expect_false(result)
})

test_that("scenario_delete works for all three scenario tables", {
  local_mocked_bindings(
    dbExecute = function(conn, statement, params) 1L,
    .package = "DBI"
  )

  for (tbl in c("plan_scenarios", "dca_calculator_scenarios", "dca_forecast_scenarios")) {
    expect_true(scenario_delete("mock_con", tbl, 1L, 1L))
  }
})
