#' Valid scenario table names
#'
#' `plan_scenarios`/`dca_calculator_scenarios` hold pure scalar/small-list
#' inputs as JSONB; `dca_forecast_scenarios` holds a full input+result
#' snapshot (including row-level transaction data) as a `BYTEA` R-serialized
#' blob -- see [scenario_save_forecast()]. All three share the same 3-slot
#' cap/label-collision save semantics, hence the shared helpers below.
#' @noRd
.SCENARIO_JSON_TABLES <- c("plan_scenarios", "dca_calculator_scenarios")
#' @noRd
.SCENARIO_TABLES <- c(.SCENARIO_JSON_TABLES, "dca_forecast_scenarios")
#' @noRd
.SCENARIO_MAX_SLOTS <- 3L

#' @noRd
.check_scenario_table <- function(table, allowed = .SCENARIO_TABLES) {
  if (!isTRUE(table %in% allowed)) {
    stop("Unknown scenario table: ", table, call. = FALSE)
  }
}

#' Check whether a practice has room to save a new scenario under a label
#'
#' A plain `INSERT` only works cleanly when both hold: fewer than
#' `.SCENARIO_MAX_SLOTS` existing rows for the practice, and no existing row
#' already has this exact label (the table's `UNIQUE (practice_id, label)`
#' constraint would reject the insert otherwise). Checked as two explicit
#' `SELECT`s (not relying on the constraint violation itself) so failures
#' are distinguishable and testable without a real Postgres instance. The
#' cap check has a benign, accepted race (two concurrent saves for the same
#' practice could both pass it, landing 4 rows briefly) -- not worth an
#' advisory lock for a single-user-per-practice app.
#' @noRd
.scenario_slot_check <- function(con, table, practice_id, label) {
  existing <- DBI::dbGetQuery(
    con,
    sprintf("SELECT id FROM %s WHERE practice_id = $1 AND label = $2", table),
    params = list(practice_id, label)
  )
  if (nrow(existing) > 0) {
    return(list(can_insert = FALSE, reason = "label_collision"))
  }

  count <- DBI::dbGetQuery(
    con,
    sprintf("SELECT count(*) AS n FROM %s WHERE practice_id = $1", table),
    params = list(practice_id)
  )
  if (isTRUE(count$n[[1]] >= .SCENARIO_MAX_SLOTS)) {
    return(list(can_insert = FALSE, reason = "cap_reached"))
  }

  list(can_insert = TRUE, reason = NULL)
}

#' List a practice's saved scenario slots for one feature area
#'
#' Selects only `id`/`label`/`updated_at` (plus `source_filename` for
#' `dca_forecast_scenarios`) -- never the `payload`/`inputs` column itself,
#' so rendering a picker stays cheap regardless of how large a saved
#' scenario is.
#'
#' @param con An open `DBI` connection (see `directCareAuth::db_connect()`).
#' @param table One of `"plan_scenarios"`, `"dca_calculator_scenarios"`,
#'   `"dca_forecast_scenarios"`.
#' @param practice_id The practice's `practices.id`.
#' @return A data frame with columns `id`, `label`, `updated_at` (plus
#'   `source_filename` for `dca_forecast_scenarios`), 0-3 rows, newest
#'   `updated_at` first.
#' @export
scenario_list <- function(con, table, practice_id) {
  .check_scenario_table(table)
  cols <- if (table == "dca_forecast_scenarios") {
    "id, label, source_filename, updated_at"
  } else {
    "id, label, updated_at"
  }
  DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT %s FROM %s WHERE practice_id = $1 ORDER BY updated_at DESC",
      cols, table
    ),
    params = list(practice_id)
  )
}

#' Save (insert or overwrite) a scenario in a JSONB-backed table
#'
#' For Launch Planner and DCA Quick Calculator scenarios -- pure
#' scalar/small-list inputs, encoded to JSONB via `jsonlite`. See
#' [scenario_save_forecast()] for DCA's full forecast pipeline, which needs
#' row-level transaction data and is stored as a `BYTEA` blob instead.
#'
#' @param con An open `DBI` connection.
#' @param table `"plan_scenarios"` or `"dca_calculator_scenarios"`.
#' @param practice_id The practice's `practices.id`.
#' @param label User-chosen slot label.
#' @param inputs A plain R list of scalar/small-list inputs.
#' @param overwrite_id If supplied, updates that row by id instead of
#'   attempting an insert (label/inputs/`updated_at` all replaced) --
#'   required once a save would otherwise exceed the 3-slot cap or collide
#'   with an existing label.
#' @return `list(ok = TRUE, id = <row id>)`, or
#'   `list(ok = FALSE, reason = "cap_reached" | "label_collision")` when
#'   `overwrite_id` is `NULL` and a plain insert isn't possible -- the
#'   caller should show a slot picker and retry with `overwrite_id` set.
#' @export
scenario_save_json <- function(con, table, practice_id, label, inputs, overwrite_id = NULL) {
  .check_scenario_table(table, .SCENARIO_JSON_TABLES)
  payload <- as.character(jsonlite::toJSON(inputs, auto_unbox = TRUE, null = "null"))

  if (!is.null(overwrite_id)) {
    DBI::dbExecute(
      con,
      sprintf(
        "UPDATE %s SET label = $1, inputs = $2, updated_at = now() WHERE id = $3",
        table
      ),
      params = list(label, payload, overwrite_id)
    )
    return(list(ok = TRUE, id = overwrite_id))
  }

  check <- .scenario_slot_check(con, table, practice_id, label)
  if (!check$can_insert) {
    return(list(ok = FALSE, reason = check$reason))
  }

  inserted <- DBI::dbGetQuery(
    con,
    sprintf(
      "INSERT INTO %s (practice_id, label, inputs) VALUES ($1, $2, $3) RETURNING id",
      table
    ),
    params = list(practice_id, label, payload)
  )
  list(ok = TRUE, id = inserted$id[[1]])
}

#' Load one saved JSONB scenario's inputs
#'
#' @param con An open `DBI` connection.
#' @param table `"plan_scenarios"` or `"dca_calculator_scenarios"`.
#' @param id The scenario's row id.
#' @return `list(label = , inputs = )`, or `NULL` if no row matches `id`.
#'   `inputs` is decoded with `simplifyVector = FALSE` (mirroring the
#'   `auto_unbox = TRUE` used to encode it) so scalars round-trip as plain
#'   R scalars rather than being auto-simplified into vectors/data frames.
#' @export
scenario_load_json <- function(con, table, id) {
  .check_scenario_table(table, .SCENARIO_JSON_TABLES)
  result <- DBI::dbGetQuery(
    con,
    sprintf("SELECT label, inputs FROM %s WHERE id = $1", table),
    params = list(id)
  )
  if (nrow(result) != 1) {
    return(NULL)
  }
  list(
    label = result$label[[1]],
    inputs = jsonlite::fromJSON(result$inputs[[1]], simplifyVector = FALSE)
  )
}

#' Save (insert or overwrite) a DCA full-forecast scenario snapshot
#'
#' Bundles both inputs and the computed forecast results -- never just
#' inputs to recompute-on-load -- into one `saveRDS(..., compress = "xz")`
#' payload. See `SAVED_SCENARIOS.md`'s "Why BYTEA" section: this table
#' carries row-level transaction data (an 11-column tibble with `Date`
#' columns), which JSONB would need custom type handling for with no real
#' benefit, since nothing needs to query inside this payload from SQL.
#' Snapshotting results (not just inputs) guarantees a saved comparison
#' never silently drifts if `directCareForecastR`'s forecasting logic
#' changes later.
#'
#' @param con An open `DBI` connection.
#' @param practice_id The practice's `practices.id`.
#' @param label User-chosen slot label.
#' @param bundle A plain R list (`transactions`/`overhead_monthly`/
#'   `income_monthly`/`inputs`/`results` -- see `SAVED_SCENARIOS.md` for the
#'   exact shape). Must contain only plain numbers/dates/tibbles, no plots
#'   or other non-serializable objects.
#' @param source_filename The originating upload's filename, or `NULL` for
#'   a manual-entry-only session.
#' @param overwrite_id Same semantics as [scenario_save_json()].
#' @return Same return shape as [scenario_save_json()].
#' @export
scenario_save_forecast <- function(con, practice_id, label, bundle,
                                    source_filename = NULL, overwrite_id = NULL) {
  # saveRDS()'s own `compress =` argument is silently ignored when writing
  # to a connection rather than a named file (documented R behavior, not a
  # bug here) -- serialize() + memCompress() is the correct way to get
  # xz-compressed bytes in memory without going through a real file.
  payload <- memCompress(serialize(bundle, connection = NULL), type = "xz")
  # DBI parameter binding needs SQL NULL spelled as NA (here NA_character_),
  # not a literal R NULL -- passing NULL directly fails at bind time with
  # "Parameter N does not have length 1" (confirmed empirically; a real R
  # NULL is zero-length, not a length-1 "absent value" the way NA is).
  source_filename <- if (is.null(source_filename)) NA_character_ else source_filename

  if (!is.null(overwrite_id)) {
    DBI::dbExecute(
      con,
      "UPDATE dca_forecast_scenarios
         SET label = $1, source_filename = $2, payload = $3, updated_at = now()
         WHERE id = $4",
      params = list(label, source_filename, list(payload), overwrite_id)
    )
    return(list(ok = TRUE, id = overwrite_id))
  }

  check <- .scenario_slot_check(con, "dca_forecast_scenarios", practice_id, label)
  if (!check$can_insert) {
    return(list(ok = FALSE, reason = check$reason))
  }

  inserted <- DBI::dbGetQuery(
    con,
    "INSERT INTO dca_forecast_scenarios (practice_id, label, source_filename, payload)
       VALUES ($1, $2, $3, $4)
       RETURNING id",
    params = list(practice_id, label, source_filename, list(payload))
  )
  list(ok = TRUE, id = inserted$id[[1]])
}

#' Load one saved DCA full-forecast scenario snapshot
#'
#' @param con An open `DBI` connection.
#' @param id The scenario's row id.
#' @return `list(label = , source_filename = , bundle = )`, or `NULL` if no
#'   row matches `id`. `bundle` is the exact list originally passed to
#'   [scenario_save_forecast()], reconstructed via `unserialize()`.
#' @export
scenario_load_forecast <- function(con, id) {
  result <- DBI::dbGetQuery(
    con,
    "SELECT label, source_filename, payload FROM dca_forecast_scenarios WHERE id = $1",
    params = list(id)
  )
  if (nrow(result) != 1) {
    return(NULL)
  }

  list(
    label = result$label[[1]],
    source_filename = result$source_filename[[1]],
    bundle = unserialize(memDecompress(result$payload[[1]], type = "xz"))
  )
}
