# -- Shared Save/Load scenario-slot Shiny module -----------------------------
#
# One implementation shared by all three call sites (Launch Planner results,
# DCA Quick Calculator, DCA Projections) rather than three near-duplicate
# ~150-line blocks -- the save/overwrite-picker/load modal flow and the
# "viewing a saved scenario" banner are identical everywhere; only *what* to
# save and *how* to apply a load differ, and those are pushed out to the
# host app via plain callback functions.
#
# Host apps stay fully in control of anything scenario-specific:
#   - get_inputs_for_save() / get_bundle_for_save(): what to persist.
#   - on_load(value): apply a loaded scenario to the host's own reactive
#     state (typically a batch of updateXInput() calls). For hosts with
#     dynamic/growable sub-inputs (DCA Calculator's overhead sources and
#     tiers, Projections' membership tiers and planned events), on_load()
#     is expected to do its own two-phase dance internally -- set the
#     relevant count reactiveVal(s) first, then apply per-field values
#     inside session$onFlushed(once = TRUE) once the resulting renderUI()
#     has actually re-rendered with the right number of fields. The shared
#     module itself doesn't need to know about any of that; it just calls
#     on_load() once.
#
# The "viewing saved scenario" banner clears itself automatically once the
# user makes a real edit -- by diffing get_dirty_signal()'s current value
# against a snapshot taken from what was just loaded, not by tracking a
# separate "is this dirty" flag. Since get_dirty_signal() is a host-supplied
# closure that reads the host's own input$xxx values, calling it from
# inside this module's own reactive still correctly establishes a
# dependency on those inputs (Shiny reactivity depends on where a value is
# *read*, not where the closure was *defined*).
#
# The comparison itself is debounced (600ms), not run on every raw change --
# a two-phase on_load() (see above) sets row counts immediately and defers
# field values to its own session$onFlushed(), so get_dirty_signal() passes
# through a real, but transient, mismatched state (right row count, still-
# default field values) before the deferred values land and the client
# echoes them back. Confirmed empirically: without debouncing, the banner
# vanished immediately on every load that had any dynamic sub-inputs
# (DCA Calculator's tiers, Projections' membership tiers/events), even
# though the fields themselves went on to populate correctly moments later.
# Debouncing coalesces a load's whole settling burst into one comparison
# after it quiets down, without needing to know exactly when "done" is --
# a load's several changes all land within milliseconds of each other, well
# inside the debounce window, while a genuine user edit is an isolated
# event the debounced reactive reports on its own shortly after. This is a
# different kind of timing dependency than the guessed multi-second
# Shiny-render delays this app's tour code moved away from elsewhere: it's
# coalescing a known-simultaneous burst of related changes, not guessing
# how long an unrelated render takes.
#
# The comparison itself uses isTRUE(all.equal(...)) on *normalized*
# (.normalize_for_diff(), below) values, not identical() on the raw ones --
# two separate bugs, both only visible once actually round-tripped through
# a real Postgres jsonb column (neither showed up in scenarios.R's mocked
# DBI tests, which never touch a real database):
#   1. Scalars come back as R integer where they went in as double
#      (jsonlite::fromJSON() decodes whole numbers as integer) --
#      identical() treats 8000L and 8000 as unequal. all.equal() ignores
#      the class difference while still catching a genuine value change.
#   2. Postgres's jsonb type (unlike plain json) does not preserve object
#      key order -- it stores keys in its own canonical internal order and
#      reconstructs them that way on read, regardless of the order they
#      were inserted in. Both identical() and, less obviously,
#      all.equal() compare named lists *positionally*, not by matching
#      names first (confirmed: all.equal(list(a=1,b=2), list(b=2,a=1))
#      reports a mismatch) -- so a value saved as
#      list(ovhd_multi=, monthly_overhead=, ...) came back from Postgres
#      as list(tiers=, ovhd_multi=, income_items=, ...), comparing
#      unrelated fields against each other. .normalize_for_diff()
#      recursively sorts every *named* list by name before comparing
#      (leaving unnamed lists -- e.g. the `tiers` array itself, where row
#      order is meaningful UI state -- untouched).
# This was the harder pair of bugs to isolate: symptom #2 in particular
# looked exactly like a timing race (debouncing was added first, which
# correctly fixed the transient-mismatch problem described above but left
# this one in place) until direct before/after inspection of a real
# save+load round trip against Postgres showed the actual field names on
# each side, not the timing, were still misaligned.

#' @noRd
.normalize_for_diff <- function(x) {
  if (is.list(x)) {
    x <- lapply(x, .normalize_for_diff)
    nm <- names(x)
    if (!is.null(nm) && all(nzchar(nm))) {
      x <- x[order(nm)]
    }
    x
  } else {
    x
  }
}

#' Save/Load scenario-slot action buttons
#'
#' Drop into any tab's own action bar (e.g. a `.tour_nav_footer()` extra
#' slot, or a bespoke bottom row) -- deliberately no styling opinions beyond
#' `bsicons::bs_icon()` on each button, so it reads consistently wherever
#' it's placed without fighting the host app's own layout/CSS.
#'
#' @param id Module namespace ID.
#' @export
mod_scenario_slots_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::actionButton(
      ns("save_click"),
      shiny::tagList(bsicons::bs_icon("save"), " Save Scenario"),
      class = "btn-outline-secondary"
    ),
    shiny::actionButton(
      ns("load_click"),
      shiny::tagList(bsicons::bs_icon("folder2-open"), " Load Scenario"),
      class = "btn-outline-secondary"
    )
  )
}

#' "Viewing saved scenario" banner
#'
#' @param id Module namespace ID -- must match the corresponding
#'   [mod_scenario_slots_server()] call.
#' @export
mod_scenario_banner_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("banner"))
}

#' @noRd
.fmt_scenario_updated_at <- function(x) {
  format(as.POSIXct(x, tz = "UTC"), "%b %d, %Y %I:%M %p")
}

#' @noRd
.scenario_slot_choice_names <- function(slots) {
  lapply(seq_len(nrow(slots)), function(i) {
    shiny::tags$span(
      shiny::tags$strong(slots$label[i]),
      " — updated ", .fmt_scenario_updated_at(slots$updated_at[i])
    )
  })
}

#' Save/Load scenario-slot server logic
#'
#' Owns the Save/Load modals (including the overwrite-slot picker once a
#' save would exceed the 3-slot cap or collide with an existing label) and
#' the "viewing saved scenario" banner state -- see the file-level comment
#' above for the full design.
#'
#' @param id Module namespace ID.
#' @param table One of `"plan_scenarios"`, `"dca_calculator_scenarios"`,
#'   `"dca_forecast_scenarios"`.
#' @param get_con A zero-arg function returning a fresh, open `DBI`
#'   connection for each action (opened and closed within a single
#'   observer, matching this app's existing `db_connect()`/`on.exit()`
#'   convention -- never held open across the module's lifetime).
#' @param practice_id A zero-arg function (e.g. `function() r$practice_id`)
#'   returning the current practice's id.
#' @param get_inputs_for_save For the two JSONB tables: a zero-arg function
#'   returning the plain R list to save. Ignored when `is_forecast = TRUE`.
#' @param get_bundle_for_save For `dca_forecast_scenarios` only: a zero-arg
#'   function returning the full input+results bundle list (see
#'   `SAVED_SCENARIOS.md`). Required when `is_forecast = TRUE`.
#' @param get_source_filename For `dca_forecast_scenarios` only: a zero-arg
#'   function returning the originating upload's filename, or `NULL`.
#' @param get_dirty_signal A zero-arg function returning a lightweight,
#'   `identical()`-comparable snapshot of whatever should trigger
#'   auto-clearing the banner on a real edit -- for the two JSONB tables,
#'   typically the same value as `get_inputs_for_save()`; for
#'   `dca_forecast_scenarios`, a lighter subset (the Projections tab's own
#'   current input settings, not the full transactions/results bundle).
#' @param on_load A one-arg function called with the loaded scenario's
#'   `inputs` (JSONB tables) or `bundle` (forecast table) once the user
#'   picks one to load -- see the file-level comment above for the
#'   two-phase-load expectation for hosts with dynamic sub-inputs.
#' @param extract_dirty_signal A one-arg function applied to whatever was
#'   just passed to `on_load()`, producing the snapshot compared against
#'   `get_dirty_signal()`'s live readings. Defaults to the identity
#'   function (correct for the two JSONB tables, where the full loaded
#'   `inputs` list already matches `get_dirty_signal()`'s shape).
#' @param is_forecast `TRUE` for `dca_forecast_scenarios`, `FALSE`
#'   (default) for the two JSONB tables.
#' @param has_access A zero-arg function returning `TRUE`/`FALSE`, checked
#'   before either the Save or Load modal opens. Defaults to always-`TRUE`
#'   (unrestricted) -- this module has no concept of billing/plans itself;
#'   a host app that gates Save/Load passes its own paid-plan check here
#'   (e.g. `function() .has_paid_plan(r$plan_tier)`). Checked here, not
#'   left to the host's own UI alone, because this module's observers are
#'   registered once, unconditionally, regardless of what UI is currently
#'   shown -- a host swapping in a locked-looking button for `save_click`/
#'   `load_click` (see `mod_scenario_slots_ui()`'s host-side callers) stops
#'   a normal user from reaching the real modal, but doesn't stop a
#'   `Shiny.setInputValue()` call targeting this module's own input ids
#'   directly. Same "client-facing gate, not a security boundary" caveat
#'   as every other paywall gate in this codebase applies beyond that.
#' @param on_access_denied A zero-arg function called instead of opening
#'   the real modal when `has_access()` is `FALSE` -- typically a host's
#'   own "here's what you're missing" modal. Defaults to a no-op.
#' @return `list(loaded_label = <reactive>)`.
#' @export
mod_scenario_slots_server <- function(id, table, get_con, practice_id,
                                       get_inputs_for_save = NULL,
                                       get_bundle_for_save = NULL,
                                       get_source_filename = NULL,
                                       get_dirty_signal,
                                       on_load,
                                       extract_dirty_signal = function(x) x,
                                       is_forecast = FALSE,
                                       has_access = function() TRUE,
                                       on_access_denied = function() invisible(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    loaded_label <- shiny::reactiveVal(NULL)
    loaded_snapshot <- shiny::reactiveVal(NULL)
    picker_slots <- shiny::reactiveVal(NULL)
    load_slots <- shiny::reactiveVal(NULL)

    # Keeps the overwrite-picker's label field in sync with whichever slot
    # is currently selected -- defaults to that slot's own current label
    # (SAVED_SCENARIOS.md: "just overwrite" needs no typing), not whatever
    # the user originally typed in the failed plain-save attempt that got
    # them here. Registered once at module init (not inside the observer
    # that opens the modal) so repeated save attempts don't accumulate
    # duplicate observers.
    shiny::observeEvent(input$overwrite_choice, {
      slots <- picker_slots()
      shiny::req(slots)
      idx <- match(input$overwrite_choice, as.character(slots$id))
      if (!is.na(idx)) {
        shiny::updateTextInput(session, "overwrite_label", value = slots$label[idx])
      }
    })

    output$banner <- shiny::renderUI({
      label <- loaded_label()
      if (is.null(label)) {
        return(NULL)
      }
      shiny::tags$div(
        class = "alert alert-info d-flex justify-content-between align-items-center py-2 px-3 mb-3",
        shiny::tags$span(
          bsicons::bs_icon("bookmark-check"), " Viewing saved scenario: ",
          shiny::tags$strong(label)
        ),
        shiny::actionLink(ns("dismiss_banner"), "Return to live session", class = "small")
      )
    })

    shiny::observeEvent(input$dismiss_banner, {
      loaded_label(NULL)
    })

    # Auto-clear the banner on a real edit -- a value diff (see the
    # file-level comment), not a suppress-then-timeout flag, but debounced:
    # a two-phase on_load() (dynamic sub-inputs -- membership tiers,
    # overhead/fee events, calculator tiers/sources) sets row *counts*
    # immediately and defers *values* to its own session$onFlushed(), which
    # means get_dirty_signal() briefly reads a real, but transient,
    # mismatched state (right row count, still-default field values) before
    # the deferred update lands and the client echoes it back -- confirmed
    # empirically: without debouncing, the banner vanished immediately on
    # every load with dynamic sub-inputs, even though the fields themselves
    # went on to populate correctly a moment later. Debouncing coalesces
    # that whole settling burst into one comparison after it quiets down,
    # without needing to know exactly when "done" is -- a load's several
    # changes all land within milliseconds of each other, well inside the
    # debounce window, while a genuine user edit is an isolated event that
    # the debounced reactive reports on its own shortly after.
    dirty_signal <- shiny::reactive(get_dirty_signal())
    dirty_signal_debounced <- shiny::debounce(dirty_signal, millis = 600)

    shiny::observeEvent(dirty_signal_debounced(), {
      current <- .normalize_for_diff(dirty_signal_debounced())
      snap <- .normalize_for_diff(shiny::isolate(loaded_snapshot()))
      if (!is.null(snap) && !is.null(shiny::isolate(loaded_label())) && !isTRUE(all.equal(current, snap))) {
        loaded_label(NULL)
        loaded_snapshot(NULL)
      }
    }, ignoreInit = TRUE)

    # -- Save flow ----------------------------------------------------------
    shiny::observeEvent(input$save_click, {
      if (!isTRUE(has_access())) {
        on_access_denied()
        return(invisible(NULL))
      }
      shiny::showModal(shiny::modalDialog(
        title = "Save Scenario",
        shiny::textInput(ns("save_label"), "Label", placeholder = "e.g. Optimistic"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("save_confirm"), "Save", class = "btn-primary")
        )
      ))
    })

    .do_save <- function(label, overwrite_id = NULL) {
      con <- get_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      if (is_forecast) {
        scenario_save_forecast(
          con, practice_id(), label, get_bundle_for_save(),
          source_filename = if (is.null(get_source_filename)) NULL else get_source_filename(),
          overwrite_id = overwrite_id
        )
      } else {
        scenario_save_json(con, table, practice_id(), label, get_inputs_for_save(), overwrite_id = overwrite_id)
      }
    }

    shiny::observeEvent(input$save_confirm, {
      label <- trimws(if (is.null(input$save_label)) "" else input$save_label)
      if (nchar(label) == 0) {
        shiny::showNotification("Enter a label before saving.", type = "warning")
        return()
      }

      result <- .do_save(label)
      if (result$ok) {
        shiny::removeModal()
        shiny::showNotification(paste0("Saved as \"", label, "\"."), type = "message")
        return()
      }

      con <- get_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      slots <- scenario_list(con, table, practice_id())
      picker_slots(slots)

      # label_collision: default to the slot that already owns this exact
      # label -- obviously the one meant. cap_reached: no such signal
      # exists, so default to the first (most-recently-updated, per
      # scenario_list()'s own ORDER BY) slot instead.
      default_idx <- if (identical(result$reason, "label_collision")) {
        match(label, slots$label)
      } else {
        1L
      }
      if (is.na(default_idx)) {
        default_idx <- 1L
      }

      shiny::showModal(shiny::modalDialog(
        title = "Choose a slot to overwrite",
        shiny::tags$p(
          class = "text-muted small",
          if (identical(result$reason, "cap_reached")) {
            "You already have 3 saved scenarios here. Pick one to overwrite."
          } else {
            paste0("\"", label, "\" is already in use. Pick a slot to overwrite, or rename it below.")
          }
        ),
        shiny::radioButtons(
          ns("overwrite_choice"), NULL,
          choiceNames = .scenario_slot_choice_names(slots),
          choiceValues = slots$id,
          selected = slots$id[default_idx]
        ),
        shiny::textInput(ns("overwrite_label"), "Label", value = slots$label[default_idx]),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("overwrite_confirm"), "Overwrite", class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$overwrite_confirm, {
      shiny::req(input$overwrite_choice)
      new_label <- trimws(if (is.null(input$overwrite_label)) "" else input$overwrite_label)
      if (nchar(new_label) == 0) {
        shiny::showNotification("Enter a label before saving.", type = "warning")
        return()
      }

      result <- .do_save(new_label, overwrite_id = as.integer(input$overwrite_choice))
      if (result$ok) {
        shiny::removeModal()
        shiny::showNotification(paste0("Saved as \"", new_label, "\"."), type = "message")
      }
    })

    # -- Load flow ------------------------------------------------------------
    .show_load_modal <- function(slots) {
      load_slots(slots)
      shiny::showModal(shiny::modalDialog(
        title = "Load Scenario",
        shiny::radioButtons(
          ns("load_choice"), NULL,
          choiceNames = .scenario_slot_choice_names(slots),
          choiceValues = slots$id
        ),
        footer = shiny::tagList(
          shiny::actionButton(ns("delete_click"), "Delete", class = "btn-outline-danger me-auto"),
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("load_confirm"), "Load", class = "btn-primary")
        )
      ))
    }

    shiny::observeEvent(input$load_click, {
      if (!isTRUE(has_access())) {
        on_access_denied()
        return(invisible(NULL))
      }
      con <- get_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      slots <- scenario_list(con, table, practice_id())

      if (nrow(slots) == 0) {
        shiny::showModal(shiny::modalDialog(
          title = "Load Scenario",
          shiny::tags$p(class = "text-muted", "No saved scenarios yet -- use Save Scenario first."),
          footer = shiny::modalButton("Close")
        ))
        return()
      }

      .show_load_modal(slots)
    })

    shiny::observeEvent(input$load_confirm, {
      shiny::req(input$load_choice)
      con <- get_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      loaded <- if (is_forecast) {
        scenario_load_forecast(con, as.integer(input$load_choice))
      } else {
        scenario_load_json(con, table, as.integer(input$load_choice))
      }

      shiny::removeModal()

      if (is.null(loaded)) {
        shiny::showNotification("That scenario no longer exists.", type = "error")
        return()
      }

      loaded_value <- if (is_forecast) loaded$bundle else loaded$inputs
      on_load(loaded_value)
      loaded_snapshot(extract_dirty_signal(loaded_value))
      loaded_label(loaded$label)
    })

    # Deletion is a two-step confirm (a plain "Delete" click opens a second,
    # narrower modalDialog asking to confirm that exact slot's label) since
    # it's destructive and, unlike a save overwrite, has no "just save again
    # to undo" recovery path. Reuses load_slots() (set by .show_load_modal(),
    # the same slot list already on screen) rather than re-querying, so the
    # confirm text can show the slot's own label without another round trip.
    shiny::observeEvent(input$delete_click, {
      shiny::req(input$load_choice)
      slots <- load_slots()
      shiny::req(slots)
      idx <- match(input$load_choice, as.character(slots$id))
      shiny::req(!is.na(idx))

      shiny::showModal(shiny::modalDialog(
        title = "Delete Scenario",
        shiny::tags$p(
          "Delete \"", shiny::tags$strong(slots$label[idx]), "\"? This can't be undone."
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("delete_confirm"), "Delete", class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$delete_confirm, {
      shiny::req(input$load_choice)
      con <- get_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      deleted_id <- as.integer(input$load_choice)
      ok <- scenario_delete(con, table, practice_id(), deleted_id)

      if (!ok) {
        shiny::removeModal()
        shiny::showNotification("That scenario no longer exists.", type = "error")
        return()
      }

      # Clears the "viewing saved scenario" banner if the deleted slot was
      # the one currently being viewed -- otherwise the banner would keep
      # naming a scenario that no longer exists.
      slots <- load_slots()
      idx <- match(deleted_id, slots$id)
      if (!is.na(idx) && identical(shiny::isolate(loaded_label()), slots$label[idx])) {
        loaded_label(NULL)
        loaded_snapshot(NULL)
      }

      shiny::showNotification("Scenario deleted.", type = "message")

      remaining <- scenario_list(con, table, practice_id())
      if (nrow(remaining) == 0) {
        shiny::removeModal()
      } else {
        .show_load_modal(remaining)
      }
    })

    list(loaded_label = shiny::reactive(loaded_label()))
  })
}
