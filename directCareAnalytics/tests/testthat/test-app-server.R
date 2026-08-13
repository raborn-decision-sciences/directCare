# Unit tests for app_server's global Start Over logic and Account Settings
# profile save.

# Regression coverage for a real, session-crashing bug found live 2026-08-13
# via shinyloadtest::record_session(): output$main_nav_footer is
# suspendWhenHidden = FALSE (evaluates eagerly, before the client
# necessarily reports the navbar's initial selected tab back), and its
# switch(input$main_nav, ...) threw ("EXPR must be a length 1 vector") on
# NULL -- an uncaught error here kills the whole Shiny session. Normally a
# tight enough race against a direct browser connection to go unnoticed;
# reliably lost through the recorder proxy's own relay overhead.
test_that("main_nav_footer doesn't error when input$main_nav is still NULL", {
  testServer(app_server, {
    session$flushReact()
    expect_no_error(session$getOutput("main_nav_footer"))
  })
})

test_that("global_confirm_start_over clears data but keeps practice identity", {
  testServer(app_server, {
    r$practice_id <- "river-dpc"
    r$practice_name <- "River DPC"
    r$panel_size <- 80
    r$membership_fee <- 90
    r$membership_tiers <- list(list(label = "Adult", members = 80, fee = 90))
    r$transactions <- tibble::tibble(date = as.Date(character()))
    r$overhead <- tibble::tibble(date = as.Date(character()))
    r$income <- tibble::tibble(date = as.Date(character()))
    r$overhead_monthly <- tibble::tibble(x = 1)
    r$income_monthly <- tibble::tibble(x = 1)
    r$scenario_inputs <- list(a = 1)
    r$validation <- list(warn = "test")

    # A leading no-op flush is needed so the real trigger below isn't
    # consumed by the observer's ignoreInit skip.
    session$setInputs(global_confirm_start_over = 0)
    session$setInputs(global_confirm_start_over = 1)

    expect_equal(r$practice_id, "river-dpc")
    expect_equal(r$practice_name, "River DPC")
    expect_null(r$panel_size)
    expect_null(r$membership_fee)
    expect_null(r$membership_tiers)
    expect_null(r$transactions)
    expect_null(r$overhead)
    expect_null(r$income)
    expect_null(r$overhead_monthly)
    expect_null(r$income_monthly)
    expect_null(r$scenario_inputs)
    expect_equal(r$validation, list())
    expect_equal(r$reset_signal, 1L)
  })
})

test_that("account_save_profile saves the optional profile fields and writes them back to res_auth", {
  captured <- NULL
  local_mocked_bindings(
    db_checkout = function() structure(list(), class = "mock_con"),
    practice_update_profile = function(con, practice_id, practice_name, address, ...) {
      captured <<- list(...)
      list(ok = TRUE)
    },
    auth_event_log = function(...) invisible(NULL),
    .package = "directCareAuth"
  )
  local_mocked_bindings(db_release = function(con) invisible(NULL), .package = "directCareAuth")

  res_auth <- reactiveValues(
    practice_id = 7L, email = "doc@example.com",
    practice_name = "River DPC", address = "",
    first_name = "", last_name = "", practice_type = "",
    practice_type_other = "", practice_status = "",
    practice_specialty = "", referral_source = ""
  )

  testServer(function(input, output, session) {
    app_server(input, output, session, res_auth = res_auth)
  }, {
    session$setInputs(
      account_practice_name = "River DPC", account_address = "123 Main St",
      account_first_name = "Jane", account_last_name = "Smith",
      account_practice_type = "Physician", account_practice_status = "Just exploring",
      account_practice_specialty = "Primary Care", account_referral_source = "Word of mouth",
      account_save_profile = 1
    )

    expect_equal(captured$first_name, "Jane")
    expect_equal(captured$last_name, "Smith")
    expect_equal(captured$practice_type, "Physician")
    expect_equal(captured$practice_status, "Just exploring")
    expect_equal(captured$practice_specialty, "Primary Care")
    expect_equal(captured$referral_source, "Word of mouth")
    expect_equal(res_auth$first_name, "Jane")
    expect_equal(res_auth$practice_type, "Physician")
  })
})

# -- Guided tour chapter transitions ----------------------------------------
# Regression coverage for a real bug found live 2026-08-12: .tour_advance()
# used to key its "already shown this chapter" tracking off guide$id, which
# cicerone's Cicerone R6 object does not actually expose as a public field
# (it's always NULL despite being a constructor arg) -- `NULL %in% x`
# produces a zero-length result, and `FALSE || logical(0)` evaluates to NA
# in R, which crashed .tour_advance()'s `if()` with "missing value where
# TRUE/FALSE needed" on the very first chapter transition of *any* tour.
# An uncaught error inside an observeEvent kills the whole Shiny session by
# default, which is what made this so severe live: the tour looked "stuck"
# on its first popup because the session behind it had already died.

test_that(".tour_advance() does not error advancing into a real chapter", {
  testServer(app_server, {
    session$setInputs(launch_tour_historical = 1)
    session$setInputs(cicerone_ready = 1)
    expect_equal(active_tour(), "historical")

    expect_no_error(session$setInputs(`upload-btn_use_real` = 1))
    expect_equal(visited_chapters(), "upload-csv_file")
  })
})

test_that(".tour_advance() does not replay a chapter already shown this run", {
  testServer(app_server, {
    session$setInputs(launch_tour_historical = 1)
    session$setInputs(cicerone_ready = 1)
    session$setInputs(`upload-btn_use_real` = 1)
    expect_equal(visited_chapters(), "upload-csv_file")

    # Simulate going back to Upload and re-clicking the same button --
    # active_tour is still "historical" (nothing resets it to NULL), so
    # without the visited-chapters guard this would replay h2's popover on
    # perfectly ordinary, non-tour navigation.
    expect_no_error(session$setInputs(`upload-btn_use_real` = 2))
    expect_equal(visited_chapters(), "upload-csv_file")
  })
})

test_that("re-opening Quick Calculator after finishing its tour doesn't replay it", {
  testServer(app_server, {
    session$setInputs(launch_tour_calculator = 1)
    session$setInputs(cicerone_ready = 1)
    session$setInputs(`upload-btn_use_calculator` = 1)
    expect_equal(visited_chapters(), "upload-calculator-monthly_overhead")

    expect_no_error(session$setInputs(`upload-btn_use_calculator` = 2))
    expect_equal(visited_chapters(), "upload-calculator-monthly_overhead")
  })
})

test_that("re-opening Plan My Practice after finishing its tour doesn't replay it", {
  testServer(app_server, {
    session$setInputs(launch_tour_plan = 1)
    session$setInputs(cicerone_ready = 1)
    session$setInputs(`upload-btn_use_plan` = 1)
    expect_equal(visited_chapters(), "edit-est_rent")

    expect_no_error(session$setInputs(`upload-btn_use_plan` = 2))
    expect_equal(visited_chapters(), "edit-est_rent")
  })
})

# -- Projections tab (proj2/proj2b/proj2c): forecast_type tab clicks --------
# guide_proj2/proj2b/proj2c cover Break-even/Revenue Forecast/Income Target
# respectively -- split into 3 chapters because bslib's navset hides
# inactive tab-pane content entirely, so a single chapter spanning all three
# would have had its Revenue/Target steps silently dropped by cicerone at
# $init() time (same failure class as guide$id, just for a different
# reason). Reaching proj2 for real requires a completed forecast (async,
# mirai-based) -- these tests instead set active_tour()/visited_chapters()
# directly, exactly as if guide_proj2 had already started, and exercise only
# the forecast_type tab-click observer itself.

test_that("clicking the Revenue Forecast tab advances proj2 -> proj2b", {
  testServer(app_server, {
    active_tour("historical")
    # Leading no-op flush -- see this file's very first test for why: the
    # observer's ignoreInit skip would otherwise consume the real trigger
    # below, since this is the first time this input is ever set.
    session$setInputs(`projections-forecast_type` = "breakeven")
    expect_no_error(session$setInputs(`projections-forecast_type` = "revenue"))
    expect_true("projections-revenue_plot" %in% visited_chapters())
  })
})

test_that("clicking the Income Target tab advances proj2 -> proj2c", {
  testServer(app_server, {
    active_tour("historical")
    session$setInputs(`projections-forecast_type` = "breakeven")
    expect_no_error(session$setInputs(`projections-forecast_type` = "target"))
    expect_true("projections-target_plot" %in% visited_chapters())
  })
})

test_that("re-clicking the same forecast_type tab doesn't replay its chapter", {
  testServer(app_server, {
    active_tour("plan")
    session$setInputs(`projections-forecast_type` = "breakeven")
    session$setInputs(`projections-forecast_type` = "revenue")
    expect_equal(visited_chapters(), "projections-revenue_plot")

    expect_no_error(session$setInputs(`projections-forecast_type` = "revenue"))
    expect_equal(visited_chapters(), "projections-revenue_plot")
  })
})
