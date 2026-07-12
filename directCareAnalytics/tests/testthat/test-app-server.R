# Unit tests for app_server's global Start Over logic

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
