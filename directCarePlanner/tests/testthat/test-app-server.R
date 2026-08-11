# Unit tests for app_server's Account Settings profile save.

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
