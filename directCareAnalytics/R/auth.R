#' Authenticate a practice login against the `practices` table
#'
#' Passed as `check_credentials` to `shinymanager::secure_server()`, via a
#' closure in `run_app.R` that captures the client IP from `session`
#' (`check_credentials` itself, as shinymanager calls it, only receives
#' `user`/`password` -- no session access). Beta: `subscription_status` is
#' not checked here -- all accounts stay 'trial' and access is gated on a
#' valid email/password pair only. Wire in a status check once billing is
#' added.
#'
#' Failed attempts and successful logins are recorded via
#' `directCareAuth::auth_event_log()`; a login is locked out before its
#' password is even checked if *either* the email or the client IP has too
#' many recent failures (see `directCareAuth::auth_is_locked_out()` and
#' `directCareAuth::auth_is_locked_out_by_ip()` -- the IP check catches a
#' credential-spray attack across many emails that the email-only check
#' can't see).
#'
#' @param ip The client's IP, from `directCareAuth::extract_client_ip()`.
#'   Defaults to `NA` (e.g. in tests, where no real session exists) --
#'   `auth_is_locked_out_by_ip()` treats `NA` as "nothing to gate on".
#' @noRd
check_credentials_db <- function(user, password, ip = NA_character_) {
  con <- directCareAuth::db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (directCareAuth::auth_is_locked_out(con, user) ||
        directCareAuth::auth_is_locked_out_by_ip(con, ip)) {
    directCareAuth::auth_event_log(con, event_type = "login_locked_out", email = user, ip_address = ip)
    return(list(result = FALSE))
  }

  practice <- directCareAuth::practice_find_by_email(con, user)

  if (nrow(practice) != 1 || !bcrypt::checkpw(password, practice$password_hash)) {
    directCareAuth::auth_event_log(con, event_type = "login_failure", email = user, ip_address = ip)
    return(list(result = FALSE))
  }

  directCareAuth::auth_event_log(
    con, event_type = "login_success", email = user, practice_id = practice$id, ip_address = ip
  )

  list(
    result = TRUE,
    user_info = list(
      practice_id = practice$id,
      practice_name = practice$practice_name,
      email = practice$email,
      address = practice$address
    )
  )
}
