#' Authenticate a practice login against the `practices` table
#'
#' Passed as `check_credentials` to `shinymanager::secure_server()`. Beta:
#' `subscription_status` is not checked here -- all accounts stay 'trial'
#' and access is gated on a valid email/password pair only. Wire in a
#' status check once billing is added.
#'
#' Failed attempts and successful logins are recorded via
#' `directCareAuth::auth_event_log()`; an email with too many recent
#' failures is locked out before its password is even checked (see
#' `directCareAuth::auth_is_locked_out()`).
#' @noRd
check_credentials_db <- function(user, password) {
  con <- directCareAuth::db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (directCareAuth::auth_is_locked_out(con, user)) {
    directCareAuth::auth_event_log(con, event_type = "login_locked_out", email = user)
    return(list(result = FALSE))
  }

  practice <- directCareAuth::practice_find_by_email(con, user)

  if (nrow(practice) != 1 || !bcrypt::checkpw(password, practice$password_hash)) {
    directCareAuth::auth_event_log(con, event_type = "login_failure", email = user)
    return(list(result = FALSE))
  }

  directCareAuth::auth_event_log(
    con, event_type = "login_success", email = user, practice_id = practice$id
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
