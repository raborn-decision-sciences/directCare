#' Minimum viable password-strength check
#'
#' Deliberately simple: a length floor is the cheapest check that rules out
#' the bulk of trivially guessable passwords without a dependency on an
#' external strength-scoring library. Revisit if real-world signups show
#' this isn't enough.
#' @noRd
password_is_strong <- function(password) {
  is.character(password) && length(password) == 1 && nchar(password) >= 10
}

#' Look up a practice by login email
#'
#' @param con An open `DBI` connection (see [db_connect()]).
#' @param email The email address to look up.
#' @return A data frame with 0 or 1 rows and columns `id`, `practice_name`,
#'   `email`, `password_hash`, `address`.
#' @export
practice_find_by_email <- function(con, email) {
  DBI::dbGetQuery(
    con,
    "SELECT id, practice_name, email, password_hash, address
       FROM practices WHERE email = $1",
    params = list(email)
  )
}

#' Create a new practice account
#'
#' @param con An open `DBI` connection.
#' @param practice_name Display name for the practice.
#' @param email Login email. Must be unique across `practices`.
#' @param password Plaintext password; hashed here via `bcrypt::hashpw()`
#'   before it ever reaches the database.
#' @return `list(ok = TRUE, id = <new practice id>)` on success, or
#'   `list(ok = FALSE, reason = "email_taken" | "weak_password")`.
#' @export
practice_create <- function(con, practice_name, email, password) {
  if (!password_is_strong(password)) {
    return(list(ok = FALSE, reason = "weak_password"))
  }

  password_hash <- bcrypt::hashpw(password, bcrypt::gensalt())

  # ON CONFLICT DO NOTHING + checking row count, rather than a preceding
  # SELECT, avoids a check-then-insert race between two concurrent signups
  # for the same email.
  inserted <- DBI::dbGetQuery(
    con,
    "INSERT INTO practices (practice_name, email, password_hash)
       VALUES ($1, $2, $3)
       ON CONFLICT (email) DO NOTHING
       RETURNING id",
    params = list(practice_name, email, password_hash)
  )

  if (nrow(inserted) != 1) {
    return(list(ok = FALSE, reason = "email_taken"))
  }

  list(ok = TRUE, id = inserted$id[[1]])
}

#' Change a practice's password
#'
#' Requires the current password, matching the account-settings modal's
#' "must know current password" flow (no email-based reset exists yet).
#'
#' @param con An open `DBI` connection.
#' @param practice_id The practice's `practices.id`.
#' @param current_password The account's current plaintext password.
#' @param new_password The desired new plaintext password.
#' @return `list(ok = TRUE)` on success, or
#'   `list(ok = FALSE, reason = "wrong_current_password" | "weak_password")`.
#' @export
practice_update_password <- function(con, practice_id, current_password, new_password) {
  practice <- DBI::dbGetQuery(
    con,
    "SELECT password_hash FROM practices WHERE id = $1",
    params = list(practice_id)
  )

  if (nrow(practice) != 1 || !bcrypt::checkpw(current_password, practice$password_hash)) {
    return(list(ok = FALSE, reason = "wrong_current_password"))
  }

  if (!password_is_strong(new_password)) {
    return(list(ok = FALSE, reason = "weak_password"))
  }

  new_hash <- bcrypt::hashpw(new_password, bcrypt::gensalt())
  DBI::dbExecute(
    con,
    "UPDATE practices SET password_hash = $1 WHERE id = $2",
    params = list(new_hash, practice_id)
  )

  list(ok = TRUE)
}

#' Update a practice's profile fields
#'
#' @param con An open `DBI` connection.
#' @param practice_id The practice's `practices.id`.
#' @param practice_name New display name.
#' @param address New address (may be `NA`/empty).
#' @return `list(ok = TRUE)`.
#' @export
practice_update_profile <- function(con, practice_id, practice_name, address) {
  DBI::dbExecute(
    con,
    "UPDATE practices SET practice_name = $1, address = $2 WHERE id = $3",
    params = list(practice_name, address, practice_id)
  )

  list(ok = TRUE)
}
