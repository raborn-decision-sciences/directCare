#' Default password-reset parameters
#'
#' 3 requests / 60 minutes for the rate limit, 60 minutes for token
#' expiry -- reasonable defaults, not derived from measured abuse.
#' Revisit if real usage suggests otherwise.
#' @noRd
DEFAULT_RESET_MAX_REQUESTS <- 3L
#' @noRd
DEFAULT_RESET_WINDOW_MINUTES <- 60L
#' @noRd
DEFAULT_RESET_TOKEN_EXPIRY_MINUTES <- 60L

#' Generate a hex-encoded cryptographically random token
#' @noRd
.generate_reset_token <- function() {
  paste(sprintf("%02x", as.integer(openssl::rand_bytes(32))), collapse = "")
}

#' Hash a reset token for storage
#'
#' A fast hash (SHA-256), not bcrypt -- the token is already a
#' high-entropy random value, not a low-entropy password, so there's no
#' need for bcrypt's deliberate slowness here.
#' @noRd
.hash_reset_token <- function(token) {
  as.character(openssl::sha256(token))
}

#' Check whether an email has requested too many password resets recently
#'
#' Same pattern as [auth_is_locked_out()] -- counts recent
#' `"password_reset_requested"` `auth_events` rows. Checked (and logged)
#' regardless of whether the email maps to a real account, so the rate
#' limit itself can't be used to probe which emails have accounts.
#'
#' @param con An open `DBI` connection.
#' @param email The email to check.
#' @param max_requests Request threshold. Defaults to 3.
#' @param window_minutes Lookback window, in minutes. Defaults to 60.
#' @return `TRUE` if rate-limited, `FALSE` otherwise.
#' @export
password_reset_is_rate_limited <- function(con, email,
                                            max_requests = DEFAULT_RESET_MAX_REQUESTS,
                                            window_minutes = DEFAULT_RESET_WINDOW_MINUTES) {
  result <- DBI::dbGetQuery(
    con,
    "SELECT count(*) AS n FROM auth_events
       WHERE email = $1 AND event_type = 'password_reset_requested'
         AND created_at > now() - make_interval(mins => $2)",
    params = list(email, as.integer(window_minutes))
  )
  isTRUE(result$n[[1]] >= max_requests)
}

#' Request a password reset token for an email
#'
#' Looks up the practice by email; if found, generates a random token,
#' stores its hash with an expiry, and returns the raw token for the
#' caller to embed in a reset-link email (via [send_password_reset_email()]).
#'
#' Callers must not reveal the `ok`/`FALSE` distinction to the end user --
#' this function returning `list(ok = FALSE)` for an unrecognized email,
#' silently, is exactly what prevents the reset-request form from being
#' usable to test which emails have accounts. Show the same generic
#' "if an account exists, we've sent a link" message either way.
#'
#' @param con An open `DBI` connection.
#' @param email The email address requesting a reset.
#' @param expiry_minutes How long the token stays valid. Defaults to 60.
#' @return `list(ok = TRUE, token = <raw token>, practice_id = , practice_name = )`
#'   or `list(ok = FALSE)`.
#' @export
password_reset_request <- function(con, email, expiry_minutes = DEFAULT_RESET_TOKEN_EXPIRY_MINUTES) {
  practice <- practice_find_by_email(con, email)
  if (nrow(practice) != 1) {
    return(list(ok = FALSE))
  }

  token <- .generate_reset_token()
  token_hash <- .hash_reset_token(token)

  DBI::dbExecute(
    con,
    "INSERT INTO password_resets (practice_id, token_hash, expires_at)
       VALUES ($1, $2, now() + make_interval(mins => $3))",
    params = list(practice$id, token_hash, as.integer(expiry_minutes))
  )

  list(ok = TRUE, token = token, practice_id = practice$id, practice_name = practice$practice_name)
}

#' Consume a password reset token, setting a new password
#'
#' @param con An open `DBI` connection.
#' @param token The raw token from the reset link.
#' @param new_password The desired new plaintext password.
#' @return `list(ok = TRUE, practice_id = )` on success, or
#'   `list(ok = FALSE, reason = "invalid_or_expired_token" | "weak_password")`.
#' @export
password_reset_consume <- function(con, token, new_password) {
  if (!is.character(token) || length(token) != 1 || !nzchar(token)) {
    return(list(ok = FALSE, reason = "invalid_or_expired_token"))
  }

  token_hash <- .hash_reset_token(token)
  reset_row <- DBI::dbGetQuery(
    con,
    "SELECT id, practice_id FROM password_resets
       WHERE token_hash = $1 AND used_at IS NULL AND expires_at > now()",
    params = list(token_hash)
  )

  if (nrow(reset_row) != 1) {
    return(list(ok = FALSE, reason = "invalid_or_expired_token"))
  }

  if (!password_is_strong(new_password)) {
    return(list(ok = FALSE, reason = "weak_password"))
  }

  new_hash <- bcrypt::hashpw(new_password, bcrypt::gensalt())
  DBI::dbExecute(
    con,
    "UPDATE practices SET password_hash = $1 WHERE id = $2",
    params = list(new_hash, reset_row$practice_id[[1]])
  )
  DBI::dbExecute(
    con,
    "UPDATE password_resets SET used_at = now() WHERE id = $1",
    params = list(reset_row$id[[1]])
  )

  list(ok = TRUE, practice_id = reset_row$practice_id[[1]])
}
