#' Default login-lockout parameters
#'
#' 5 failed attempts / 15 minute cooldown -- a reasonable default, not
#' something derived from measured abuse. Revisit if real usage suggests
#' otherwise.
#' @noRd
DEFAULT_LOCKOUT_MAX_ATTEMPTS <- 5L
#' @noRd
DEFAULT_LOCKOUT_WINDOW_MINUTES <- 15L

#' Default IP-based login-lockout threshold
#'
#' Deliberately more permissive than the email-based 5/15 -- a small
#' practice's whole staff could plausibly share one NAT'd office IP, and a
#' strict per-IP threshold risks locking out an entire office over a few
#' mistyped passwords across different people. 20/15 min is comfortably
#' above realistic shared-office login volume, still well below what an
#' automated credential-spray tool would attempt. Also not derived from
#' measured abuse -- revisit once deployed.
#' @noRd
DEFAULT_IP_LOCKOUT_MAX_ATTEMPTS <- 20L

#' Record a security-relevant auth event
#'
#' Backs both the login-lockout checks ([auth_is_locked_out()] and
#' [auth_is_locked_out_by_ip()], which count recent `"login_failure"` rows)
#' and a plain audit trail of login/account activity. `event_type` is a
#' free-form label; current callers use `"login_success"`,
#' `"login_failure"`, `"login_locked_out"`, `"password_change"`, and
#' `"profile_update"`.
#'
#' @param con An open `DBI` connection.
#' @param event_type A short label for the event (see above).
#' @param email The email involved, if any (e.g. unset for a
#'   profile/password change looked up by `practice_id` only).
#' @param practice_id The `practices.id` involved, if known (e.g. unset for
#'   a failed login against an unrecognized email).
#' @param detail Optional free-text detail.
#' @param ip_address The client's IP, if known (see
#'   [extract_client_ip()]). Unset for events with no request context
#'   (e.g. a password change triggered from an already-authenticated
#'   session action, not a fresh login attempt).
#' @return `invisible(NULL)`.
#' @export
auth_event_log <- function(con, event_type, email = NA_character_,
                            practice_id = NA_integer_, detail = NA_character_,
                            ip_address = NA_character_) {
  DBI::dbExecute(
    con,
    "INSERT INTO auth_events (practice_id, email, event_type, detail, ip_address)
       VALUES ($1, $2, $3, $4, $5)",
    params = list(practice_id, email, event_type, detail, ip_address)
  )
  invisible(NULL)
}

#' Check whether an email is currently locked out of login
#'
#' Locked out once it has accumulated `max_attempts` `"login_failure"`
#' events within the last `window_minutes` -- no separate lockout table,
#' just a count against the same `auth_events` audit log.
#'
#' @param con An open `DBI` connection.
#' @param email The login email to check.
#' @param max_attempts Failed-attempt threshold. Defaults to 5.
#' @param window_minutes Lookback window, in minutes. Defaults to 15.
#' @return `TRUE` if locked out, `FALSE` otherwise.
#' @export
auth_is_locked_out <- function(con, email,
                                max_attempts = DEFAULT_LOCKOUT_MAX_ATTEMPTS,
                                window_minutes = DEFAULT_LOCKOUT_WINDOW_MINUTES) {
  result <- DBI::dbGetQuery(
    con,
    "SELECT count(*) AS n FROM auth_events
       WHERE email = $1 AND event_type = 'login_failure'
         AND created_at > now() - make_interval(mins => $2)",
    params = list(email, as.integer(window_minutes))
  )
  isTRUE(result$n[[1]] >= max_attempts)
}

#' Check whether an IP address is currently locked out of login
#'
#' Mirror-image of [auth_is_locked_out()], keyed by `ip_address` instead of
#' `email` -- catches a credential-spray attack (many different emails
#' tried from one source) that the email-based check alone can't see.
#' `check_credentials_db()` gates on either being true, so a login is
#' blocked if *either* the email or the IP has too many recent failures.
#'
#' @param con An open `DBI` connection.
#' @param ip The client's IP to check, or `NA` if it couldn't be
#'   determined -- always returns `FALSE` in that case (nothing to gate
#'   on), rather than failing open/closed on missing data.
#' @param max_attempts Failed-attempt threshold. Defaults to 20 (see
#'   `DEFAULT_IP_LOCKOUT_MAX_ATTEMPTS`).
#' @param window_minutes Lookback window, in minutes. Defaults to 15.
#' @return `TRUE` if locked out, `FALSE` otherwise.
#' @export
auth_is_locked_out_by_ip <- function(con, ip,
                                      max_attempts = DEFAULT_IP_LOCKOUT_MAX_ATTEMPTS,
                                      window_minutes = DEFAULT_LOCKOUT_WINDOW_MINUTES) {
  if (is.na(ip)) {
    return(FALSE)
  }
  result <- DBI::dbGetQuery(
    con,
    "SELECT count(*) AS n FROM auth_events
       WHERE ip_address = $1 AND event_type = 'login_failure'
         AND created_at > now() - make_interval(mins => $2)",
    params = list(ip, as.integer(window_minutes))
  )
  isTRUE(result$n[[1]] >= max_attempts)
}
