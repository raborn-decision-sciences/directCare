#' Read the ZeptoMail API token
#'
#' Mirrors [db_connect()]'s `*_FILE`-env-var-with-fallback pattern -- the
#' token comes from a Docker secret file (`ZEPTOMAIL_TOKEN_FILE`) when
#' present, falling back to a plain `ZEPTOMAIL_TOKEN` env var for local
#' dev.
#' @noRd
.zeptomail_token <- function() {
  token_file <- Sys.getenv("ZEPTOMAIL_TOKEN_FILE", unset = NA)
  if (!is.na(token_file) && file.exists(token_file)) {
    return(trimws(readLines(token_file, warn = FALSE)))
  }
  Sys.getenv("ZEPTOMAIL_TOKEN", unset = "")
}

#' Minimal HTML-escaping for values interpolated into the email body
#'
#' practice_name is user-supplied (from signup); escaping it here avoids
#' HTML injection in the rendered email. Deliberately not a dependency on
#' htmltools (a UI-layer package) for four lines of base R.
#' @noRd
.html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

#' Send a password-reset email via ZeptoMail
#'
#' POSTs to ZeptoMail's transactional send API. The from-address comes
#' from a `FROM_EMAIL` env var (not hardcoded), so it works whichever
#' domain is configured to send, without a code change.
#'
#' @param to_email Recipient email address.
#' @param practice_name Recipient's practice name, for a personalized
#'   greeting.
#' @param reset_url The full URL (including token) the user clicks to set
#'   a new password.
#' @return The `httr2` response, invisibly. Errors (network failure, a
#'   non-2xx response) propagate as a plain R error -- a failed send
#'   needs to surface to the caller, not be silently swallowed.
#' @export
send_password_reset_email <- function(to_email, practice_name, reset_url) {
  token <- .zeptomail_token()
  if (!nzchar(token)) {
    stop("ZeptoMail API token is not configured (ZEPTOMAIL_TOKEN_FILE/ZEPTOMAIL_TOKEN)")
  }

  from_email <- Sys.getenv("FROM_EMAIL", unset = "")
  if (!nzchar(from_email)) {
    stop("FROM_EMAIL is not configured")
  }

  safe_name <- .html_escape(practice_name %||% "")
  safe_url <- .html_escape(reset_url)

  body <- list(
    from = list(address = from_email),
    to = list(list(email_address = list(address = to_email, name = practice_name %||% ""))),
    subject = "Reset your password",
    htmlbody = paste0(
      "<p>Hi ", safe_name, ",</p>",
      "<p>Click the link below to reset your password. ",
      "This link expires in 1 hour and can only be used once.</p>",
      "<p><a href=\"", safe_url, "\">Reset your password</a></p>",
      "<p>If you didn't request this, you can safely ignore this email.</p>"
    )
  )

  httr2::request("https://api.zeptomail.com/v1.1/email") |>
    httr2::req_headers(
      Authorization = paste("Zoho-enczapikey", token),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_perform() |>
    invisible()
}

# NULL-coalescing operator for values that may be NULL/NA before use.
`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
