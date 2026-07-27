#' Extract the real client IP from a Shiny session's request
#'
#' Caddy is the only reverse proxy in front of this app and *appends* to
#' `X-Forwarded-For` rather than replacing it -- the last hop is always the
#' real connecting IP; anything earlier in the header could be forged by
#' the client itself (e.g. a request sent with its own
#' `X-Forwarded-For: 1.2.3.4` -- Caddy tacks the real IP on after it rather
#' than dropping the forged value). Reading the *first* value instead would
#' silently defeat IP-based lockout entirely.
#'
#' Only ever called from each app's own `run_app.R`, where `session` is
#' actually in scope (`check_credentials_db()` itself, called by
#' `shinymanager::secure_server()`, has no session access) -- lives here
#' anyway since both apps need it and it belongs alongside the lockout
#' logic it feeds.
#'
#' @param session The Shiny session (its `request` field is a Rook-style
#'   environment; same access pattern already used elsewhere in these apps
#'   for `QUERY_STRING`).
#' @return The client's IP as a string, or `NA_character_` if it can't be
#'   determined (e.g. `session$request` unexpectedly missing both
#'   headers) -- callers should treat that as "nothing to gate on", not an
#'   error.
#' @export
extract_client_ip <- function(session) {
  xff <- session$request$HTTP_X_FORWARDED_FOR
  if (is.null(xff) || !nzchar(xff)) {
    remote_addr <- session$request$REMOTE_ADDR
    return(if (is.null(remote_addr) || !nzchar(remote_addr)) NA_character_ else remote_addr)
  }
  hops <- trimws(strsplit(xff, ",", fixed = TRUE)[[1]])
  hops[length(hops)]
}
