#' Open a connection to the shared practices database
#'
#' Connection params come from env vars set in docker-compose.yml. The
#' password is read from a Docker secret file (DB_PASSWORD_FILE) when
#' present, falling back to a plain DB_PASSWORD env var for local dev.
#' @noRd
db_connect <- function() {
  password_file <- Sys.getenv("DB_PASSWORD_FILE", unset = NA)
  if (!is.na(password_file) && file.exists(password_file)) {
    password <- trimws(readLines(password_file, warn = FALSE))
  } else {
    password <- Sys.getenv("DB_PASSWORD", unset = "")
  }

  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST", "db"),
    port = as.integer(Sys.getenv("DB_PORT", "5432")),
    dbname = Sys.getenv("DB_NAME", "directcare"),
    user = Sys.getenv("DB_USER", "directcare"),
    password = password
  )
}

#' Authenticate a practice login against the `practices` table
#'
#' Passed as `check_credentials` to `shinymanager::secure_server()`. Beta:
#' `subscription_status` is not checked here -- all accounts stay 'trial'
#' and access is gated on a valid email/password pair only. Wire in a
#' status check once billing is added.
#' @noRd
check_credentials_db <- function(user, password) {
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  practice <- DBI::dbGetQuery(
    con,
    "SELECT id, practice_name, email, password_hash FROM practices WHERE email = $1",
    params = list(user)
  )

  if (nrow(practice) != 1 || !bcrypt::checkpw(password, practice$password_hash)) {
    return(list(result = FALSE))
  }

  list(
    result = TRUE,
    user_info = list(
      practice_id = practice$id,
      practice_name = practice$practice_name,
      email = practice$email
    )
  )
}
