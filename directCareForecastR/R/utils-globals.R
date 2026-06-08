# Suppress R CMD check NOTEs for bare column names used in dplyr verbs and
# the rlang .data pronoun. These are not true global variables — they are
# evaluated inside data-masking contexts where the column names are in scope.
utils::globalVariables(c(
  ".data",
  "account_name",
  "amount",
  "category",
  "description",
  "full_account_name",
  "is_refund",
  "month",
  "overhead",
  "period_start",
  "practice_id",
  "revenue",
  "total_overhead",
  "week_start",
  "year"
))
