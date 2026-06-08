
ingest_manual <- function(df, practice_id, type = c("overhead", "income")) {
  type <- match.arg(type)
  if (type == "overhead") normalize_overhead_manual(df, practice_id, source = "manual")
  else normalize_income_manual(df, practice_id, source = "manual")
}
