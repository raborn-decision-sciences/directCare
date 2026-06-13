# ingest_csv_generic type='both' errors when col_type is NULL

    Code
      ingest_csv_generic(path = txn_sample, practice_id = 1, col_date = "date",
        col_amount = "amount", type = "both")
    Condition
      Error in `ingest_csv_generic()`:
      ! col_type must be specified when type = 'both'. Supply the name of the column in your CSV that identifies whether each row is an expense or income transaction (e.g. col_type = "type").

# ingest_csv_generic type='both' errors when col_type column does not exist

    Code
      ingest_csv_generic(path = txn_sample, practice_id = 1, col_date = "date",
        col_amount = "amount", col_type = "nonexistent_column")
    Condition
      Error in `ingest_csv_generic()`:
      ! The following columns were not found in sample_transactions.csv: nonexistent_column. Check that col_date, col_amount, and col_type match the actual column names.

# ingest_csv_generic errors on missing required col_date

    Code
      ingest_csv_generic(path = overhead_sample, practice_id = 1, col_date = "does_not_exist",
        col_amount = "amount", type = "overhead")
    Condition
      Error in `ingest_csv_generic()`:
      ! The following columns were not found in sample_overhead.csv: does_not_exist. Check that col_date, col_amount match the actual column names.

# ingest_csv_generic errors when dates cannot be parsed

    Code
      ingest_csv_generic(tmp, 1, "dt", "amt", type = "overhead")
    Condition
      Error in `ingest_csv_generic()`:
      ! 1 date(s) in column 'dt' could not be parsed. Supply date_format (e.g. date_format = "%d/%m/%Y") to specify the format explicitly.

