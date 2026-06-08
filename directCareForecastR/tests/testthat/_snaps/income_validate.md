# validate_income errors on missing required columns

    Code
      validate_income(bad)
    Condition
      Error in `validate_income()`:
      ! Income tibble is missing required columns: revenue

# validate_income errors on NA dates

    Code
      validate_income(bad)
    Condition
      Error in `validate_income()`:
      ! 1 row(s) have unparseable dates. Check the source file.

# validate_income errors on NA revenue

    Code
      validate_income(bad)
    Condition
      Error in `validate_income()`:
      ! 1 row(s) have missing revenue values. Check the source file.

# validate_income warns on negative revenue

    Code
      validate_income(income)
    Condition
      Warning:
      1 negative revenue row(s) detected and flagged as cancellations/chargebacks. These will reduce revenue totals in the affected period(s).

# validate_income warns on zero revenue

    Code
      validate_income(income)
    Condition
      Warning:
      1 zero-revenue row(s) detected. These will be ignored in summaries.

# validate_income warns on future dates

    Code
      validate_income(income)
    Condition
      Warning:
      1 row(s) have future dates. Verify these are not data entry errors.

# ingest_manual type = 'income' warns on negative revenue

    Code
      ingest_manual(df, practice_id = 1, type = "income")
    Condition
      Warning:
      1 negative revenue row(s) detected and flagged as cancellations/chargebacks. These will reduce revenue totals in the affected period(s).

