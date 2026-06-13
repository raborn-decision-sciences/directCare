# validate_overhead errors on missing required columns

    Code
      validate_overhead(bad)
    Condition
      Error in `validate_overhead()`:
      ! Overhead tibble is missing required columns: amount

# validate_overhead errors on NA dates

    Code
      validate_overhead(bad)
    Condition
      Error in `validate_overhead()`:
      ! 1 rows have unparseable dates. Check the source file.

# validate_overhead errors on NA amounts

    Code
      validate_overhead(bad)
    Condition
      Error in `validate_overhead()`:
      ! 1 rows have missing amounts. Check the source file.

# validate_overhead warns on negative amounts

    Code
      validate_overhead(overhead)
    Condition
      Warning:
      1 negative amount(s) detected and flagged as refunds. These will reduce overhead totals in the affected month(s).

# validate_overhead warns on zero amounts

    Code
      validate_overhead(overhead)
    Condition
      Warning:
      1 zero-amount row(s) detected. These will be ignored in summaries.

# validate_overhead warns on future dates

    Code
      validate_overhead(overhead)
    Condition
      Warning:
      1 row(s) have future dates. Verify these are not data entry errors.

