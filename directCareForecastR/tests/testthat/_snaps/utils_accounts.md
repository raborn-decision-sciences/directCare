# map_accounts warns on unmatched accounts

    Code
      map_accounts(make_raw_tbl("Completely Unknown Account"), default_account_map())
    Condition
      Warning:
      The following accounts could not be mapped and were assigned 'other': Completely Unknown Account
    Output
      # A tibble: 1 x 4
        account                    amount date       category
        <chr>                       <dbl> <chr>      <chr>   
      1 Completely Unknown Account   1000 01/15/2025 other   

