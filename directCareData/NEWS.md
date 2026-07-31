# directCareData 0.0.0.9000

* `direct_care_landscape` is now populated with 3,131 real practice
  locations sourced from the DPC Frontier mapper
  (<https://mapper.dpcfrontier.com/>), replacing the zero-row placeholder.
  `county_fips` is derived via a spatial join on the practice's
  latitude/longitude; `practice_name`, `estimated_panel_size`, and `city`
  remain `NA` for every row, since the source export doesn't provide them.

* Initial package scaffold: data-raw pipeline and dataset documentation for
  county-level market data (Census ACS, SAHIE, NPPES), geography
  crosswalks, and a placeholder direct care practice landscape table. No
  data has been generated yet — pending a Census API key to run the
  data-raw pipeline.
