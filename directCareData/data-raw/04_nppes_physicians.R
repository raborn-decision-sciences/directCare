# Downloads the current CMS NPPES "Full Replacement Monthly" bulk file,
# extracts physician counts by county, and discards the raw download.
#
# The file is several GB; only the columns needed to identify individual
# physicians and their practice ZIP are read, via chunked reads, to keep
# memory bounded. The raw download and its extracted CSV are deleted at
# the end of this script and must never be committed to the repo.
#
# NPPES file index: https://download.cms.gov/nppes/NPI_Files.html
# The exact filename changes monthly; set nppes_month/nppes_year below to
# match the file you intend to pull, or update the URL construction if
# CMS's naming convention has changed.

library(dplyr)

nppes_month <- format(Sys.Date() - 30, "%B")
nppes_year <- format(Sys.Date() - 30, "%Y")

nppes_zip_url <- sprintf(
  "https://download.cms.gov/nppes/NPPES_Data_Dissemination_%s_%s.zip",
  nppes_month,
  nppes_year
)

nppes_zip_path <- withr::local_tempfile(fileext = ".zip")
utils::download.file(nppes_zip_url, nppes_zip_path, mode = "wb")

nppes_extract_dir <- withr::local_tempdir()
nppes_csv_name <- grep(
  "^npidata_pfile_.*\\.csv$",
  utils::unzip(nppes_zip_path, list = TRUE)$Name,
  value = TRUE
)[1]
utils::unzip(nppes_zip_path, files = nppes_csv_name, exdir = nppes_extract_dir)
nppes_csv_path <- file.path(nppes_extract_dir, nppes_csv_name)

# NUCC taxonomy codes for allopathic & osteopathic physicians (MD/DO).
# This allow-list should be reviewed against the current NUCC taxonomy
# code set (https://www.nucc.org/index.php/code-sets-mainmenu-41/provider-taxonomy-mainmenu-40)
# before each refresh, since new taxonomy codes are added periodically.
physician_taxonomy_prefixes <- c("207", "208")

needed_cols <- c(
  "Entity Type Code",
  "Healthcare Provider Taxonomy Code_1",
  "Provider Business Practice Location Address Postal Code"
)

nppes_physicians <- readr::read_csv_chunked(
  nppes_csv_path,
  callback = readr::DataFrameCallback$new(function(chunk, pos) {
    chunk |>
      filter(
        `Entity Type Code` == "1",
        substr(`Healthcare Provider Taxonomy Code_1`, 1, 3) %in% physician_taxonomy_prefixes
      ) |>
      transmute(
        zip = substr(`Provider Business Practice Location Address Postal Code`, 1, 5)
      )
  }),
  col_select = dplyr::all_of(needed_cols),
  col_types = readr::cols(.default = "c")
)

nppes_county <- nppes_physicians |>
  inner_join(
    zip_county_crosswalk |> filter(is_primary),
    by = "zip"
  ) |>
  count(county_fips, name = "physician_count")

nppes_provenance <- list(
  vintage = paste(nppes_month, nppes_year),
  retrieved_on = Sys.Date(),
  source_url = "https://download.cms.gov/nppes/NPI_Files.html"
)

unlink(nppes_zip_path)
unlink(nppes_extract_dir, recursive = TRUE)
