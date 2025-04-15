# Packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(glue)
  # Note: As of 2025, you need to install a patch to get tigris to work. See
  # https://github.com/walkerke/tigris#:~:text=Is%20the%20package%20not%20working%20(April%202025)%3F
  library(tigris)
})

# Constants
data_dir <- file.path("data")
lodes_base_url <- "https://lehd.ces.census.gov/data/lodes/LODES8/"
tiger_year <- 2024
tracts_base_url <- glue("https://www2.census.gov/geo/tiger/TIGER{tiger_year}/TRACT/")

# Create data directory if it doesn't exist
dir.create(data_dir, showWarnings = F)

# **[LODES Documentation](https://lehd.ces.census.gov/doc/help/onthemap/LODESTechDoc.pdf)**
# - 3 data files. OD is origin-destination data, which is the one we need
# - File name: `[ST]_od_[PART]_[TYPE]_[YEAR].csv.gz`, where
#   - `[ST]`: the state code (e.g. `il` for Illinois)
#   - `[PART]`: `main` for residence and workplace in the state, or `aux` for
#     workplace in the state but residence outside the state
#   - `[TYPE]`: job type. `JT00` for All Jobs
#   - `[YEAR]`: 4-digit year. 2000-2022.
# - Columns in the data:
#   - `w_geocode`: workplace census block code. This is a 15-digit FIPS code. The
#     first 11 digits represent the tract.
#   - `h_geocode`: residence census block code
#   - `S000`: number of jobs
#   - `SXXX`: other related variables that could be useful later on

fetch_lodes_flows <- function(state, part = "main") {
  type <- "JT00"
  year <- 2022
  message(glue("Fetching LODES data for {toupper(state)}..."))
  # https://lehd.ces.census.gov/data/lodes/LODES8/il/od/il_od_aux_JT00_2022.csv.gz
  url <- glue("{lodes_base_url}{state}/od/{state}_od_{part}_{type}_{year}.csv.gz")
  local_path <- glue("{data_dir}/lodes_{state}.csv.gz")
  download.file(url, destfile = local_path)

  message("Downloaded data to ", local_path, "\n")
  local_path
}

# Residence area characteristics (RAC)
fetch_lodes_rac <- function(state) {
  # All jobs (vs by age etc.)
  segment <- "S000"
  # All jobs (vs public, private, federal, etc.)
  type <- "JT00"
  year <- 2022

  message(glue("Fetching LODES residence area characteristics for {toupper(state)}..."))
  url <- glue("{lodes_base_url}{state}/rac/{state}_rac_{segment}_{type}_{year}.csv.gz")
  local_path <- glue("{data_dir}/lodes_{state}_rac.csv.gz")
  download.file(url, destfile = local_path)

  message("Downloaded data to ", local_path, "\n")
  local_path
}

fetch_tract_shapes <- function() {
  state <- "il"
  message(glue("Fetching tract shapes for state {toupper(state)}..."))
  # cb = FALSE is higher quality; cb = TRUE is faster
  result <- tigris::tracts(state, cb = TRUE)
  dest_file <- file.path(data_dir, "tracts_original.geojson")
  st_write(result, dest_file, append = FALSE, delete_dsn = TRUE)
  message("Downloaded tract shapes using tigris to ", dest_file, "\n")
  dest_file
}

# This fetches a sketchy cca-tract relationship file that I don't really trust
# fetch_cca_relationships <- function() {
#   message("Fetching Chicago Community Area (CCA) to tract relationships...")
#   dest_file <- file.path(data_dir, "cca_relationships.xlsx")
#   download.file(
#     "https://guides.lib.uchicago.edu/ld.php?content_id=59026582",
#     destfile = dest_file,
#   )
#   message("Downloaded CCA-tract relationships to ", dest_file, "\n")
#   dest_file
# }

fetch_cca_shapes <- function() {
  message("Fetching Chicago Community Area (CCA) shapes...")
  # This is geospatial data but the file is a CSV for some reason
  dest_file <- file.path(data_dir, "cca_shapes_original.csv")
  download.file(
    # See https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6
    "https://data.cityofchicago.org/api/views/igwz-8jzy/rows.csv",
    destfile = dest_file,
  )
  message("Downloaded CCA shapes to ", dest_file, "\n")
  dest_file
}

# Uncomment whichever of these you need to run
# fetch_lodes_flows("il")
fetch_lodes_rac("il")
# fetch_tract_shapes()
# fetch_cca_shapes()
