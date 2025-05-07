suppressPackageStartupMessages({
  library(tidyverse) # Standard stuff
  library(glue) # String concatenation
  library(sf) # Geospatial data
  # Note: As of 2025, you need to install a patch to get tigris to work. See
  # https://github.com/walkerke/tigris#:~:text=Is%20the%20package%20not%20working%20(April%202025)%3F
  library(tigris)
})

# Constants
lodes_base_url <- "https://lehd.ces.census.gov/data/lodes/LODES8/"
tiger_year <- 2024
tracts_base_url <- glue("https://www2.census.gov/geo/tiger/TIGER{tiger_year}/TRACT/")
state <- "il"
# NAD83, the CRS used by Tigris. We use this when we create the CCAs sf.
crs <- 4269
# Paths
data_dir <- file.path("data")
lodes_flows_path <- file.path(data_dir, glue("lodes_{state}.csv.gz"))
tract_flows_path <- file.path(data_dir, "tract_flows.csv")
lodes_rac_path <- file.path(data_dir, glue("lodes_{state}_rac.csv.gz"))
tracts_path <- file.path(data_dir, "tracts.geojson")
cca_shapes_path <- file.path(data_dir, "cca_shapes_original.csv")
ccas_path <- file.path(data_dir, "ccas.geojson")
cca_flows_path <- file.path(data_dir, "cca_flows.csv")

# Create data directory if it doesn't exist
dir.create(data_dir, showWarnings = FALSE)

# Fetch LODES block-to-block flows in Illinois
if (file.exists(lodes_flows_path)) {
  message(glue("{lodes_flows_path} already exists, so skipping download."))
} else {
  part <- "main"
  type <- "JT00"
  year <- 2022

  message(glue("Fetching LODES data for {toupper(state)}..."))
  url <- glue("{lodes_base_url}{state}/od/{state}_od_{part}_{type}_{year}.csv.gz")
  download.file(url, destfile = lodes_flows_path)
  message(glue("Downloaded data to {lodes_flows_path}"))
}

# Aggregate from census blocks to tracts
if (file.exists(tract_flows_path)) {
  message(glue("{tract_flows_path} already exists, so leaving it as-is."))
} else {
  message("Aggregating commuting flows to the census tract level...")

  # This has about 5 million rows FYI
  original <- read_csv(
    lodes_flows_path,
    # Only load the columns we really need
    col_select = c("w_geocode", "h_geocode", "S000"),
    col_types = "cci"
  )

  # This has about 1 million rows (only hehe)
  tract_flows <- original %>%
    mutate(
      h_tract = substr(h_geocode, 1, 11),
      w_tract = substr(w_geocode, 1, 11)
    ) %>%
    group_by(w_tract, h_tract) %>%
    summarize(n = sum(S000), .groups = "drop")
  write_csv(tract_flows, tract_flows_path)
  message(glue("Saved {tract_flows_path}"))
}

tract_flows <- read_csv(tract_flows_path, col_types = "cci")

# Fetch LODES residence area characteristics (RAC) data
if (file.exists(lodes_rac_path)) {
  message(glue("{lodes_rac_path} already exists, so skipping download."))
} else {
  # All jobs (vs by age etc.)
  segment <- "S000"
  # All jobs (vs public, private, federal, etc.)
  type <- "JT00"
  year <- 2022

  message(glue("Fetching LODES residence area characteristics for {toupper(state)}..."))
  url <- glue("{lodes_base_url}{state}/rac/{state}_rac_{segment}_{type}_{year}.csv.gz")
  download.file(url, destfile = lodes_rac_path)
  message(glue("Downloaded data to {lodes_rac_path}"))
}

if (file.exists(ccas_path)) {
  message(glue("{ccas_path} already exists, so skipping download."))
} else {
  message("Fetching Chicago Community Area (CCA) shapes...")
  # This is geospatial data but the file is a CSV for some reason
  tmp_file <- tempfile(fileext = ".csv")
  download.file(
    # See https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6
    "https://data.cityofchicago.org/api/views/igwz-8jzy/rows.csv",
    destfile = tmp_file,
  )

  message(glue("Downloaded CCA shapes. Doing a little processing..."))
  ccas <- read_csv(tmp_file, show_col_types = FALSE)
  unlink(tmp_file)
  # Mutation and saving
  ccas %>%
    mutate(
      .keep = "none",
      # Change from all upper case to title case
      name = str_to_title(COMMUNITY),
      # It's AREA_NUMBER. Not a typo.
      num = as.integer(AREA_NUMBE),
      geometry = the_geom,
    ) %>%
    st_as_sf(wkt = "geometry") %>%
    # Set CRS to match the tracts
    st_set_crs(crs) %>%
    write_sf(ccas_path, delete_dsn = TRUE)
  message(glue("Saved community area shapes to {ccas_path}"))
}

ccas <- read_sf(ccas_path)

if (file.exists(tracts_path)) {
  message(glue("{tracts_path} already exists, so skipping download."))
} else {
  message(glue("Fetching tract shapes for state {toupper(state)}..."))
  # cb = FALSE is higher quality; cb = TRUE is faster
  tract_shapes <- tigris::tracts(state, cb = TRUE) %>%
    select(GEOID, geometry)

  message("Downloaded tract shapes. Joining tract shapes with the LODES data...")
  # Get node properties by tract
  tracts <- tract_flows %>%
    mutate(commuting = w_tract != h_tract) %>%
    group_by(h_tract, commuting) %>%
    # Rename to match the shapes
    rename(GEOID = h_tract) %>%
    summarize(n = sum(n), .groups = "drop_last") %>%
    mutate(
      .keep = "unused",
      new_col_name = if_else(commuting, "commuting_n", "not_commuting_n")
    ) %>%
    pivot_wider(names_from = new_col_name, values_from = n, values_fill = 0) %>%
    mutate(
      # Total number of workers who live in this tract
      total = not_commuting_n + commuting_n,
      pct_commuting = commuting_n / total * 100,
    ) %>%
    # Join with the shapes to get an sf
    full_join(tract_shapes, by = "GEOID") %>%
    st_as_sf()

  write_sf(tracts, tracts_path, delete_dsn = TRUE)
  message(glue("Saved {tracts_path}"))
}

tracts <- read_sf(tracts_path)

if ("cca" %in% colnames(tracts)) {
  message("tracts.geojson already contains CCA column, so skipping CCA assignment.")

  # We still need to define tract_cca_relationships. We can do this using tracts.
  tract_cca_relationships <- tracts %>%
    filter(!is.na(cca)) %>%
    as_tibble() %>%
    mutate(.keep = "none", tract = GEOID, cca, cca_num)
} else {
  message("Determining relationships between tracts and community areas...")
  # This is done by finding the CCA containing the tract's centroid.
  tract_centroids <- suppressWarnings(tracts %>% st_centroid())
  tracts_with_cca <- tract_centroids %>%
    # left = FALSE causes it to drop tracts outside any CCA
    st_join(ccas, join = st_within, left = FALSE)

  # Double check that no tracts matched multiple CCAs
  dupes <- tracts_with_cca %>%
    count(GEOID) %>%
    filter(n > 1)
  if (nrow(dupes) > 0) {
    warning(glue("Some tracts matched multiple community areas: {paste(dupes$GEOID, collapse=', ')}"))
  }

  # A simple data frame relating tracts to CCAs
  tract_cca_relationships <- tracts_with_cca %>%
    as_tibble() %>%
    # Drops the geometry column
    select(GEOID, name, num) %>%
    rename(tract = GEOID, cca = name, cca_num = num)
  tracts <- tracts %>%
    # Join the CCA names and numbers. These will be NA for tracts outside of Chicago.
    left_join(tract_cca_relationships, by = join_by(GEOID == tract))
  message("Found the CCA for every tract")
  # Resave the tracts geojson
  write_sf(tracts, tracts_path, delete_dsn = TRUE)
  message(glue("Saved {tracts_path}, now with a CCA column."))
}

# Aggregate from census tracts to CCAs
if (file.exists(cca_flows_path)) {
  message(glue("{cca_flows_path} already exists, so leaving it as-is."))
} else {
  message("Aggregating commuting flows to the community area level...")
  # Next, we're going to aggregate the flows to the community area level, just
  # like we did when we aggregated blocks to tracts.
  cca_flows <- tract_flows %>%
    left_join(
      tract_cca_relationships,
      by = join_by(h_tract == tract)
    ) %>%
    # `from` is where people live; `to` is where they work
    rename(from = cca) %>%
    left_join(
      tract_cca_relationships,
      by = join_by(w_tract == tract)
    ) %>%
    rename(to = cca) %>%
    mutate(
      across(c(from, to), ~ if_else(is.na(.x), "OUTSIDE CHICAGO", .x)),
    ) %>%
    group_by(from, to) %>%
    summarize(
      n = sum(n),
      .groups = "drop"
    )

  # Complete the data frame with 0s so that every dyad is present, including
  # those with 0 commuting.
  cca_flows <- cca_flows %>%
    complete(
      from,
      to,
      fill = list(n = 0)
    ) %>%
    arrange(from, to)

  message("Calculating pairwise distances between all CCAs...")
  # Get the distance between centroids of every pair of community areas
  cca_centroids <- suppressWarnings(ccas %>% st_centroid()) %>%
    select(name, num, geometry)
  cca_flows <- cca_flows %>%
    left_join(
      cca_centroids %>% select(name, geometry),
      by = join_by(from == name)
    ) %>%
    rename(from_centroid = geometry) %>%
    left_join(
      cca_centroids %>% select(name, geometry),
      by = join_by(to == name)
    ) %>%
    rename(to_centroid = geometry) %>%
    mutate(
      distance = st_distance(from_centroid, to_centroid, by_element = TRUE, which = "Great Circle"),
    ) %>%
    select(-from_centroid, -to_centroid)

  write_csv(cca_flows, cca_flows_path)
  message("Saved cca_flows.geojson")
}

cca_flows <- read_csv(cca_flows_path, col_types = "ccid")

if (!("distance_from_loop" %in% colnames(ccas))) {
  distances_from_loop <- cca_flows %>%
    filter(from == "Loop") %>%
    select(to, distance) %>%
    rename(name = to, distance_from_loop = distance)
  ccas <- ccas %>%
    left_join(distances_from_loop, by = "name")
  write_sf(ccas, ccas_path, delete_dsn = TRUE)
  message("Added distance_from_loop column to the CCAs data frame")
}

message("Ready!")
