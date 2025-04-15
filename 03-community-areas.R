suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(readxl)
  library(glue)
  library(spatstat) # For weighted.median
})

data_dir <- file.path("data")

# 1. LOAD THE DATA

# These files were created by 01-fetch.R and 02-tracts.R
tract_flows <- read_csv(file.path(data_dir, "tract_flows.csv"), col_types = "cci")
tracts <- read_sf(file.path(data_dir, "tracts.geojson"))
ccas <- read_csv(file.path(data_dir, "cca_shapes_original.csv"), show_col_types = FALSE)

# 2. PROCESSING AND TRANSFORMATION

ccas <- ccas %>%
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
  st_set_crs(st_crs(tracts))
message("Loaded community area shapes into an sf object")

if ("cca" %in% colnames(tracts)) {
  message("tracts.geojson already contains CCA column. Skipping CCA assignment.")

  # We still need to define tract_cca_relationships. We can do this using tracts.
  tract_cca_relationships <- tracts %>%
    filter(!is.na(cca)) %>%
    as_tibble() %>%
    mutate(.keep = "none", tract = GEOID, cca, cca_num)
} else {
  tracts <- tracts %>% select(-cca, -cca_num)

  # Determine which tracts belong to which community areas.
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
  message("Determined the CCA for each tract")
}

# For debugging, this code will plot the tracts on a map colored by CCA
# library(tmap)
# right_join(tracts, tract_cca_relationships, by = join_by("GEOID" == "tract")) %>%
#   tm_shape() +
#   tm_polygons(fill = "cca", fill.scale = tm_scale_categorical(n.max = 100))

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
message("Computed CCA flows")

# Complete the data frame with 0s so that every dyad is present, including
# those with 0 commuting.
cca_flows <- cca_flows %>%
  complete(
    from,
    to,
    fill = list(n = 0)
  ) %>%
  arrange(from, to)
message("Added rows for all possible CCA-to-CCA flows")

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
message("Calculated distances between community area centroids")

distances_from_loop <- cca_flows %>%
  filter(from == "Loop") %>%
  select(to, distance) %>%
  rename(name = to, distance_from_loop = distance)
# ccas <- ccas %>% select(-distance_from_loop)
if (!("distance_from_loop" %in% colnames(ccas))) {
  ccas <- ccas %>%
    left_join(distances_from_loop, by = "name")
  message("Added distance_from_loop column to the CCAs data frame")
}

# Calculate some other useful columns for the CCAs data frame

ccas <- ccas %>%
  bind_rows(
    tibble(
      name = "OUTSIDE CHICAGO",
      geometry = st_sfc(st_polygon(), crs = st_crs(ccas))
    )
  )

# Helpful for reloading the data when debugging
# ccas <- read_sf(file.path(data_dir, "ccas.geojson"))

# Number of workers who live in this CCA and work anywhere in Illinois
ccas <- cca_flows %>%
  group_by(from) %>%
  rename(name = from) %>%
  summarize(
    h_total = sum(n),
    .groups = "drop"
  ) %>%
  right_join(ccas, by = "name")

# These properties cannot be calculated for the OUTSIDE CHICAGO rows (in or out)
ccas <- cca_flows %>%
  filter(to != "OUTSIDE CHICAGO", from != "OUTSIDE CHICAGO") %>%
  group_by(from) %>%
  rename(name = from) %>%
  summarize(
    # Total workers who live in this CCA and work in Chicago
    h_in_chicago = sum(n),
    # Average distance workers who live here have to commute (in Chicago)
    h_avg_distance = weighted.mean(distance, n),
    # Median distance workers who live here have to commute (in Chicago)
    h_median_distance = weighted.median(distance, n),
    .groups = "drop"
  ) %>%
  right_join(ccas, by = "name")

# Number of workers who live and work in the same CCA (i.e., don't commute
# outside their community area)
ccas <- cca_flows %>%
  filter(to == from) %>%
  mutate(.keep = "none", name = from, work_in_same = n) %>%
  right_join(ccas, by = "name")

# Total number of people who work in this CCA and live anywhere in Illinois
ccas <- cca_flows %>%
  group_by(to) %>%
  rename(name = to) %>%
  summarize(
    w_total = sum(n),
    .groups = "drop"
  ) %>%
  right_join(ccas, by = "name")

# Number of people who work in this CCA and live in Chicago
ccas <- cca_flows %>%
  filter(from != "OUTSIDE CHICAGO", to != "OUTSIDE CHICAGO") %>%
  group_by(to) %>%
  rename(name = to) %>%
  summarize(
    w_from_chicago = sum(n),
    .groups = "drop"
  ) %>%
  right_join(ccas, by = "name")

message("Added aggregated commuting data columns to the CCAs data frame")

# 3. SAVE THE DATA

write_csv(
  tract_cca_relationships,
  file.path(data_dir, "tract_cca_relationships.csv"),
)
message("Saved tract_cca_relationships.csv")

write_sf(
  ccas,
  file.path(data_dir, "ccas.geojson"),
  # Overwrite existing file
  delete_dsn = TRUE,
)
message("Saved ccas.geojson")

write_csv(
  cca_flows,
  file.path(data_dir, "cca_flows.csv")
)
message("Saved cca_flows.geojson")

write_sf(
  tracts,
  file.path(data_dir, "tracts.geojson"),
  # Overwrite existing file
  delete_dsn = TRUE,
)
message("Saved tracts.geojson, now with CCA column")
