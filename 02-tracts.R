suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
})

data_dir <- file.path("data")

# This has about 5 million rows FYI
original <- read_csv(
  file.path(data_dir, "lodes_il.csv.gz"),
  # Only load the columns we really need
  col_select = c("w_geocode", "h_geocode", "S000"),
  col_types = "cci"
)

# Aggregate from census blocks to tracts
# This has about 1 million rows (only hehe)
tract_flows <- original %>%
  mutate(
    h_tract = substr(h_geocode, 1, 11),
    w_tract = substr(w_geocode, 1, 11)
  ) %>%
  group_by(w_tract, h_tract) %>%
  summarize(n = sum(S000), .groups = "drop")
write_csv(tract_flows, file.path(data_dir, "tract_flows.csv"))
message("Saved tract_flows.csv")

# We start with just the geographic data of tracts, then join in the network data
tract_shapes <- read_sf(file.path(data_dir, "tracts_original.geojson")) %>%
  select(GEOID, geometry)

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
  # Join with the shapes
  full_join(tract_shapes, by = "GEOID") %>%
  st_as_sf()

write_sf(
  tracts,
  file.path(data_dir, "tracts.geojson"),
  # Overwrite existing file
  delete_dsn = TRUE,
)
message("Saved tracts.geojson")
