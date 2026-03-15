# Setup -----
library("sf")
library("dplyr")
library("countrycode")
library("rnaturalearth")

source("/data/brazil_mining/11_stream.R")

max_order <- 7 # how far do we want to look up and down

# Load and prepare mines -----
#mines with centroids lat and lon
mines <- readRDS("/data/brazil/mining_mapbiomas/mining_area_csv/mb_mines_2023.csv")|> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Load and prepare the HydroBASINS data -----

# Make sure this is in the right place:
# https://data.hydrosheds.org/file/hydrobasins/standard/hybas_sa_lev01-12_v1c.zip
lvl <- "07" # Basin level, not sure which one to use

s <- st_read(paste0("/data/brazil_health/HYBAS/hybas_lake_sa_lev", lvl, "_v1c.shp")) |> 
  st_make_valid() |>
  # Overwrite (artifical) long-distance links for sinks
  mutate(NEXT_DOWN = ifelse(DIST_SINK == 0, 0, NEXT_DOWN))

# Lakes are not relevant and break streams
# We need the IDs to filter out lakes from the basin-chains that we create below
id_lakes <- s |> st_drop_geometry() |> filter(LAKE != 0) |> pull(HYBAS_ID)
s <- s |> filter(LAKE == 0)

# Cache a lightweight version
d <- s |> st_drop_geometry() |> select(HYBAS_ID, NEXT_DOWN)

# Create the intersection of basins and mines
intersect <- st_intersects(s, mines)

# Add mine IDs to the basins that contain mines
id_treateds <- s[["HYBAS_ID"]][lengths(intersect) > 0]
# Mines in a given basin
id_mines <- intersect[lengths(intersect) > 0]
names(id_mines) <- s[["HYBAS_ID"]][lengths(intersect) > 0]

# After you create id_treateds and id_mines:
mining_summary <- lapply(names(id_mines), function(basin_id) {
  mine_indices <- id_mines[[basin_id]]
  mines_in_basin <- mines[mine_indices, ]
  
  data.frame(
    basin_id = as.numeric(basin_id),
    mining_area_ha = sum(mines_in_basin$mining_area_ha, na.rm = TRUE),
    mining_class = paste(unique(mines_in_basin$mining_class), collapse = "; ")
  )
}) |> bind_rows()

# ... all your upstream/downstream calculations here ...

# Get down- / upstream basins ---
# stream_ordered from R/11_stream.R traverses up-/downstream and returns IDs + order
upstream_order <- lapply(id_treateds, stream_ordered,
                         max = max_order, down = FALSE, d = d)
downstream_order <- lapply(id_treateds, stream_ordered,
                           max = max_order, down = TRUE, d = d)
# Add names
names(upstream_order) <- id_treateds
names(downstream_order) <- id_treateds
# Vectorize the status IDs
upstream_ids <- do.call(c, sapply(upstream_order, \(df) df[, "id_next"]))
downstream_ids <- do.call(c, sapply(downstream_order, \(df) df[, "id_next"]))

# We only filtered lakes themselves -- they can still show up here as downstream, so we filter
downstream_order <- lapply(downstream_order, \(x) x[which(!x[, 1] %in% id_lakes), ])

# Add the treatment status to the basins
s_relevant <- s |>
  mutate(status = case_when(
    HYBAS_ID %in% id_treateds ~ "mine",
    HYBAS_ID %in% downstream_ids ~ "downstream",
    HYBAS_ID %in% upstream_ids ~ "upstream",
    TRUE ~ NA_character_
  )) |> filter(!is.na(status))

print("Treatment status of basins:")
s_relevant |> pull(status) |> table() |> print()

# Calculate distances -----

# Prepare distances between adjacent basins ---
# Centroid distances
basin_centroids <- s_relevant |> st_centroid() |>
  select(HYBAS_ID, NEXT_DOWN, DIST_SINK)
distances_centroids <- basin_centroids |>
  st_drop_geometry() |> # Remove geom for faster join
  left_join(basin_centroids, by = join_by("NEXT_DOWN" == "HYBAS_ID")) |>
  mutate(geometry.x = basin_centroids[["geometry"]]) |> # Add both geoms for distance
  rename(geometry.y = geometry) |>
  mutate(distance = ifelse(NEXT_DOWN == 0, 0, # Distance in km
                           as.numeric(st_distance(st_sfc(geometry.x), st_sfc(geometry.y), by_element = TRUE)) / 1e3
  )) |>
  transmute(HYBAS_ID, NEXT_DOWN, dist_km_centroid = distance)

# River flow distances
distances_river <- s_relevant |>
  st_drop_geometry() |>
  select(HYBAS_ID, NEXT_DOWN, NEXT_SINK, DIST_SINK, DIST_MAIN) %>% # Cursed pipe
  left_join(., ., by = join_by("NEXT_DOWN" == "HYBAS_ID")) |>
  mutate(distance = ifelse(NEXT_DOWN == 0, DIST_SINK.x, DIST_SINK.x - DIST_SINK.y)) |>
  transmute(HYBAS_ID, NEXT_DOWN, dist_km = distance)

# Build cumulative distances ---

# Downstream -- add the mine basin at order 0
ds_dists <- lapply(seq_along(downstream_order), \(i) {
  cbind("id_next" = as.numeric(names(downstream_order)[i]), n = 0L) |>
    rbind(downstream_order[[i]]) |>
    as.matrix()
})
ds_dists <- lapply(ds_dists, \(ds_dist) {
  out <- data.frame(
    basin_id = ds_dist[, 1L], # id_next
    mine_basin = ds_dist[, 1L][1L], # id of the mine basin
    dist_n = ds_dist[, 2L], # n, i.e., order
    dist_km = 0, dist_km_centroid = 0
  )
  # We're done if there's just the mine basin
  if(nrow(ds_dist) == 1) {return(out)}
  # Otherwise, we compute and add cumulative distances
  dists <- data.frame(
    HYBAS_ID = ds_dist[seq(1, nrow(ds_dist) - 1), "id_next"],
    NEXT_DOWN = ds_dist[seq(2, nrow(ds_dist)), "id_next"]
  ) |> # Now we can merge in the distances by ID
    left_join(distances_river, by = c("HYBAS_ID", "NEXT_DOWN")) |>
    left_join(distances_centroids, by = c("HYBAS_ID", "NEXT_DOWN")) |>
    transmute( # We keep NEXT_DOWN, as we want the distance to that
      basin_id = NEXT_DOWN, dist_km = cumsum(dist_km), dist_km_centroid = cumsum(dist_km_centroid)
    )
  out |> rows_update(dists, by = "basin_id")
})

# Upstream -- add the mine basin and info on the next basin
us_dists <- lapply(seq_along(upstream_order), \(i) {
  cbind("id_next" = as.numeric(names(upstream_order)[i]), n = 0L) |>
    rbind(upstream_order[[i]]) |>
    as.data.frame() |>
    left_join(distances_river |> select(HYBAS_ID, NEXT_DOWN), by = c("id_next" = "HYBAS_ID")) |>
    mutate(NEXT_DOWN = ifelse(n == 0, NA, NEXT_DOWN)) # We stop at the mine itself
})
# Function to accumulate a distance following NEXT_DOWN
accumulate_dists <- function(dists, name = "dist_km") {
  accumulated <- dists[[name]]
  for (i in seq_len(NROW(dists))) {
    accumulated[i] <- accumulated[i] + # Sum to get rid of numeric(0L)
      sum(accumulated[ dists[["HYBAS_ID"]] == dists[["NEXT_DOWN"]][i] ], na.rm = TRUE)
  }
  accumulated
}
us_dists <- lapply(us_dists, \(us_dist) {
  out <- data.frame(
    basin_id = us_dist[, 1L], # id_next
    mine_basin = us_dist[, 1L][1L], # id of the mine basin
    dist_n = us_dist[, 2L], # n, i.e., order
    dist_km = 0, dist_km_centroid = 0
  )
  # We're done if there's just the mine basin
  if(nrow(us_dist) == 1) {return(out)}
  # Otherwise, we compute and add cumulative distances
  dists <- data.frame(
    HYBAS_ID = us_dist[seq(2, nrow(us_dist)), "id_next"],
    NEXT_DOWN = us_dist[seq(2, nrow(us_dist)), "NEXT_DOWN"]
  ) |> # Now we can merge in the distances by ID
    left_join(distances_river, by = c("HYBAS_ID", "NEXT_DOWN")) |>
    left_join(distances_centroids, by = c("HYBAS_ID", "NEXT_DOWN"))
  dists <- dists |> mutate(
    dist_km = accumulate_dists(dists, "dist_km"),
    dist_km_centroid = accumulate_dists(dists, "dist_km_centroid")
  )
  out |> rows_update(
    dists |> select(basin_id = HYBAS_ID, dist_km, dist_km_centroid),
    by = "basin_id"
  )
})

# Merge upstream, downstream and mines ---
basins_ordered <- rbind(
  bind_rows(ds_dists) |> mutate(status = "downstream"),
  bind_rows(us_dists) |> mutate(status = "upstream"),
  make.row.names = FALSE
) |> # Add mine status and -/+ dist_order
  mutate(
    status = ifelse(basin_id == mine_basin, "mine", status),
    dist_order = dist_n * ifelse(status == "upstream", -1, 1)
  )
# Basins can still appear as mine, treated, and control -- we want one
basins_ordered_unique <- basins_ordered |>
  group_by(basin_id) |>
  arrange( # We keep basin information in the order of
    status != "mine", # mines trump all
    dist_n, # closer order
    status != "downstream", # downstream (treated) > upstream (control)
    dist_km, # closer > farther
    .by_group = TRUE
  ) |>
  slice_head(n = 1) # We only keep the first, following the above order

# Final join to create the full basin dataset
basins_l12_2000 <- basins_ordered_unique |>
  left_join(s, by = join_by("basin_id" == "HYBAS_ID")) |>
  left_join(mining_summary, by = join_by("basin_id"))  # join by mine_basin here

# Create a lookup table with mining info from mine basins
mine_info <- basins_l12_2000 %>%
  filter(status == "mine") %>%
  select(basin_id, mining_area_ha, mining_class) %>%
  rename(mine_basin = basin_id,
         mine_mining_area_ha = mining_area_ha,
         mine_mining_class = mining_class)

# Join mining info to downstream basins based on mine_basin
basins_l12_2000_updated <- basins_l12_2000 %>%
  left_join(mine_info, by = "mine_basin") %>%
  mutate(
    mining_area_ha = ifelse(status %in% c("downstream", "upstream"), mine_mining_area_ha, mining_area_ha),
    mining_class = ifelse(status %in% c("downstream", "upstream"), mine_mining_class, mining_class)
  ) %>%
  select(-mine_mining_area_ha, -mine_mining_class)

write_sf(basins_l12_2000_updated, "/data/brazil_mining/mine_basins_lev07/basins_l07_2023.shp")
# write_sf(so, gsub("gpkg$", "shp", file))

write_sf(basins_l12_2000_updated, "/data/brazil_mining/mine_basins_lev07/basins_l07_2023.gpkg")
# write_sf(so, gsub("gpkg$", "shp", file))