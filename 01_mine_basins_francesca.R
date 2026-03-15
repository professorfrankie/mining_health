# Setup -----
library("sf")
library("dplyr")
library("countrycode")
library("rnaturalearth")
library("tidyr")

source("/home/francesca/brazil_mining/mining_health/11_stream.R")

max_order <- 7 # how far do we want to look up and down

# Load and prepare mines -----
#mines with centroids lat and lon
mines <- readRDS("/data/brazil/mining_mapbiomas/mining_area_csv/mb_mines_2023.csv")|> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

#include mining type and substances
#create mine_type variable
substances_all <- tribble(
  ~category, ~id_range, ~substance,
  "Artisanal", 202:213, "artisanal_metallic_other",
  "Artisanal", 214,     "artisanal_metallic_tin",
  "Artisanal", 215,     "artisanal_metallic_gold",
  "Artisanal", 217:222, "artisanal_non-metallic",
  "Artisanal", 224:225, "artisanal_precious_stones",
  "Artisanal", 227:229, "artisanal_non_identified",
  
  "Industrial", 102:113, "industrial_metallic_other",
  "Industrial", 114,     "industrial_metallic_tin",
  "Industrial", 115,     "industrial_metallic_gold",
  "Industrial", 117:122, "industrial_non-metallic",
  "Industrial", 124:125, "industrial_non_identified",
  "Industrial", 127:129, "industrial_energetic",
  
  "Other", 302:329, "mining_OTHER"
)
substances_expanded <- substances_all %>%
  mutate(id_range = purrr::map(id_range, ~ as.integer(.x))) %>%
  unnest(id_range) %>%
  rename(
    mining_class = id_range,
    mining_type = substance
  )
mines <- mines %>%
  left_join(substances_expanded, by = "mining_class")

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
    mining_ha_total = sum(mines_in_basin$mining_area_ha, na.rm = TRUE),
    mining_ha_artisanal = sum(
      mines_in_basin$mining_area_ha[mines_in_basin$category == "Artisanal"],
      na.rm = TRUE),
    mining_ha_industrial = sum(
      mines_in_basin$mining_area_ha[mines_in_basin$category == "Industrial"],
      na.rm = TRUE),
    mining_ha_gold = sum(
      mines_in_basin$mining_area_ha[grepl("gold", mines_in_basin$mining_type)],
      na.rm = TRUE),
    mining_class = paste(unique(mines_in_basin$mining_class), collapse = "; ")
  )
}) |> bind_rows() %>%
  mutate(
    mining_ha_artisanal_share = mining_ha_artisanal/mining_ha_total,
    mining_ha_industrial_share = mining_ha_industrial/mining_ha_total,
    mining_ha_gold_share = mining_ha_gold/mining_ha_total
  )
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

### adding cumulative mining
df <- basins_l12_2000 %>%
  filter(status == "mine") %>%
  select(basin_id, NEXT_DOWN, mining_ha_total,
         mining_ha_artisanal, mining_ha_industrial, mining_ha_gold)

df$basin_id <- as.character(df$basin_id)
df$NEXT_DOWN <- as.character(df$NEXT_DOWN)


# Create a named vector for mining areas
mining_total <- setNames(df$mining_ha_total, df$basin_id)
mining_artisanal <- setNames(df$mining_ha_artisanal, df$basin_id)
mining_industrial <- setNames(df$mining_ha_industrial, df$basin_id)
mining_gold <- setNames(df$mining_ha_gold, df$basin_id)


# Create a list of which basins flow into each basin
upstream_list <- lapply(df$basin_id, function(b) {
  df$basin_id[df$NEXT_DOWN == b]
})
names(upstream_list) <- df$basin_id

# Recursive function to compute cumulative mining
get_total <- function(basin) {
  # Start with own mining
  total <- mining_total[basin]
  # Add cumulative mining from upstream basins
  for (up in upstream_list[[basin]]) {
    total <- total + get_total(up)
  }
  return(total)
}

get_artisanal <- function(basin) {
  # Start with own mining
  artisanal <- mining_artisanal[basin]
  # Add cumulative mining from upstream basins
  for (up in upstream_list[[basin]]) {
    artisanal <- artisanal + get_artisanal(up)
  }
  return(artisanal)
}
get_industrial <- function(basin) {
  # Start with own mining
  industrial <- mining_industrial[basin]
  # Add cumulative mining from upstream basins
  for (up in upstream_list[[basin]]) {
    industrial <- industrial + get_industrial(up)
  }
  return(industrial)
}
get_gold <- function(basin) {
  # Start with own mining
  gold <- mining_gold[basin]
  # Add cumulative mining from upstream basins
  for (up in upstream_list[[basin]]) {
    gold <- gold + get_gold(up)
  }
  return(gold)
}

# Apply to all basins
df$cumulative_mining <- sapply(df$basin_id, get_total)
df$cumulative_artisanal <- sapply(df$basin_id, get_artisanal)
df$cumulative_industrial <- sapply(df$basin_id, get_industrial)
df$cumulative_gold <- sapply(df$basin_id, get_gold)

df1 <- df %>%
  mutate(cumulative_artisanal_share = cumulative_artisanal/cumulative_mining,
         cumulative_industrial_share = cumulative_industrial/cumulative_mining,
         cumulative_gold_share = cumulative_gold/cumulative_mining) %>%
  select(basin_id, cumulative_mining, cumulative_artisanal, cumulative_industrial, cumulative_gold,
         cumulative_artisanal_share, cumulative_industrial_share, cumulative_gold_share)

# Convert basins_l12_2000$basin_id to character
basins_l12_2000 <- basins_l12_2000 %>%
  mutate(basin_id = as.character(basin_id))

# Then do the join
basins_l12_2000 <- basins_l12_2000 %>%
  left_join(df1, by = "basin_id")


# Create a lookup table with mining info from mine basins
mine_info <- basins_l12_2000 %>%
  filter(status == "mine") %>%
  select(basin_id, mining_ha_total, mining_ha_artisanal, mining_ha_industrial, mining_ha_gold,
         mining_class,
         mining_ha_artisanal_share, mining_ha_industrial_share, mining_ha_gold_share,
         cumulative_mining, cumulative_artisanal, cumulative_industrial, cumulative_gold,
         cumulative_artisanal_share, cumulative_industrial_share, cumulative_gold_share) %>%
  rename(mine_basin = basin_id,
         mine_mining_ha_total = mining_ha_total,
         mine_mining_class = mining_class,
         mine_mining_ha_artisanal = mining_ha_artisanal, 
         mine_mining_ha_industrial = mining_ha_industrial, 
         mine_mining_ha_gold = mining_ha_gold,
         mine_mining_ha_artisanal_share = mining_ha_artisanal_share, 
         mine_mining_ha_industrial_share = mining_ha_industrial_share, 
         mine_mining_ha_gold_share = mining_ha_gold_share,
         mine_cumulative_mining = cumulative_mining, 
         mine_cumulative_artisanal = cumulative_artisanal, 
         mine_cumulative_industrial = cumulative_industrial, 
         mine_cumulative_gold = cumulative_gold,
         mine_cumulative_artisanal_share = cumulative_artisanal_share, 
         mine_cumulative_industrial_share = cumulative_industrial_share, 
         mine_cumulative_gold_share = cumulative_gold_share)


basins_l12_2000 <- basins_l12_2000 %>%
  mutate(mine_basin = as.character(mine_basin))

mine_info <- mine_info %>%
  mutate(mine_basin = as.character(mine_basin))

# Join mining info to downstream basins based on mine_basin
basins_l12_2000_updated <- basins_l12_2000 %>%
  left_join(mine_info, by = "mine_basin") %>%
  mutate(
    mining_ha_total = ifelse(status %in% c("downstream", "upstream"), mine_mining_ha_total, mining_ha_total),
    mining_class = ifelse(status %in% c("downstream", "upstream"), mine_mining_class, mining_class),
    mining_ha_artisanal = ifelse(status %in% c("downstream", "upstream"), mine_mining_ha_artisanal, mining_ha_artisanal),
    mining_ha_industrial = ifelse(status %in% c("downstream", "upstream"), mine_mining_ha_industrial, mining_ha_industrial),
    mining_ha_gold = ifelse(status %in% c("downstream", "upstream"), mine_mining_ha_gold, mining_ha_gold),
    mining_ha_artisanal_share = ifelse(status %in% c("downstream", "upstream"), mine_mining_ha_artisanal_share, mining_ha_artisanal_share),
    mining_ha_industrial_share = ifelse(status %in% c("downstream", "upstream"), mine_mining_ha_industrial_share, mining_ha_industrial_share),
    mining_ha_gold_share = ifelse(status %in% c("downstream", "upstream"), mine_mining_ha_gold_share, mining_ha_gold_share),
    cumulative_mining = ifelse(status %in% c("downstream", "upstream"), mine_cumulative_mining, cumulative_mining),
    cumulative_artisanal = ifelse(status %in% c("downstream", "upstream"), mine_cumulative_artisanal, cumulative_artisanal),
    cumulative_industrial = ifelse(status %in% c("downstream", "upstream"), mine_cumulative_industrial, cumulative_industrial),
    cumulative_gold = ifelse(status %in% c("downstream", "upstream"), mine_cumulative_gold, cumulative_gold),
    cumulative_artisanal_share = ifelse(status %in% c("downstream", "upstream"), mine_cumulative_artisanal_share, cumulative_artisanal_share),
    cumulative_industrial_share = ifelse(status %in% c("downstream", "upstream"), mine_cumulative_industrial_share, cumulative_industrial_share),
    cumulative_gold_share = ifelse(status %in% c("downstream", "upstream"), mine_cumulative_gold_share, cumulative_gold_share)
  ) %>%
  select(
    -mine_mining_ha_total, -mine_mining_class, -mine_mining_ha_artisanal,
    -mine_mining_ha_industrial, -mine_mining_ha_gold,
    -mine_mining_ha_artisanal_share, -mine_mining_ha_industrial_share,
    -mine_mining_ha_gold_share, -mine_cumulative_mining,
    -mine_cumulative_artisanal, -mine_cumulative_industrial,
    -mine_cumulative_gold, -mine_cumulative_artisanal_share,
    -mine_cumulative_industrial_share, -mine_cumulative_gold_share
  )

write_sf(basins_l12_2000_updated, "/home/francesca/brazil_mining/mining_health/mine_basins_lev07/basins_l07_2023.shp")
# write_sf(so, gsub("gpkg$", "shp", file))

write_sf(basins_l12_2000_updated, "/home/francesca/brazil_mining/mining_health/mine_basins_lev07/basins_l07_2023.gpkg")
# write_sf(so, gsub("gpkg$", "shp", file))

