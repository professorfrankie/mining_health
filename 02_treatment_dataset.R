library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(tidyselect)
library(sf)
library(geobr)


# Load municipality capitals (once)
capitals <- read_municipal_seat(year = 2010)  # 2010 is fine for spatial accuracy

# Set path where your basin files are stored
basin_path <- "/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07"

# List of years you want to process
years <- 1998:2023

# Function to join basin for a single year
process_year <- function(y) {
  file <- file.path(basin_path, paste0("basins_l07_", y, ".gpkg"))
  
  if (!file.exists(file)) {
    warning(paste("Missing file for year:", y))
    return(NULL)
  }
  
  basin <- st_read(file, quiet = TRUE)
  
  # Match CRS
  caps <- st_transform(capitals, st_crs(basin))
  
  # Spatial join
  joined <- st_join(caps, basin, join = st_within, left = TRUE) %>%
    mutate(year = y)
  
  return(joined)
}

# Apply to all years and combine
capitals_all_years <- map_dfr(years, process_year)
#capitals_all_years <- capitals_all_years %>%
#  select(code_muni, name_muni, abbrev_state, year, dist_n, dist_order, status)

write_sf(capitals_all_years, "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_location.shp")
write_sf(capitals_all_years, "/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_location.gpkg")

# Load municipality polygons (not just capital points)
municipalities <- read_municipality(year = 2023) %>% 
  st_make_valid()  # ensures clean geometries

# Path to HydroBASINS files
basin_path <- "/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07"
years <- 1998:2023
threshold <- 0.2  # 20%

# Function to check overlaps per year
process_year_overlap <- function(y) {
  message("Processing year: ", y)
  file <- file.path(basin_path, paste0("basins_l07_", y, ".gpkg"))
  
  if (!file.exists(file)) {
    warning(paste("Missing file for year:", y))
    return(NULL)
  }
  
  basin <- st_read(file, quiet = TRUE) %>%
    st_make_valid() %>%
    st_simplify(preserveTopology = TRUE, dTolerance = 1000)
  
  # Transform municipalities to match CRS of basin
  muni <- st_transform(municipalities, st_crs(basin)) %>%
    st_simplify(preserveTopology = TRUE, dTolerance = 1000) %>%
    mutate(muni_area_km2 = as.numeric(st_area(geom)) / 1e6)
  
  # Filter only municipalities that touch/intersect any basin polygon
  muni_filtered <- muni %>%
    st_filter(basin, .predicate = st_intersects)  # much faster than full intersection
  
  if (nrow(muni_filtered) == 0) return(NULL)  # skip if nothing overlaps
  
  # Intersect only the filtered set
  intersection <- st_intersection(muni_filtered, basin) %>%
    mutate(intersection_area_km2 = as.numeric(st_area(geom)) / 1e6)
  
  # Compute percentage and keep the best (smallest dist_order)
  overlap <- intersection %>%
    mutate(percent_overlap = intersection_area_km2 / muni_area_km2) %>%
    filter(percent_overlap >= threshold) %>%
    group_by(code_muni) %>%
    slice_min(order_by = dist_n, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(year = y)
  
  return(overlap)
}

# Apply across years
munis_overlapping_basins <- map_dfr(years, process_year_overlap)

write_sf(munis_overlapping_basins, "/home/francesca/brazil_mining/mining_health/processed data/treatment_area_overlap_20.shp")
write_sf(munis_overlapping_basins, "/home/francesca/brazil_mining/mining_health/processed data/treatment_area_overlap_20.gpkg")


capitals_all_years <- st_read("/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_location.gpkg")
munis_overlapping_basins1 <- st_read("/home/francesca/brazil_mining/mining_health/processed data/treatment_area_overlap_20.gpkg",
                                     layer = "treatment_area_overlap_20"
                                     )


#Treatment 3: combination of area overlap and capial location
# Ensure keys are consistent
capitals_all_years <- capitals_all_years %>%
  mutate(code_muni_chr = as.character(code_muni),
         year = as.integer(year),
         basin_id = as.character(basin_id),
         mine_basin = as.character(mine_basin))

munis_overlapping_basins1 <- munis_overlapping_basins1 %>%
  mutate(code_muni_chr = as.character(code_muni),
         year = as.integer(year),
         basin_id = as.character(basin_id),
         mine_basin = as.character(mine_basin),
         code_state = as.character(code_state),
         code_region = as.character(code_region))

# Combine keeping all columns
combined_treatment <- bind_rows(capitals_all_years, munis_overlapping_basins1) %>%
  group_by(code_muni_chr, year) %>%
  slice_min(order_by = dist_n, with_ties = FALSE) %>%
  ungroup()

write_sf(combined_treatment, "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_20.shp")
write_sf(combined_treatment, "/home/francesca/brazil_mining/mining_health/processed data/treatment_combined_20.gpkg")


# expand treatment overlap dataset
#treatment_overlap <- st_read("/home/francesca/brazil_mining/mining_health/treatment_area_overlap_20.gpkg")

#municipalities <- read_municipality(year = 2010) %>% 
#  st_make_valid() %>%
#  select(code_muni, name_muni)

#expanded_df <- crossing(
#  municipalities,
#  year = 1998:2023
#)

#treatment_overlap1 <- expanded_df %>%
#  left_join(treatment_overlap, by = c("year" = "year", "code_muni" = "code_muni")) %>%
#  select(-geom.x, -name_muni.x) %>%
#  rename(geom = geom.y,
#         name_muni = name_muni.y)
#write_sf(treatment_overlap1, "/home/francesca/brazil_mining/mining_health/treatment_area_overlap_20.shp")
#write_sf(treatment_overlap1, "/home/francesca/brazil_mining/mining_health/treatment_area_overlap_20.gpkg")
