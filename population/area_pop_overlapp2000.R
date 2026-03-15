install.packages(c("terra", "sf", "exactextractr", "dplyr"))

library(terra)           # for raster operations
library(sf)              # for vector (shapefile) operations
library(exactextractr)   # fast & accurate zonal stats
library(dplyr)           # data manipulation

pop_raster <- rast("/home/francesca/brazil_mining/population/bra_ppp_2000_1km_Aggregated.tif")

# Municipal boundaries (make sure CRS matches the raster)
municipalities <- st_read("/home/francesca/brazil/shp/BR_Municipios_2021.shp")

# Downstream basin boundaries
basins <- st_read("/home/francesca/brazil_mining/basins_l12_2000.gpkg")

municipalities <- st_transform(municipalities, crs(pop_raster))
basins <- st_transform(basins, crs(pop_raster))

# Use exactextractr to sum population within each municipality
total_pop <- exact_extract(pop_raster, municipalities, 'sum')

# Add result to your municipality data
municipalities$total_pop <- total_pop

# Intersect geometries
muni_downstream_overlap <- st_intersection(municipalities, basins)

# Optional: Keep track of original IDs
muni_downstream_overlap <- muni_downstream_overlap %>%
  mutate(muni_id = municipalities$ID)  # Replace `ID` with your municipality identifier

# Sum population in overlapping parts
overlap_pop <- exact_extract(pop_raster, muni_downstream_overlap, 'sum')

# Add to the overlap object
muni_downstream_overlap$overlap_pop <- overlap_pop

# Aggregate overlap_pop by municipality
overlap_summary <- muni_downstream_overlap %>%
  st_drop_geometry() %>%
  group_by(muni_id) %>%
  summarise(overlap_pop = sum(overlap_pop, na.rm = TRUE))

# Join with full municipality data
municipalities <- municipalities %>%
  left_join(overlap_summary, by = c("ID" = "muni_id")) %>%
  mutate(
    overlap_pop = ifelse(is.na(overlap_pop), 0, overlap_pop),
    exposure_weight = overlap_pop / total_pop
  )

