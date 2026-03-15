# Load libraries
library(sf)
library(geobr)
library(ggplot2)
library(dplyr)

# 1. Load Brazilian Municipalities
br_mun <- read_municipality(year = 2020)

# 2. Load HydroBASINS shapefile
lvl <- "07" # Basin level, not sure which one to use
hydrobasins <- st_read(paste0("/data/brazil_health/HYBAS/hybas_lake_sa_lev", lvl, "_v1c.shp"))

# 3. Reproject HydroBASINS to match municipality CRS
hydrobasins <- st_transform(hydrobasins, st_crs(br_mun))

# 4. (Optional) Clip HydroBASINS to Brazil boundary
# Use geobr's country boundary
br <- read_country()

# Load lwgeom for robust geometry repair
install.packages("lwgeom")
library(lwgeom)

# Fix invalid geometries in HydroBASINS
hydrobasins <- st_make_valid(hydrobasins)

# Optional: fix invalid country boundary too (just in case)
br <- st_make_valid(br)

# Now try the intersection again
hydro_brazil <- st_intersection(hydrobasins, br)


# 5. Plot
muni_basin_l05 <- ggplot() +
  geom_sf(data = br_mun, fill = NA, color = "grey60", size = 0.1) +
  geom_sf(data = hydro_brazil, fill = NA, color = "blue", alpha = 0.5) +
  labs(
    title = "HydroBASINS and Brazilian Municipalities",
    subtitle = "HydroBASINS Level 05 clipped to Brazil",
    caption = "HydroBASINS: HydroSHEDS | Municipalities: geobr"
  ) +
  theme_minimal()

muni_basin_l06 <- ggplot() +
  geom_sf(data = br_mun, fill = NA, color = "grey30", size = 0.1) +
  geom_sf(data = hydro_brazil, fill = NA, color = "blue", alpha = 0.5) +
  labs(
    title = "HydroBASINS and Brazilian Municipalities",
    subtitle = "HydroBASINS Level 06 clipped to Brazil",
    caption = "HydroBASINS: HydroSHEDS | Municipalities: geobr"
  ) +
  theme_minimal()

muni_basin_l07 <- ggplot() +
  geom_sf(data = br_mun, fill = NA, color = "grey30", size = 0.1) +
  geom_sf(data = hydro_brazil, fill = NA, color = "blue", alpha = 0.5) +
  geom_sf(data = br_mun, fill = NA, color = "grey30", size = 0.1) +
  labs(
    title = "HydroBASINS and Brazilian Municipalities",
    subtitle = "HydroBASINS Level 07 clipped to Brazil",
    caption = "HydroBASINS: HydroSHEDS | Municipalities: geobr"
  ) +
  theme_minimal()

ggsave("/home/francesca/brazil_mining/mining_health/figures/muni_basin_l05.png", plot = muni_basin_l05, width = 10, height = 8, dpi = 300)
ggsave("/home/francesca/brazil_mining/mining_health/figures/muni_basin_l06.png", plot = muni_basin_l06, width = 10, height = 8, dpi = 300)
ggsave("/home/francesca/brazil_mining/mining_health/figures/muni_basin_l07.png", plot = muni_basin_l07, width = 10, height = 8, dpi = 300)