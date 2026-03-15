library(sf)
library(dplyr)
library(geobr)

basins_l07_1998 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_1998.shp")
basins_l07_1999 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_1999.shp")
basins_l07_2000 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2000.shp")
basins_l07_2001 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2001.shp")
basins_l07_2002 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2002.shp")
basins_l07_2003 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2003.shp")
basins_l07_2004 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2004.shp")
basins_l07_2005 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2005.shp")
basins_l07_2006 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2006.shp")
basins_l07_2007 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2007.shp")
basins_l07_2008 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2008.shp")
basins_l07_2009 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2009.shp")
basins_l07_2010 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2010.shp")
basins_l07_2011 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2011.shp")
basins_l07_2012 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2012.shp")
basins_l07_2013 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2013.shp")
basins_l07_2014 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2014.shp")
basins_l07_2015 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2015.shp")
basins_l07_2016 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2016.shp")
basins_l07_2017 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2017.shp")
basins_l07_2018 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2018.shp")
basins_l07_2019 <- read_sf("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2019.shp")

combined <- bind_rows(basins_l07_1998, basins_l07_1999, basins_l07_2000, basins_l07_2001,
                      basins_l07_2002, basins_l07_2003, basins_l07_2004, basins_l07_2005,
                      basins_l07_2006, basins_l07_2007, basins_l07_2008, basins_l07_2009,
                      basins_l07_2010, basins_l07_2011, basins_l07_2012, basins_l07_2013,
                      basins_l07_2014, basins_l07_2015, basins_l07_2016, basins_l07_2017,
                      basins_l07_2018, basins_l07_2019)
length(unique(combined$basin_d))
combined_unique <- combined %>%
  distinct(basin_d, PFAF_ID, geometry, .keep_all = TRUE)
glimpse(combined_unique)


mean(combined_unique$dist_km)
#156.8341

#calculating mean diameter using the area of a circle
combined_unique$area_m2 <- st_area(combined_unique)
combined_unique$area_km2 <- st_area(combined_unique) / 1e6 
mean(combined_unique$area_km2)

combined_unique$diameter_m <- 2 * sqrt(combined_unique$area_m2 / pi)
combined_unique$diameter_km <- combined_unique$diameter_m / 1000
combined_unique$diameter_m <- as.numeric(combined_unique$diameter_m)
combined_unique$diameter_km <- as.numeric(combined_unique$diameter_km)
mean(combined_unique$diameter_km)
# 50.72131





basins <- st_read("/data/brazil_health/HYBAS/hybas_lake_sa_lev07_v1c.shp")
basins_dist <- basins %>%
  st_drop_geometry() %>%  
  select(HYBAS_ID, NEXT_DOWN, DIST_SINK)  
basins_dist <- basins_dist %>%
  left_join(., ., by = c("NEXT_DOWN" = "HYBAS_ID")) %>%
  mutate(
    dist_km = ifelse(
      NEXT_DOWN == 0,       
      DIST_SINK.x,
      DIST_SINK.x - DIST_SINK.y  
    )
  ) %>%
  select(HYBAS_ID, NEXT_DOWN, dist_km)
mean(basins_dist$dist_km)


head(basins_dist)
summary(basins_dist$dist_km)






basins_valid <- basins %>%
  st_make_valid()
st_crs(basins_valid)

municipalities <- read_municipality(year = 2023) %>% 
  st_make_valid()
st_crs(municipalities)

munis <- st_transform(municipalities, st_crs(basins_valid))

st_crs(basins_valid) == st_crs(munis)

munis_no_islands <- munis %>%
  filter(!code_muni %in% c("2605459", "2204659", "3520400", "2607604"))

basins_brazil <- basins_valid %>%
  filter(st_intersects(., munis_no_islands, sparse = FALSE) %>% apply(1, any))

plot(st_geometry(basins_brazil),
     col = "lightblue",
     border = "darkblue",
     main = "Basin Map")

mean(basins_brazil$DIST_)

brazil_basin_2023 <- st_read("/home/francesca/brazil_mining/mining_health/rshiny/mine_basins_lev07/basins_l07_2023.shp")
mean(brazil_basin_2023$dist_km)

treatment_capital_child_mort  <- st_read("/home/francesca/brazil_mining/mining_health/processed data/treatment_capital_child_mort.gpkg")
mean(treatment_capital_child_mort$dist_km, na.rm = TRUE)
