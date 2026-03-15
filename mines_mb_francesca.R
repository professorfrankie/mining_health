library(dplyr)
library(sf)
library(terra)


muni_conc <- readRDS("./int/muni_conc.RDS")

all_polys <- c()
all_dfs <- c()
for(yy in 1985:2023) {
  
  print(cat("Year ", yy, " started... "))
  
  t.start <- Sys.time()
  
  tmp_raster <- rast(paste0("/data/brazil/mining_mapbiomas/mining_area_tif/", 
                            "mining_", yy, ".tif"))
  tmp_multipolys <- st_as_sf(as.polygons(tmp_raster)) |> 
    rename(mined_substance = paste0("mined_substance_", yy)) |> 
    mutate(year = yy, .before = mined_substance)
  tmp_polys <- st_cast(tmp_multipolys |> 
                         filter(mined_substance != 0, st_geometry_type(geometry) == "MULTIPOLYGON"), "POLYGON") |> 
    rbind(tmp_multipolys |> filter(st_geometry_type(geometry) != "MULTIPOLYGON")) |> 
    mutate(mining_area = st_area(geometry))
  
  tmp_df <- st_drop_geometry(tmp_polys) |> 
    transmute(year, 
              mining_id = seq_len(nrow(tmp_polys)), 
              mining_class = mined_substance, 
              mining_area_ha = units::drop_units(mining_area) / 10^4, 
              lon = st_coordinates(st_centroid(tmp_polys$geometry))[,1], 
              lat = st_coordinates(st_centroid(tmp_polys$geometry))[,2])
  
  write_sf(tmp_polys, paste0("/data/brazil/mining_mapbiomas/mining_area_gpkg/", 
                             "mb_mines_", yy, ".gpkg"))
  
  saveRDS(tmp_df, paste0("/data/brazil/mining_mapbiomas/mining_area_csv/", 
                         "mb_mines_", yy, ".csv"))
  
  all_polys <- rbind(all_polys, tmp_polys)
  all_dfs <- rbind(all_dfs, tmp_df)
  
  rm(tmp_raster, tmp_polys, tmp_df); gc()
  
  cat("finished after:", round(Sys.time() - t.start, 2), units(Sys.time() - t.start), "\n")
}

write_sf(all_polys, paste0("/data/brazil/mining_mapbiomas/mining_area_gpkg/", 
                           "mb_mines_all.gpkg"))

saveRDS(all_dfs, paste0("/data/brazil/mining_mapbiomas/mining_area_csv/", 
                       "mb_mines_all.csv"))


