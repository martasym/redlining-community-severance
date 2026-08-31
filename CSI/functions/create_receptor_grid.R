### script objective: create receptor grid for a given spatial resolution and spatial context
if(config$evaluation == TRUE){
  
  netcdf_obs <- RNetCDF::open.nc(config$example_obs_file)
  lat_obs <- RNetCDF::var.get.nc(netcdf_obs, "latitude")
  lon_obs <- RNetCDF::var.get.nc(netcdf_obs, "longitude")
  station_reference <- RNetCDF::var.get.nc(netcdf_obs, "station_reference")
  stations <- data.frame(station_reference = station_reference, lat = lat_obs, lon = lon_obs)
  stations_sf <- sf::st_as_sf(stations, coords = c("lon", "lat"), crs = config$crs_long_lat) %>%
    sf::st_transform(config$crs_sim)
  sites <- sf::st_intersection(stations_sf, spatial_context)
  
  if(nrow(sites)>0){
    sites_to_bind <- sites[,-1]
    names(sites_to_bind) <- "geom"
    sf::st_geometry(sites_to_bind) <- "geom"
    
    if(config$receptor_grid == TRUE){
      receptor_grid <- sf::st_sf(geom=sf::st_make_grid(spatial_context, cellsize = config$sp_res, what = "centers"), crs=config$crs_sim)
      receptor_grid <- rbind(sites_to_bind, receptor_grid)
    } else {
      receptor_grid <- sites_to_bind
    }
    # save evaluation sites in rds object
    sites$rec_id <- 1:nrow(sites)
    saveRDS(sites, paste0(config$input_static_path, "evaluation_sites.rds"))
    number_of_sites <- nrow(sites)
  } else {
    #receptor_grid <- sf::st_sf(geom=sf::st_make_grid(spatial_context, cellsize = config$sp_res, what = "centers"), crs=config$crs_sim)
    print("sorry but there are no monitoring stations in this spatial context within our database")
  }
} else {
  receptor_grid <- sf::st_sf(geom=sf::st_make_grid(spatial_context, cellsize = config$sp_res, what = "centers"), crs=config$crs_sim)
}
# if there are evaluation sites, the initial receptor_ids, from 1 to number_of_sites, will be given to them
receptor_grid$rec_id <- 1:nrow(receptor_grid)

## categorize receptors based on distance between receptor grid and emission sources by estimate
## distance between receptor grid and emission sources. receptors closer than 250 m from roads with
## AADT > 2000 are run within R-LINE. receptors further away
## receive uniquely values from CALIOPE interpolated. receptors in the transition area
## (from 140 m distance to road to 250 m distance) receive an weighted interpolated value
## between CALIOPE and CALIOPE-Urban


roads <- sf::read_sf(paste0(config$input_static_path,config$shapefile_name, ".shp")) %>%
#roads <- sf::read_sf(paste0(config$path_to_roads_txt_and_shp,config$shapefile_name, ".shp")) %>%
  # a road id from 1 to the number of roads is given at this step
  dplyr::mutate(road_id = 1:nrow(.))

#rec_close_250 <- sf::st_is_within_distance(receptor_grid, roads[roads$aadt > 2000,], dist = 250)

rec_close_250 <- sf::st_is_within_distance(sf::st_transform(receptor_grid, crs = sf::st_crs(roads)), roads[roads$aadt > 2000,], dist = 250)
rec_far_250_from_source <- as.numeric(lapply(rec_close_250, length))
close_250 <- ifelse(rec_far_250_from_source > 0, "yes", "no")
receptor_grid$within_250m <- close_250

# within 140 m and 250 m

#rec_within_140 <- sf::st_is_within_distance(receptor_grid[receptor_grid$within_250m == "yes",], roads[roads$aadt > 2000,], 140)

rec_within_140 <- sf::st_is_within_distance((sf::st_transform(receptor_grid, crs = sf::st_crs(roads)))[receptor_grid$within_250m == "yes",], roads[roads$aadt > 2000,], 140)



rec_far_140_from_source <- as.numeric(lapply(rec_within_140, length))
close_140 <- ifelse(rec_far_140_from_source > 0, "yes", "no")
receptor_grid$within_140m <- "no"
receptor_grid[receptor_grid$within_250m == "yes", "within_140m"] <- close_140

saveRDS(receptor_grid, paste0(config$input_static_path, "receptor_grid.rds"))

rm(rec_close_250,rec_within_140)
rm(close_250,close_140,rec_far_140_from_source,rec_far_250_from_source)
