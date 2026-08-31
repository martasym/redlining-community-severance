# Sets the pathways to the different folders.

# Main folder can be changed depending on where the finals folder is on computer.
main_folder <- "/Users/martasymkowick/CommunitySeverance/code_MS/Finals/"
generated.folder.CSI <- "CSI/Generated/"

# Re-specifies the grid context.
grid_contxt <- sf::st_centroid(sld_us_loc[,c("GEOID20")]) 
grid_contxt_df <- grid_contxt
sf::st_geometry(grid_contxt_df) <- NULL

aadt <- readRDS(paste0(main_folder, "pulledData/aadt_ca_2019.rds"))
aadt_segments <- aadt 

aadt_segments <- sf::st_transform(aadt_segments, crs = crs)
aadt_segments_id_cntxt <- sapply(sf::st_intersects(aadt_segments, spatial_context),function(x){length(x)>0})
aadt_segments_contxt <- aadt_segments[aadt_segments_id_cntxt, ]
aadt_segments_contxt <- sf::st_cast(aadt_segments_contxt, "LINESTRING")

aadt_segments_contxt <- aadt_segments_contxt[which(!is.na(aadt_segments_contxt$aadt)),]

n_points <- 3 
road_points <- sf::st_transform(aadt_segments_contxt, crs) %>%
  sf::st_line_sample(n = n_points, type = "regular") %>%
  sf::st_cast("POINT") 
aadt_segments_p <- st_sf(aadt = rep(aadt_segments_contxt$aadt, each =n_points), geom = road_points)

# Save AADT segments and grid context
saveRDS(aadt_segments_contxt, paste0(main_folder, generated.folder.CSI, "aadt_segments_contxt.rds"))
saveRDS(grid_contxt, paste0(main_folder, generated.folder.CSI, "grid_contxt.rds"))

aadt_segments_contxt <- readRDS(paste0(main_folder, generated.folder.CSI, "aadt_segments_contxt.rds"))
grid_contxt <- readRDS(paste0(main_folder, generated.folder.CSI, "grid_contxt.rds"))

# kriging from mid point  
# adapted from Criado et al. (2022) https://earth.bsc.es/gitlab/es/universalkriging/-/blob/production/general/UK_mean.R

UK_mean_uniform_ok <- regrid_ok(non_uniform_data = sf::as_Spatial(aadt_segments_p), # traffic_esri_cntxt
                                target_grid = sf::as_Spatial(grid_contxt), crs_sim = crs)

new <- UK_mean_uniform_ok[which(is.na(UK_mean_uniform_ok$var1.pred)),]

# Remove coincident sampling points.
# Nine duplicate coordinates caused singular covariance matrices
# and two missing kriging predictions. Removing the duplicates
# produced virtually identical predictions while resolving the NAs.
aadt_segments_p_new <- st_difference(aadt_segments_p)

UK_mean_uniform_ok_new <- regrid_ok(non_uniform_data = sf::as_Spatial(aadt_segments_p_new), # traffic_esri_cntxt
                                target_grid = sf::as_Spatial(grid_contxt), crs_sim = crs)

summary(UK_mean_uniform_ok)
summary(UK_mean_uniform_ok_new)

colnames(UK_mean_uniform_ok_new)[1] <- "aadt"
# Evaluates that the dataset without repeats will work.
grid_contxt$Shape == UK_mean_uniform_ok_new$geometry

UK_mean_uniform_ok_new$GEOID20 <- grid_contxt$GEOID20

saveRDS(UK_mean_uniform_ok_new, paste0(main_folder, generated.folder.CSI, "traffic_segment_2_grid_sld_", name_short ,".rds"))
