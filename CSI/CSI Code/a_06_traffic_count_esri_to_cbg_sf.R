# Sets the pathways to the different folders.

# Main folder can be changed depending on where the finals folder is on computer.
main.folder <- "/Users/martasymkowick/CommunitySeverance/code_MS/Finals/"
generated.folder.CSI <- "CSI/Generated/"

# Re-specifies the grid context.
grid_contxt <- sf::st_centroid(sld_us_loc[,c("GEOID20")]) 
grid_contxt_df <- grid_contxt
sf::st_geometry(grid_contxt_df) <- NULL

# Loads the DARTE traffic ESRI Traffic data emission data: https://demographics5.arcgis.com/arcgis/rest/services/USA_Traffic_Counts/MapServer/0
# Downloaded at a_03_prep_traffic.R
traffic_esri <- readRDS(paste0(main_folder,"/CSI/PulledData/traffic_counts_esri.rds"))

traffic_esri <- sf::st_transform(traffic_esri, crs)
colnames(traffic_esri)[which(colnames(traffic_esri) == "Traffic1")] <- "aadt"

traffic_esri_id_cntxt <- sapply(sf::st_intersects(traffic_esri, spatial_context),function(x){length(x)>0})
traffic_esri_cntxt <- traffic_esri[traffic_esri_id_cntxt, ]

# Regrid function can be found at code/functions
UK_mean_uniform_ok <- regrid_ok(non_uniform_data = sf::as_Spatial(traffic_esri_cntxt), # traffic_esri_cntxt
                                target_grid = sf::as_Spatial(grid_contxt), crs_sim = crs) # grid_contxt

# Renames the traffic count variable as ADDT and assigns the corresponding GEOID.
colnames(UK_mean_uniform_ok)[1] <- "aadt"
UK_mean_uniform_ok$GEOID20 <- grid_contxt$GEOID20

saveRDS(UK_mean_uniform_ok, paste0(main.folder, generated.folder.CSI, "traffic_count_2_grid_sld_",name_short, ".rds"))
