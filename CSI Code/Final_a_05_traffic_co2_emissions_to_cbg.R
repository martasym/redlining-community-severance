# Sets coordinate reference system
crs <- 2163

# Sets the pathways to the different folders.

# Main folder can be changed depending on where the finals folder is on computer.
main_folder <- "/Users/martasymkowick/CommunitySeverance/code_MS/Finals/"
generated.folder.CSI <- "CSI/Generated/"

# Loads the DARTE traffic CO2 emission data: https://daac.ornl.gov/CMS/guides/CMS_DARTE_V2.html 
traffic_co2_emis <- sf::read_sf(paste0(main_folder, "pulledData/", "DARTE_v2.gdb"))

# Sets the variable to be density to kg CO₂/m².
traffic_co2_emis$traffic_co2_emis <- traffic_co2_emis$kgco2_2017 / traffic_co2_emis$bg_area_m2
traffic_co2_emis <- traffic_co2_emis[,c("GEOID", "traffic_co2_emis")]

# Better clarifies the dataset by dropping geometry and standardizing variable name.
traffic_co2_emis_df <- traffic_co2_emis
sf::st_geometry(traffic_co2_emis_df) <- NULL
colnames(traffic_co2_emis_df)[1] <- "GEOID20"

# Joins together the different pieces of data, so it is geoid with the traffic co2 emissions (in kg/ m^2)
grid_contxt_df <- dplyr::left_join(grid_contxt_df, traffic_co2_emis_df, by = "GEOID20")

# Saves the dataset.
saveRDS(grid_contxt_df, paste0(main_folder, generated.folder.CSI, "traffic_co2_emis_", name_short,".rds"))

# Version used in CSI
Used_CO2_Emis <- readRDS( paste0(main_folder, "CSI/Data Test/", "traffic_co2_emis_", name_short,".rds"))
