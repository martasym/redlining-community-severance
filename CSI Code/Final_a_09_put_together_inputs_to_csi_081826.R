# Sets the pathways to the different folders.

# Main folder can be changed depending on where the finals folder is on computer.
main_folder <- "/Users/martasymkowick/CommunitySeverance/code_MS/Finals/"
generated.folder.CSI <- "CSI/Generated/"

# Pulling the generated data produced in running.
traffic_count_2_grid_sld <- readRDS(paste0(main_folder, generated.folder.CSI, "traffic_count_2_grid_sld_", name_short,".rds"))
road_inf_dist_2_grid_sld <- readRDS(paste0(main_folder, generated.folder.CSI, "road_inf_dist_2_grid_sld_", name_short,".rds"))
barrier_factor <- readRDS(paste0(main_folder, generated.folder.CSI, "barrier_sp_units_", name_short,"_for_use.rds"))
traffic_co2_emis <- readRDS(paste0(main_folder, generated.folder.CSI, "traffic_co2_emis_", name_short,".rds"))
traffic_segment_2_grid_sld <- readRDS(paste0(main_folder, generated.folder.CSI, "traffic_segment_2_grid_sld_new_070126", name_short ,".rds"))

# Load grids and smart location dataset subset
data_desc <- readRDS(paste0(main_folder, "CSI/Data Test/", "smart_location_data_subset_desc.rds"))

# Subset data for community severance index estimation
vars_in <- which(colnames(sld_us_loc) %in% c("GEOID20",  
                                             "D3AAO", "D3APO", 
                                             "D3B", "D3BAO", "NatWalkInd"))
sld_us_loc_df <- sld_us_loc
sf::st_geometry(sld_us_loc_df) <- NULL
sld_us_loc_df <- sld_us_loc_df[ , vars_in]

colnames(sld_us_loc_df)[c(2:5)] <- c("autom_netw_dens", "pedest_netw_dens", "street_no_autom_inters_dens", "autom_inters_dens")
sld_us_loc_df <- as.data.frame(sld_us_loc_df)
sld_us_loc_df[sld_us_loc_df %in% c(-99999)] <- NA

# join data
traffic_count_2_grid_sld_df <- traffic_count_2_grid_sld
sf::st_geometry(traffic_count_2_grid_sld_df) <- NULL
traffic_count_2_grid_sld_df <- traffic_count_2_grid_sld_df[,c("GEOID20", "aadt")]
colnames(traffic_count_2_grid_sld_df) <- c("GEOID20", "aadt_esri_point")

traffic_segment_2_grid_sld_df <- traffic_segment_2_grid_sld
sf::st_geometry(traffic_segment_2_grid_sld_df) <- NULL
traffic_segment_2_grid_sld_df <- traffic_segment_2_grid_sld_df[,c("GEOID20", "aadt")]
colnames(traffic_segment_2_grid_sld_df) <- c("GEOID20", "aadt_fhwa_segm")


barrier_factor <- barrier_factor[,c("GEOID20", "barrier_factor_osm", "barrier_factor_fhwa")]

road_inf_dist_2_grid_sld_df <- road_inf_dist_2_grid_sld
sf::st_geometry(road_inf_dist_2_grid_sld_df) <- NULL

data_in_cs <- dplyr::left_join(sld_us_loc_df, traffic_count_2_grid_sld_df, by = "GEOID20") %>%
  dplyr::left_join(traffic_segment_2_grid_sld_df, by = "GEOID20") %>%
  dplyr::left_join(road_inf_dist_2_grid_sld_df, by = "GEOID20") %>%
  dplyr::left_join(traffic_co2_emis, by = "GEOID20") %>%
  dplyr::left_join(barrier_factor, by = "GEOID20")

data_cs <- subset(data_in_cs, !(data_in_cs$GEOID20 == "060816009001"))
data_cs <- subset(data_cs, !(data_cs$GEOID20 == "060816009002"))
data_cs <- subset(data_cs, !(data_cs$GEOID20 == "060816007002"))
data_cs <- subset(data_cs, !(data_cs$GEOID20 == "060750179021"))
data_in_cs <- data_cs

# explore summary descriptive
summ_data_in_cs <- sumtable(data_in_cs, file=paste0(main_folder, generated.folder.CSI, 'cs_input_summary_edited', name_short))
colnames(data_desc)[1] <- "Variable"
#summ_data_in_cs <- dplyr::left_join(summ_data_in_cs, data_desc, by = "Variable")


# Homogenize data ranges by scaling by standard deviation
data_in_cs_id <- data_in_cs[,c("GEOID20")]
dta <- data_in_cs[ , -c(which(colnames(data_in_cs) == "GEOID20"))]
dta_scaled = as.data.frame(apply(dta, 2, function(a) a/sd(a, na.rm = T)))
dta_prep <- cbind(data_in_cs_id, dta_scaled)
colnames(dta_prep)[1] <- "GEOID20"

# Build dataframe for distributional characteristics 
# Delete variables distance to road and add proximity based

vtable::st(dta, add.median = T,fit.page = '\\textwidth', digits = 2, out = 'latex')

dta_prep <- cbind(data_in_cs_id, dta_scaled)
colnames(dta_prep)[1] <- "GEOID20"

# Make sure to change the date before saving.
name_short_readable <- "sf_082026_"

saveRDS(dta_prep, paste0(main_folder, generated.folder.CSI, "community_severance_", name_short_readable, "_input_data.rds"))

Used_CSIData <- readRDS(paste0(main_folder, "CSI/Data Test/" , "community_severance_sf_081826__input_data.rds"))

