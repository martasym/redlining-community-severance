# Sets the pathways to the different folders.

# Main folder can be changed depending on where the finals folder is on computer.
main_folder <- "/Users/martasymkowick/CommunitySeverance/code_MS/Finals/"
generated.folder.CSI <- "CSI/Generated/"

# Sets the operations so that coordinates are not in degrees
sf::sf_use_s2(FALSE)
## read data

# Sets the number of cores for the estimation of barrier factor
geom_prec_n_cores <- 4

vars_in <- which(colnames(sld_us_loc) %in% c("GEOID20", "CBSA", "CBSA_Name"))
dta <- sld_us_loc[ , vars_in]

grid_contxt <- sf::st_centroid(sld_us_loc[, c("GEOID20")]) %>%
  sf::st_transform(crs)

grid_contxt_df <- grid_contxt
sf::st_geometry(grid_contxt_df) <- NULL

grid_contxt$id_local <- 1:nrow(grid_contxt)

# Loads osm roads prepared previously.
osm_driving_network <- readRDS(paste0(main_folder, generated.folder.CSI, "osm_driving_network_trial_081826_california.rds"))

# Extracts driving network following Larkin et al., (2017)
os_highway_roads <- c(
  #Major roads were derived from OSM motorways, motorway links, trunks, trunk links, primary and secondary roads and links.
  'motorway', 'motorway_link', 'trunk', 'trunk_link', 'primary', 'secondary', 'primary_link', 'secondary_link')
roads_in <- osm_driving_network[which(osm_driving_network$highway %in% os_highway_roads),]
roads_in <- sf::st_transform(roads_in, crs)

roads_contxt_id <- sapply(sf::st_intersects(roads_in, spatial_context),function(x){length(x)>0})
roads_contxt <- roads_in[roads_contxt_id, ]
rm(roads_in)

# Road infrastructure faf5 data downloaded from faf5 geodatabase - find link in readme
# MSN - nav replacement of - faf5_network <- sf::read_sf(paste0(raw.data.folder, "geometry/", "FAF5Network.gdb"))
faf5_network <- sf::read_sf(paste0(main_folder, "pulledData/", "FAF5Network.gdb"))
faf5_highways <- faf5_network[which(faf5_network$F_Class %in% c(1,2,3)),]
faf5_highways <- faf5_highways %>%
  sf::st_transform(2163)


roads_contxt_id_fhwa <- sapply(sf::st_intersects(faf5_highways, spatial_context),function(x){length(x)>0})
roads_contxt_fhwa <- faf5_highways[roads_contxt_id_fhwa, ]
rm(faf5_highways)

# Running in small groups to make the run time faster. 
### Numbers are specific to 578 sf urban block groups.
num_in_group <- 3
n_groups <- (as.integer((as.integer(nrow(grid_contxt) - (num_in_group * 100)))/num_in_group) + 101)
group_1 <- 1:num_in_group

for(g in 2:(n_groups-1)){
assign(paste0("group_", g), get(paste0("group_", (g-1))) + num_in_group)
}

assign("group_193", as.integer(c(577, 578)))


barrier_sp_units_osm_coll <- data.frame()
for(grp_n in 1:n_groups){
  grp <- get(paste0("group_", grp_n))
  sp_unit_df <- as.data.frame(grid_contxt[grp,])
  sp_unit_df$sp_unit_pos <- grp
  print(sp_unit_df)
  sp_unit_ap <- as.array(unlist(sp_unit_df[,c("sp_unit_pos", "id_local")]))
  dim(sp_unit_ap) <- c(sp_unit_pos = nrow(sp_unit_df), col = 2)
  barrier_sp_units_osm <- multiApply::Apply(list(sp_unit_ap), target_dims  = 'col',
                                            # function estimate_barrier_spatial_units is stored at functions.R
                                            fun = estimate_barrier_spatial_units, ncores = geom_prec_n_cores)$output1
  barrier_sp_units_osm <- t(barrier_sp_units_osm)
  colnames(barrier_sp_units_osm) <- c("id_local", "n_contxt_sp_units_osm",  "barrier_factor_osm") # "barrier_sp_units",
  barrier_sp_units_osm_coll <- rbind(barrier_sp_units_osm_coll, barrier_sp_units_osm)
  if (grp_n < n_groups){
    saveRDS(barrier_sp_units_osm_coll, paste0(main_folder, generated.folder.CSI, "barrier_sp_units_osm_partial_", name_short, ".rds"))
  } else {
    saveRDS(barrier_sp_units_osm_coll, paste0(main_folder, generated.folder.CSI, "barrier_sp_units_osm_all_", name_short, ".rds"))
  }
  rm(barrier_sp_units_osm, sp_unit_ap)
  print(paste0("group ", grp_n, " done."))
}
rm(roads_contxt)
# Run for highways and principal arterials from FHWA
roads_contxt <- roads_contxt_fhwa
barrier_sp_units_fhwa_coll <- data.frame()
for(grp_n in 1:n_groups){
  grp <- get(paste0("group_", grp_n))
  sp_unit_df <- as.data.frame(grid_contxt[grp,])
  sp_unit_df$sp_unit_pos <- grp
  sp_unit_ap <- as.array(unlist(sp_unit_df[,c("sp_unit_pos", "id_local")]))
  dim(sp_unit_ap) <- c(sp_unit_pos = nrow(sp_unit_df), col = 2)
  barrier_sp_units_fhwa <- multiApply::Apply(list(sp_unit_ap), target_dims  = 'col',
                                            # function estimate_barrier_spatial_units is stored at functions.R
                                            fun = estimate_barrier_spatial_units, ncores = geom_prec_n_cores)$output1
  barrier_sp_units_fhwa <- t(barrier_sp_units_fhwa)
  colnames(barrier_sp_units_fhwa) <- c("id_local", "n_contxt_sp_units_fhwa",  "barrier_factor_fhwa") # "barrier_sp_units",
  barrier_sp_units_fhwa_coll <- rbind(barrier_sp_units_fhwa_coll, barrier_sp_units_fhwa)
  if (grp_n < n_groups){
    saveRDS(barrier_sp_units_fhwa_coll, paste0(main_folder, generated.folder.CSI, "barrier_sp_units_fhwa_partial_", name_short, ".rds"))
  } else {
    saveRDS(barrier_sp_units_fhwa_coll, paste0(main_folder, generated.folder.CSI, "barrier_sp_units_fhwa_all_", name_short, ".rds"))
  }
  rm(barrier_sp_units_fhwa, sp_unit_ap)
  print(paste0("group ", grp_n, " done."))
} 

barrier_sp_units_osm <- readRDS(paste0(main_folder, generated.folder.CSI, "barrier_sp_units_osm_all_", name_short, ".rds")) # these are generated using OSM
barrier_sp_units_fhwa <- readRDS(paste0(main_folder, generated.folder.CSI, "barrier_sp_units_fhwa_all_", name_short, ".rds")) # these are generated using FHWA
barrier_sp_units_all <- dplyr::left_join(barrier_sp_units_osm, barrier_sp_units_fhwa, by = "id_local", copy = T)
grid_contxt <- dplyr::left_join(grid_contxt, barrier_sp_units_all, by = "id_local", copy = T)
grid_contxt_df <- grid_contxt
sf::st_geometry(grid_contxt_df) <- NULL
saveRDS(grid_contxt_df, paste0(main_folder, generated.folder.CSI, "barrier_sp_units_", name_short,"_for_use.rds"))

# Version used in CSI
Used_barrier <- readRDS(paste0(main_folder, "CSI/Data Test/","barrier_sp_units_", name_short,"_for_use.rds"))
