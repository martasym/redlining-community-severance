
# Sets coordinate reference system
crs <- 2163
cbsa_of_interest <- "San Francisco, CA"
name_short <- "sf"

main_folder <- "/Users/martasymkowick/CommunitySeverance/code_MS/Finals/"
generated.folder.CSI <- "CSI/Generated/"

# Defines spatial context. Produced by the Metropolitan Transportation Commission: https://www.arcgis.com/home/item.html?id=15e88533ab3c488a853ed32a438ad4c4#overview 
SFgeom <- sf::read_sf(paste0(main_folder,"pulledData/", "bayArea/","region_county_clp.shp"))
sf <- SFgeom[which(SFgeom$fipco == "075"),] %>%
  sf::st_transform(crs)
sfpoly <- st_cast(sf, "POLYGON")
sfpoly <- sfpoly[6,] 
sfpoly %>%
  sf::st_transform(crs)

spatial_context <- sfpoly

# Smart location database (you can download it here https://www.epa.gov/smartgrowth/smart-location-mapping#SLD)
sld_us <- sf::read_sf(paste0(main_folder, "pulledData/SmartLocationDatabaseV3/", "SmartLocationDatabase.gdb"))

sld_us <- sld_us %>%
  sf::st_transform(crs)
sld_us_id_cntxt <- sapply(sf::st_intersects(sld_us, spatial_context),function(x){length(x)>0})
sld_us_loc <- sld_us[sld_us_id_cntxt, ]
# Removes of three San Mateo block groups and Treasure Island block group
sld_us_loc <- subset(sld_us_loc, !(sld_us_loc$GEOID10 == "060816009001"))
sld_us_loc <- subset(sld_us_loc, !(sld_us_loc$GEOID10 == "060816009002"))
sld_us_loc <- subset(sld_us_loc, !(sld_us_loc$GEOID10 == "060816007002"))
sld_us_loc <- subset(sld_us_loc, !(sld_us_loc$GEOID10 == "060750179021"))
sld_us_loc_df <- sld_us_loc
sf::st_geometry(sld_us_loc_df) <- NULL

# Removes bodies of water.
if(any(which(sld_us_loc_df$Ac_Water/sld_us_loc_df$Ac_Total == 1))){
  sld_us_loc <- sld_us_loc[-which(sld_us_loc_df$Ac_Water/sld_us_loc_df$Ac_Total == 1),]
}

# Creates a context grid specific to SF
grid_contxt <- sf::st_centroid(sld_us_loc[,c("GEOID20")]) 
grid_contxt_df <- grid_contxt
sf::st_geometry(grid_contxt_df) <- NULL

# Calls the road infrastructure from faf 5 model - Freight Analysis Framework 5.0 Model Network Database.
### Can be downloaded here: https://geodata.bts.gov/datasets/9343414b46794fb8be9867db2d1ccb75/about
faf5_network <- sf::read_sf(paste0(main_folder, "pulledData/", "FAF5Network.gdb"))
faf5_highways <- faf5_network[which(faf5_network$F_Class %in% c(1,2,3)),] # Interstate, Principal Arterial - Other Freeways and Expressways, and Principal Arterial - Other)
faf5_highways <- faf5_highways %>%
  sf::st_transform(crs)

osm_area = "california"
osm_driving_network <- readRDS(paste0(main_folder, generated.folder.CSI, "osm_driving_network_trial_081826_", osm_area, ".rds"))

# extract driving network following Larkin et al., (2017) Major roads were derived from OSM motorways, motorway links, trunks, trunk links, primary and secondary roads and links
os_highway_roads <- c(
  #Major roads were derived from OSM motorways, motorway links, trunks, trunk links, primary and secondary roads and links.
  'motorway', 'motorway_link', 'trunk', 'trunk_link', 'primary', 'secondary', 'primary_link', 'secondary_link',
  #Minor roads were derived from OSM tertiary roads and tertiary road links.
  'tertiary', 'tertiary_link', 'unclassified',
  #Residential roads were derived from OSM residential roads and residential road links
  'residential'
)
roads_us <- osm_driving_network[which(osm_driving_network$highway %in% os_highway_roads),]
roads_us <- sf::st_transform(roads_us, crs)

roads_us_loc <- roads_us[spatial_context,]
rm(osm_driving_network, roads_us)

# Stores the road categories and road types.
cs_category <- c("motorway", "motorway", 'trunk', 'trunk', "primary", "primary", "secondary", "secondary", "tertiary", "tertiary", "residential", "residential", "residential", "residential")
osm_category <- c("motorway", "motorway_link", 'trunk', 'trunk_link', "primary", "primary_link", "secondary", "secondary_link", 'tertiary', 'tertiary_link', "residential", "residential_link", 'unclassified', 'unclassified_link')
road_types <- data.frame(cs_category = cs_category, osm_category = osm_category)

# cats --> unique terms in each list
cats <- unique(road_types$cs_category)

for(c in 1:length(cats)){
cat <- cats[c]
osm_types <- road_types[which(road_types$cs_category == cat), "osm_category"]
roads_us_loc_cat <- roads_us_loc[which(roads_us_loc$highway %in% osm_types),]
if(nrow(roads_us_loc_cat) > 0){
  roads_us_loc_cat_id_cntxt <- sapply(sf::st_intersects(roads_us_loc_cat, spatial_context),function(x){length(x)>0})
  roads_us_loc_cat_test <- roads_us_loc_cat[roads_us_loc_cat_id_cntxt, ]
if(c == 6){
  cut <- round((nrow(grid_contxt)/4),0)
  dist_1 <- sf::st_distance(grid_contxt[1:cut,], roads_us_loc_cat_test)
  dist_1 <- apply(dist_1, 1, FUN = min, na.rm = TRUE)
  dist_2 <- sf::st_distance(grid_contxt[(cut+1):(2*cut),], roads_us_loc_cat_test)
  dist_2 <- apply(dist_2, 1, FUN = min, na.rm = TRUE)
  dist_3 <- sf::st_distance(grid_contxt[((2*cut)+1):(3*cut),], roads_us_loc_cat_test)
  dist_3 <- apply(dist_3, 1, FUN = min, na.rm = TRUE)
  dist_4 <- sf::st_distance(grid_contxt[((3*cut)+1):nrow(grid_contxt),], roads_us_loc_cat_test)
  dist_4 <- apply(dist_4, 1, FUN = min, na.rm = TRUE)
  dist <- c(dist_1, dist_2, dist_3, dist_4)
  var_name <- paste0(cat, "_dist")
  grid_contxt[,var_name] <- dist
} else {
  dist <- sf::st_distance(grid_contxt, roads_us_loc_cat_test)
  var_name <- paste0(cat, "_dist")
  grid_contxt[,var_name] <- apply(dist, 1, FUN = min, na.rm = TRUE)
}
rm(roads_us_loc_cat_id_cntxt, roads_us_loc_cat_test, dist)
} else {
grid_contxt[,var_name] <- NA
}
rm(roads_us_loc_cat)
}

saveRDS(grid_contxt, paste0(main_folder, generated.folder.CSI, "road_inf_dist_2_grid_sld_osm_temp_", name_short,".rds"))


# distance to major roads according to faf 5
# FHWA highway functional class designation:
#   1 - Interstate
# 2 - Principal Arterial - Other Freeways and Expressways
# 3 - Principal Arterial - Other

cs_category <- c("interstate_highway", "freeways_expressways", 'other_princ_arter')
fhwa_category <- c(1, 2, 3)
road_types <- data.frame(cs_category = cs_category, fhwa_category = fhwa_category)
cats <- unique(road_types$cs_category)
for(c in 1:length(cats)){
  cat <- cats[c]
  fhwa_types <- road_types[which(road_types$cs_category == cat), "fhwa_category"]
  roads_loc <- faf5_highways[which(faf5_highways$F_Class %in% fhwa_types),]
  if(nrow(roads_loc) > 0){
    roads_loc_id_cntxt <- sapply(sf::st_intersects(roads_loc, spatial_context),function(x){length(x)>0})
    roads_loc_test <- roads_loc[roads_loc_id_cntxt, ]
    
    dist <- sf::st_distance(grid_contxt, roads_loc_test)
    var_name <- paste0(cat, "_dist")
    grid_contxt[,var_name] <- apply(dist, 1, FUN = min, na.rm = TRUE)
    rm(roads_loc_id_cntxt, roads_loc_test, dist)
  } else {
    grid_contxt[,var_name] <- NA
  }
  rm(roads_loc)
}

max_motorway <- max(grid_contxt$motorway_dist) 
grid_contxt$motorway_prox <- (max_motorway - grid_contxt$motorway_dist) / max_motorway

max_primary <- max(grid_contxt$primary_dist) 
grid_contxt$primary_prox <- (max_primary - grid_contxt$primary_dist) / max_primary

max_secondary <- max(grid_contxt$secondary_dist) 
grid_contxt$secondary_prox <- (max_secondary - grid_contxt$secondary_dist) / max_secondary

max_tertiary <- max(grid_contxt$tertiary_dist) 
grid_contxt$tertiary_prox <- (max_tertiary - grid_contxt$tertiary_dist) / max_tertiary

max_residential <- max(grid_contxt$residential_dist) 
grid_contxt$residential_prox <- (max_residential - grid_contxt$residential_dist) / max_residential

max_trunk <- max(grid_contxt$trunk_dist) 
grid_contxt$trunk_prox <- (max_trunk - grid_contxt$trunk_dist) / max_trunk

max_interstate_highway <- max(grid_contxt$interstate_highway_dist) 
grid_contxt$interstate_highway_prox <- (max_interstate_highway - grid_contxt$interstate_highway_dist) / max_interstate_highway

max_freeways_expressways <- max(grid_contxt$freeways_expressways_dist) 
grid_contxt$freeways_expressways_prox <- (max_freeways_expressways - grid_contxt$freeways_expressways_dist) / max_freeways_expressways

max_other_princ_arter <- max(grid_contxt$other_princ_arter_dist)
grid_contxt$other_princ_arter_prox <- (max_other_princ_arter - grid_contxt$other_princ_arter_dist) / max_other_princ_arter

grid_contxt <- grid_contxt[,-which(colnames(grid_contxt) %in% c("motorway_dist", "primary_dist", "secondary_dist", "tertiary_dist", "residential_dist", "trunk_dist",
                                                             "interstate_highway_dist", "freeways_expressways_dist", "other_princ_arter_dist"))]

saveRDS(grid_contxt, paste0(main_folder, generated.folder.CSI, "road_inf_dist_2_grid_sld_", name_short,".rds"))
