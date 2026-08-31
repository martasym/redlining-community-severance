# Sets coordinate reference system.
crs <- 2163

### Sets study area variable names.
cbsa_of_interest <- "San Francisco, CA"
name_short <- "sf"

main_folder <- "/Users/martasymkowick/CommunitySeverance/code_MS/Finals/"
generated.folder.CSI <- "CSI/Generated/"

## Sets road characteristics (make query for osm data)
driving_network_v_t_opts <- c(
  "-where", "
    (highway IS NOT NULL)
    AND
    (highway NOT IN (
    'abandoned', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
    'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
    'proposed', 'cycleway', 'pedestrian', 'bridleway', 'path', 'footway',
    'steps'
    ))
    AND
    (access NOT IN ('private', 'no'))
    AND
    (service NOT ILIKE 'private%')
    "
)
driving_network_ext_tgs <- c("lanes", "maxspeed", "access", "service", "barrier", "surface", "tiger:cfcc", "parking:lane:both", "parking:lane:left", "parking:lane:right")

# Assigns the region OSM to be used; this data needs to be pre-downloaded.
if(cbsa_of_interest == "New York-Newark-Jersey City, NY-NJ-PA"){
  region_osm <- "us-northeast"
} else if (cbsa_of_interest == "Seattle-Tacoma-Bellevue, WA"){
  region_osm <- "us-west"
} else if (cbsa_of_interest == "San Francisco, CA"){
  region_osm <- "california" 
} else if (cbsa_of_interest == "Houston-The Woodlands-Sugar Land, TX"){
  region_osm <- "us-south" 
} else if (cbsa_of_interest == "Chicago-Naperville-Elgin, IL-IN-WI"){
  region_osm <- "us-midwest"
}

#OSM data was pulled from the historical archive here: https://download.geofabrik.de/north-america/us/california.html# 
# (raw directory index - 2019)
pbf = file.path(paste0(main_folder, "CSI/PulledData/","california", "-190101.osm.pbf"))

# road network and parking data call 
osmextract::oe_vectortranslate(
  pbf,
  layer = "lines",
  vectortranslate_options = driving_network_v_t_opts,
  osmconf_ini = NULL,
  extra_tags = driving_network_ext_tgs,
  force_vectortranslate = TRUE,
  never_skip_vectortranslate = FALSE,
  boundary = NULL,
  boundary_type = c("spat", "clipsrc"),
  quiet = FALSE
)
# storing the just saved file in a known path and delete unused variables
osm_driving_network <- osmextract::oe_read(main_folder, "/CSI/PulledData/california-190101.gpkg")
unused_vars_ind <- which(colnames(osm_driving_network) %in% c("waterway", "aerialway", "man_made"))
osm_driving_network <- osm_driving_network[,-unused_vars_ind]

# Saves the file; make sure to change date before saving.
saveRDS(osm_driving_network, paste0(main_folder, generated.folder.CSI, "osm_driving_network_trial_082026_", region_osm, ".rds"))

# Version used in CSI
Used_OSM2019 <- readRDS(paste0(main_folder, "CSI/Data Test/osm_driving_network_trial_081826_california.rds"))
