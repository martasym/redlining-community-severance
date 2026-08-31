# Load necessary R packages

library(sf)
library(dplyr)
library(ggplot2) 
library("patchwork") 

# The purpose of this R document is to assign each 2019 block group the 1940 census tract 
  # values for the tract that takes up the largest area.

## the map demonstrates overlap between two census groups 
census1940_sf %>%
  ggplot(aes(fill = census1940_sf$Tot_Pop)) + 
  geom_sf(color = NA) + 
  geom_sf(data = spatial_context, color = "blue", fill = NA, alpha = 0) +
  geom_sf(data = census2019_sf_bg, color = "yellow", fill = NA) +
  scale_fill_viridis_c(option = "magma", direction = -1)

# 1) Create an intersection data set, representing the smaller spacial pieces 
  # formed through overlap between the 2019 census 
bg2019_census1940_overlap <- sf::st_intersection(census2019_sf_bg, census2019_sf_prop)
bg2019_census1940_overlap <- subset(bg2019_census1940_overlap, select = -c(GISJOIN))
  
  # The intersected data is mapped here.
bg2019_census1940_overlap %>%
  ggplot(aes(fill = NA, alpha = 0)) + 
  geom_sf(color = NA, alpha = 0) +   
  geom_sf(data = bg2019_census1940_overlap, color = "blue", fill = "blue", alpha = 0.5) +
  geom_sf(data = bg2019_census1940_overlap$geometry[453], color = "black", fill = "black" )+
  geom_sf(data = census2019_sf_bg, color = "yellow", fill = NA, alpha = 0)

# 2) Determine how many of the current tiny intersections take up >50 of the 1970 census. 
    # All of the block groups have one tract that makes up >50. 
  
bg_overlap_50pct <- bg2019_census1940_overlap[0,]
percentage_counter <- 0

for(i in 1:nrow(bg2019_census1940_overlap)){
  if(bg2019_census1940_overlap$GEOID20[i] %in% census2019_sf_bg$GEOID20){
    val <- which(census2019_sf_bg$GEOID20 == bg2019_census1940_overlap$GEOID20[i])
    percentage = st_area(bg2019_census1940_overlap$geometry[i])/st_area(census2019_sf_bg$geometry[val])
    if (as.numeric(percentage) > 0.50) {
      bg_overlap_50pct <- rbind(bg_overlap_50pct, bg2019_census1940_overlap[i,])
    }
  }
}

# 3) Prepare the data set with the community severance index and HOLC grade to be 
  #joined to the 1940 census data
HOLC_CSI <- subset(index_join, select = c(GEOID20, geometry, Com_Severance_Index, HOLC_grade))

# 4) Joins the HOLC_CSI data set with the 1940 census data set.
census1940_HOLC_CSI <- dplyr::left_join(HOLC_CSI, sf::st_drop_geometry(bg_overlap_50pct), by = "GEOID20")
census1940_HOLC_CSI$Tot_Pop <- as.numeric(census1940_HOLC_CSI$Tot_Pop)

# 5) Saves the the data in the final data folder.
saveRDS(census1940_HOLC_CSI, paste0(main_folder, generated.data.folder, "HOLC.CSI.1940Census_dta_us.rds"))

rm(spatial_context, sf_census_gis, sf_block_groups, rrmc_results,rrmc_grid, pcp_outs)
rm(census_dta,census_gis, sf_census_dta, config, built_social_block_comm_sev_m)
rm(cng_comm_sev_vars, cng)
rm(census2019_sf_prop)
rm(bg2019_census1940_overlap)
rm(HOLC_CSI, bg_no_overlap_2019ids, census2019_sf_bg, index_join, bg_overlap_50pct)

