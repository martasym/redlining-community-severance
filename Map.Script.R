# This script creates a map of the CSI and the 1940 socio-demographic variables 
###- population density and "non-white" population.
# Load necessary R packages

library(sf)
library(dplyr)
library(ggplot2) 
library("patchwork") 

final_sf_data <- readRDS(
  paste0(generated.data.folder, "HOLC.CSI.1940Census_", "070826", "_dta_us.rds")
)

data <- final_sf_data
census1940_HOLC_CSI <- data
census1940_HOLC_CSI$population_density_1940 <- as.numeric(census1940_HOLC_CSI$Tot_Pop)
census1940_HOLC_CSI$population_density_1940 <- census1940_HOLC_CSI$population_density_1940*1000


# The following represent graphs from the three data sources combined into this data set.
Com.Sev.Index_Graphic <- ggplot() +
  geom_sf(data = census1940_HOLC_CSI, aes(fill = Com_Severance_Index)) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(title = "a.  Community Severance Index", fill = "Index")

Pop.Density_Grapic <- ggplot() +
  geom_sf(data = census1940_HOLC_CSI, aes(fill = population_density_1940)) +
  scale_fill_viridis_c(trans = "log10", labels = scales::label_comma(), breaks = scales::breaks_log(n = 5), option = "magma", direction = -1) +
  labs(title = "b.  1940 Population Density",subtitle = "(Persons Per Square Kilometer)", fill = "Density \n (x1,000)")

Prop.Non.White_Grapic <- ggplot() +
  geom_sf(data = census1940_HOLC_CSI, aes(fill = prop_non_white)) +
  scale_fill_viridis_c(trans = "log10", labels = scales::percent, option = "magma", direction = -1,na.value = "#FFFDE7") +
  labs(title = "c.  1940 Proportion 'Non-White' Indivisuals", fill = "Proportion")

Com.Sev.Index_Graphic + Pop.Density_Grapic + Prop.Non.White_Grapic

rm(Com.Sev.Index_Graphic,HOLC_Grapic, Pop.Density_Grapic, Prop.Non.White_Grapic)
rm(spatial_context, sf_census_gis, sf_block_groups, rrmc_results,rrmc_grid, pcp_outs)
rm(census_dta,census_gis, sf_census_dta, config, built_social_block_comm_sev_m)
rm(cng_comm_sev_vars, cng)
rm(census2019_sf_prop)
rm(bg2019_census1940_overlap)
rm(HOLC_CSI, bg_no_overlap_2019ids, census2019_sf_bg, index_join, bg_overlap_50pct)

