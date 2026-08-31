# Sensitivity Analysis Code
# The following code assigns a holc grade to each block group.  

# A) Download the packages to be used in the script
# The following packages are needed to run this code (only need to install once)
install.packages("sf")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")

library("sf")
library("dplyr")
library("ggplot2")
library("tidyr")
library("patchwork") 

# A) Replace the main_folder with wherever you put the shared folder.
main_folder <-"/Users/martasymkowick/CommunitySeverance/code_MS/Finals/"

# B) First, we ensure that we have the correct spatial context for the boarder of San Francisco. 
  # Data is downloaded from TIGER shapefiles from ARC GIS https://www.arcgis.com/home/item.html?id=15e88533ab3c488a853ed32a438ad4c4
    # Data is narrowed down to the San Francisco region through the flipco selector value of 075, cast into a readable geometry,
      # and then transformed into the correct geometry coordinate system.
crs <- 2163
sf_county_shapes <- sf::read_sf(paste0(main_folder, "pulledData/bayArea/","region_county_clp.shp"))
sf_county_subset <- sf_county_shapes[which(sf_county_shapes$fipco == "075"),] 
sf_county_geometry <- sf_county_subset$geometry
sf_county_polygon <- st_cast(sf_county_geometry, "POLYGON")
sf_county_polygon <- sf_county_polygon[6]
sf_county_polygon %>%
  sf::st_transform(crs)
spatial_context <- sf_county_polygon %>%
  sf::st_transform(crs)

    # The spatial context can be mapped/ checked through the following graph:
poly_graph <- ggplot() +
  geom_sf(data = spatial_context, color = "blue", fill = NA)
poly_graph

# C) Second, we have to pull the San Francisco block groups from 2019, which should line up with the Community Severance Data set
  # the block groups come from the U.S. Census (last updated according to website 2021) https://catalog.data.gov/dataset/tiger-line-shapefile-2019-state-california-current-block-group-state-based
    # I accessed it in 2024 (during the Biden Administration)
census2019_sf_bg <- sf::read_sf(paste0(main_folder, "pulledData/tl_2019_06_bg/", "tl_2019_06_bg.shp"))
census2019_sf_bg <- census2019_sf_bg %>%
  sf::st_transform(crs)
    # After downloading for all of CA, we see where it intersects with the spatial context.
census2019_sf_bg_cntxt <- sapply(sf::st_intersects(census2019_sf_bg, spatial_context),function(x){length(x)>0})
census2019_sf_bg <- census2019_sf_bg[census2019_sf_bg_cntxt, ]
    # Then, we cut it down to see the intersection (cuts off/ cleans up the block groups that go into the water).
census2019_sf_bg <- sf::st_intersection(census2019_sf_bg, spatial_context)
census2019_sf_bg <- subset(census2019_sf_bg, select = c(GEOID, geometry))
census2019_sf_bg <- subset(census2019_sf_bg, !(census2019_sf_bg$GEOID == "060816009001"))
census2019_sf_bg <- subset(census2019_sf_bg, !(census2019_sf_bg$GEOID == "060816009002"))
census2019_sf_bg <- subset(census2019_sf_bg, !(census2019_sf_bg$GEOID == "060816007002"))
census2019_sf_bg <- subset(census2019_sf_bg, !(census2019_sf_bg$GEOID == "060750179021"))

# Then, join the block groups with the community severance index.
  # I call this dataset the index_join (also found in the other R-file)
    # Next, for future use, we cut down the the census2019_sf_bg so that its just GEOID and the geometry.
colnames(census2019_sf_bg)[which(names(census2019_sf_bg) == "GEOID")] <- "GEOID20"
label_geom_list <- list("GEOID20", "GEOID20")
label_geom <- census2019_sf_bg[,which(colnames(census2019_sf_bg) %in% label_geom_list)]


# join the GEOID + the community_severance_scores
generated.data.folder <- "CSI/Generated/"
name_short <- "sf"
community_severance_scores <- readRDS(paste0(main_folder, generated.folder.CSI, "comm_sev_fa_scores_SF_", name_short, "_dta_us.rds"))

# Used in CSI
###generated.data.folder <- "CSI/Data Test/"
###community_severance_scores <- readRDS(paste0(main_folder, generated.data.folder, "comm_sev_fa_scores_SF_081826_eta011_rank2_sf", "_dta_us.rds"))
index_join <- dplyr::left_join(label_geom, community_severance_scores, by = "GEOID20")
colnames(index_join)[which(names(index_join) == "MR1_norm")] <- "Com_Severance_Index"
    #The following is a graph to test the sf block groups
census2019_sf_bg_b <- ggplot() +
  geom_sf(data = census2019_sf_bg, fill = NA)
census2019_sf_bg_b


# D) Third, we have to download the redlining data from the University of Richmond's data set https://dsl.richmond.edu/panorama/redlining/data
redlining.data.folder = "redlining/"
holc_redlining_national <- sf::read_sf(paste0(main_folder, redlining.data.folder, "HOLC_USA.shp"))
    # The data is then put into the correct coordinate system
      # Then the area surrounding the city (mostly water) is cut.
crs <- 2163
holc_redlining_national <- holc_redlining_national %>%
  sf::st_transform(crs)
redlining_dta_sf <- sf::st_intersection(holc_redlining_national, census2019_sf_bg)  
rm(holc_redlining_national)
    # The redlining data can be seen in comparison to the block groups in the map below.
redlining_dta_sf_a <- ggplot() +
  geom_sf(data = redlining_dta_sf, aes(fill = holc_grade), color = "blue") +
  scale_fill_manual(values=c("darkgreen", "lightblue","lightyellow", "darkred")) + 
  labs(title = "a.  HOLC Grade and 2019 Block Groups", fill = "Grade") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  geom_sf(data = census2019_sf_bg, fill = NA)
redlining_dta_sf_a

# At this point we have made the following data-sets/ variables:
    # A) spatial_context -> holds the boarder for SF
    # B) index_join -> community severance index with the geom of block groups
    # C) redlining_dta_sf -> redlining data for SF
    # D) census2019_sf_bg -> the geom data for the block groups in SF


# Using the collected data, we will now assign the block groups a relative grade or no grade.
  # We will do so as follows.

# 1) Add a new column to index trial (community severance index)
index_join$HOLC_grade <- NA
# 2) We will now use redlining_dta_sf. 
  # This data set will be used to evaluate the proportion grade in each block group.
    # Note that redlining_dta_sf is chopped up into smaller pieces.
# Now, we will sort the remaining pieces and join them by GEOID. 
  # This leaves fewer block groups joined by shared geo-spatial data.
  # We will call this group, redlining_sf_comp
redlining_dta_sf <- redlining_dta_sf[order(redlining_dta_sf$GEOID20),]
redlining_sf_comp <- redlining_dta_sf %>% 
  group_by(GEOID20) %>%
  dplyr::summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_as_sf()
  # This map demonstrates these re-joined lined areas.
test <- ggplot() +
  geom_sf(data = redlining_dta_sf, aes(fill = holc_grade)) +
  #geom_sf(data = faf5_highways_loc, color = "blue", fill = NA) + 
  scale_fill_manual(values=c("darkgreen", "lightblue","lightyellow", "darkred")) + 
  geom_sf(data = redlining_sf_comp, fill = NA, color = "blue")  #+ 
  #geom_sf(data = prep_no_grade_sf, color = "blue") +
  #scale_fill_viridis_c(option = "magma", direction = -1)
test
# remove unnecessary variables

# 3) Now, we have a data set, containing all the redlined areas, grouped by GEOID. 
  # We will use this data set, to create a new data set, census2019_sf_bg_holc_overlap.
  # This new data set, census2019_sf_bg_holc_overlap, will contain every block group that contains >30% lined areas.
      # The function works as follows:
          # A) We go through every block group GEOID and if it is listed in those within the lined areas,
          # B) It calculates the percentage of the area that is within the redlined area. 
          # C) If it is greater than 30 percent, the block group is added to the data set.

census2019_sf_bg_holc_overlap <- census2019_sf_bg[0,]
for(i in 1:nrow(census2019_sf_bg)){
  if(census2019_sf_bg$GEOID20[i] %in% redlining_sf_comp$GEOID20){
    val <- which(redlining_sf_comp$GEOID20 == census2019_sf_bg$GEOID20[i])
    percentage = st_area(redlining_sf_comp$geometry[val])/st_area(census2019_sf_bg$geometry[i])
    if (as.numeric(percentage) > 0.3) {
      census2019_sf_bg_holc_overlap <- rbind(census2019_sf_bg_holc_overlap, census2019_sf_bg[i,])
    }
  }
}
  # The results are mapped here.
red_sfblock_groups_a <- ggplot() +
  geom_sf(data = redlining_dta_sf, aes(fill = holc_grade)) +
  #geom_sf(data = faf5_highways_loc, color = "blue", fill = NA) + 
  scale_fill_manual(values=c("darkgreen", "lightblue","lightyellow", "darkred")) + 
  geom_sf(data = census2019_sf_bg_holc_overlap, color = "yellow", fill = NA)
#scale_fill_viridis_c(option = "magma", direction = -1)
red_sfblock_groups_a
# remove unnecessary variables


# 4) Now, we know that those block groups that <30% redlined area are not in census2019_sf_bg_holc_overlap. 
    # We will now label these in the index_join (community severance index)
        # We will give those not found within the census2019_sf_bg_holc_overlap data set an unlined grade.
index_join$HOLC_grade <- NA
for (i in 1:nrow(index_join)){
  if (index_join$GEOID20[i] %in% census2019_sf_bg_holc_overlap$GEOID20 == FALSE){
    index_join$HOLC_grade[i] <- "not_lined"
  }
}

# 5) Then, for each block group, if one grade makes up over 30% of the block group, 
  # It is assigned the grade of the block group that makes up most of the area.
  # This is done through the following for loop:
    # The percentages for each holc grade are stored in a column in data set census2019_sf_bg_holc_overlap 
    # and then the largest percentage over 30 is stored in the final grade column in census2019_sf_bg_holc_overlap.
    # If one grade does not take up more than 30 percent, it is assigned the grade "within 0.1?"
      # There are only 4 block groups that fall under this designation-- which I address later.

# change the code to be a data set of percentages
census2019_sf_bg_holc_overlap$percent_A <- 0
census2019_sf_bg_holc_overlap$percent_B <- 0
census2019_sf_bg_holc_overlap$percent_C <- 0
census2019_sf_bg_holc_overlap$percent_D <- 0
census2019_sf_bg_holc_overlap$grade <- 0

for(i in 1:nrow(census2019_sf_bg_holc_overlap)){
  grade_trial <- sf::st_intersection(redlining_dta_sf, census2019_sf_bg_holc_overlap[i,]) 
  grade_trial <- grade_trial %>% 
    group_by(holc_grade) %>%
    summarise(geometry = sf::st_union(geometry))
  
  # finds the percentage of each A, B, C, D grade
  val_A <- which(grepl("A", grade_trial$holc_grade))
  if (!is.na(val_A[1])){
    percentage_A = st_area(grade_trial$geometry[val_A])/st_area(census2019_sf_bg_holc_overlap$geometry[i])
    census2019_sf_bg_holc_overlap$percent_A[i] = percentage_A
  }
  val_B <- which(grepl("B", grade_trial$holc_grade))
  if (!is.na(val_B[1])){
    percentage_B = st_area(grade_trial$geometry[val_B])/st_area(census2019_sf_bg_holc_overlap$geometry[i])
    census2019_sf_bg_holc_overlap$percent_B[i] = percentage_B
  }
  val_C <- which(grepl("C", grade_trial$holc_grade))
  if (!is.na(val_C[1])){
    percentage_C = st_area(grade_trial$geometry[val_C])/st_area(census2019_sf_bg_holc_overlap$geometry[i])
    census2019_sf_bg_holc_overlap$percent_C[i] = percentage_C
  }
  val_D <- which(grepl("D", grade_trial$holc_grade))
  if (!is.na(val_D[1])){
  percentage_D = st_area(grade_trial$geometry[val_D])/st_area(census2019_sf_bg_holc_overlap$geometry[i])
  census2019_sf_bg_holc_overlap$percent_D[i] = percentage_D
  }
}

# If the percentage of any grade area is greater than 30, it assigns that block group the largest area.
#   otherwise, it assigns the value "within 0.1" 

for(i in 1:nrow(census2019_sf_bg_holc_overlap)){
  if(census2019_sf_bg_holc_overlap$percent_A[i] > 0.3 || census2019_sf_bg_holc_overlap$percent_B[i] > 0.3 
     ||  census2019_sf_bg_holc_overlap$percent_C[i] > 0.3 ||  census2019_sf_bg_holc_overlap$percent_D[i] > 0.3){
    if (census2019_sf_bg_holc_overlap$percent_A[i] > census2019_sf_bg_holc_overlap$percent_B[i] && 
        census2019_sf_bg_holc_overlap$percent_A[i] > census2019_sf_bg_holc_overlap$percent_C[i]  && 
        census2019_sf_bg_holc_overlap$percent_A[i] > census2019_sf_bg_holc_overlap$percent_D[i]) {
      if ((census2019_sf_bg_holc_overlap$percent_A[i] - census2019_sf_bg_holc_overlap$percent_B[i]) < 0.1 ||
          (census2019_sf_bg_holc_overlap$percent_A[i] - census2019_sf_bg_holc_overlap$percent_C[i]) < 0.1 ||
          (census2019_sf_bg_holc_overlap$percent_A[i] - census2019_sf_bg_holc_overlap$percent_D[i]) < 0.1){
        census2019_sf_bg_holc_overlap$grade[i] <- "within 0.1"
      }else{
        
        census2019_sf_bg_holc_overlap$grade[i] = "A"
      }
    } else if (census2019_sf_bg_holc_overlap$percent_B[i] > census2019_sf_bg_holc_overlap$percent_A[i] && 
               census2019_sf_bg_holc_overlap$percent_B[i] > census2019_sf_bg_holc_overlap$percent_C[i]  && 
               census2019_sf_bg_holc_overlap$percent_B[i] > census2019_sf_bg_holc_overlap$percent_D[i]){
      if ((census2019_sf_bg_holc_overlap$percent_B[i] - census2019_sf_bg_holc_overlap$percent_A[i]) < 0.1 ||
          (census2019_sf_bg_holc_overlap$percent_B[i] - census2019_sf_bg_holc_overlap$percent_C[i]) < 0.1 ||
          (census2019_sf_bg_holc_overlap$percent_B[i] - census2019_sf_bg_holc_overlap$percent_D[i]) < 0.1){
        
        census2019_sf_bg_holc_overlap$grade[i] <- "within 0.1"
      }else{
        census2019_sf_bg_holc_overlap$grade[i] = "B"
      }
    } else if (census2019_sf_bg_holc_overlap$percent_C[i] > census2019_sf_bg_holc_overlap$percent_A[i] && 
               census2019_sf_bg_holc_overlap$percent_C[i] > census2019_sf_bg_holc_overlap$percent_B[i]  && 
               census2019_sf_bg_holc_overlap$percent_C[i] > census2019_sf_bg_holc_overlap$percent_D[i]){
      if ((census2019_sf_bg_holc_overlap$percent_C[i] - census2019_sf_bg_holc_overlap$percent_B[i]) < 0.1 ||
          (census2019_sf_bg_holc_overlap$percent_C[i] - census2019_sf_bg_holc_overlap$percent_A[i]) < 0.1 ||
          (census2019_sf_bg_holc_overlap$percent_C[i] - census2019_sf_bg_holc_overlap$percent_D[i]) < 0.1){
        census2019_sf_bg_holc_overlap$grade[i] <- "within 0.1"
      }else{
        census2019_sf_bg_holc_overlap$grade[i] = "C"
      }
    } else {
      if ((census2019_sf_bg_holc_overlap$percent_D[i] - census2019_sf_bg_holc_overlap$percent_B[i]) < 0.1 ||
          (census2019_sf_bg_holc_overlap$percent_D[i] - census2019_sf_bg_holc_overlap$percent_C[i]) < 0.1 ||
          (census2019_sf_bg_holc_overlap$percent_D[i] - census2019_sf_bg_holc_overlap$percent_A[i]) < 0.1){
        census2019_sf_bg_holc_overlap$grade[i] <- "within 0.1"
      }else{
        census2019_sf_bg_holc_overlap$grade[i] = "D"
      }
    }
  }else{
    census2019_sf_bg_holc_overlap$grade[i] <- "less_than_30"
  }
}


# 6) For the sensitivity analysis, then those block groups that are in 0.1 of 
### the group are assigned the second largest overlap.

for(i in 1:nrow(census2019_sf_bg_holc_overlap)){
  if(census2019_sf_bg_holc_overlap$grade[i] == "within 0.1"){
    # need to assign to the second highest value
    vals <- vector(mode = "numeric", length = 4)
    vals[1] <- census2019_sf_bg_holc_overlap$percent_A[i]
    vals[2] <- census2019_sf_bg_holc_overlap$percent_B[i]
    vals[3] <- census2019_sf_bg_holc_overlap$percent_C[i]
    vals[4] <- census2019_sf_bg_holc_overlap$percent_D[i]
    vals <- sort(vals, decreasing = TRUE)
    if(vals[2] == census2019_sf_bg_holc_overlap$percent_A[i]){
      census2019_sf_bg_holc_overlap$grade[i] <- "A"
    } else if (vals[2] == census2019_sf_bg_holc_overlap$percent_B[i]){
      census2019_sf_bg_holc_overlap$grade[i] <- "B"
    } else if (vals[2] == census2019_sf_bg_holc_overlap$percent_C[i]){
      census2019_sf_bg_holc_overlap$grade[i] <- "C"
    } else{
      census2019_sf_bg_holc_overlap$grade[i] <- "D"
    }
  }
}

# 7) The holc grades for those block groups found in the census2019_sf_bg_holc_overlap data set
      # are added to the index trial data set (community severance index with geom data)
for (i in 1:nrow(index_join)){
  if (index_join$GEOID20[i] %in% census2019_sf_bg_holc_overlap$GEOID20){
    val <- which(grepl(index_join$GEOID20[i], census2019_sf_bg_holc_overlap$GEOID20))
    index_join$HOLC_grade[i] <- census2019_sf_bg_holc_overlap$grade[val]
  }
}

for (i in 1:nrow(index_join)){
  if (index_join$HOLC_grade[i] == "less_than_30"){
    index_join$HOLC_grade[i] <- "not_lined"
  }
}

# This map contains the original redlining data, and outlines the block groups 
    # with their assigned designation by outlined color-- grade A = green, grade B = blue,
      # grade C = light yellow, grade D = red, less than 0.3 = purple.
red_sfblock_groups_e <- ggplot() +
  geom_sf(data = index_join, aes(fill = HOLC_grade)) +
  labs(title = "b.  Assigned HOLC Grades", fill = "Grade") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  scale_fill_manual(values=c("darkgreen", "lightblue","lightyellow", "darkred", NA))
red_sfblock_groups_e

# Overlapping graphs demonstrating the assignment of HOLC grades to 2019 block groups
redlining_dta_sf_a + red_sfblock_groups_e


# remove unnecessary variables
rm(sf_county_shapes, sf_county_geometry, sf_county_polygon, poly_graph, sf_county_subset)
rm(census2019_sf_bg_cntxt, label_geom, label_geom_list, community_severance_scores, census2019_sf_bg_b)
rm(redlining_dta_sf_a)
rm(test)
rm(red_sfblock_groups_a, val, percentage)
rm(grade_trial)
rm(redlining_dta_sf, redlining_sf_comp, red_sfblock_groups_a)
rm(census2019_sf_bg_holc_overlap)
rm(red_sfblock_groups_e)