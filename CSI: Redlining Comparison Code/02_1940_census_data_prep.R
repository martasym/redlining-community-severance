# This code prepares the 1940 census data to be merged with the CSI/HOLC data.


# The following packages are needed to run this code (only need to install once)
install.packages("sf")
install.packages("dplyr")
install.packages("ggplot2")

library("sf")
library("dplyr")
library("ggplot2")

 # A) First, we ensure that we have the correct spatial context for the boarder of San Francisco. 
# Data is downloaded from TIGER shapefiles from ARC GIS https://www.arcgis.com/home/item.html?id=15e88533ab3c488a853ed32a438ad4c4
# Data is narrowed down to the San Francisco region through the flipco selector value of 075, cast into a readable geometry,
# and then transformed into the correct geometry coordinate system.
 crs <- 2163
sf_county_shapes <- sf::read_sf(paste0(main_folder, "/pulledData/", "bayArea/","region_county_clp.shp"))
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

# B) Second, we have to pull the San Francisco block groups.
# the block groups come from the 1940 U.S. Census from the Individual Public Use 
    #Microdata Series National Historical Geographic Information Systems database
    # (last updated according to website 2025) 
# I accessed it in 2025
us_census_gis <- sf::read_sf(paste0(main_folder, "1940 census data/nhgis0001_shape/nhgis0001_shapefile_tl2008_us_tract_1940/", "US_tract_1940_conflated.shp"))
us_census_gis  <- us_census_gis %>%
  sf::st_transform(crs)

# After downloading for all of CA, we see where it intersects with the spatial context.
census_gis_cntxt <- sapply(sf::st_intersects(us_census_gis, spatial_context),function(x){length(x)>0})
census1940_sf <- us_census_gis[census_gis_cntxt, ]



# Then, we cut it down to see the intersection (cuts off/ cleans up the block groups that go into the water).
census1940_sf <- sf::st_intersection(census1940_sf, spatial_context)
census1940_sf <- subset(census1940_sf, select = c(GISJOIN, geometry))

     #The following is a graph to test the sf block groups (changed to census1940_sf 022426)
census1940_sf_a <- ggplot() +
  geom_sf(data = census1940_sf, fill = NA )
census1940_sf_a


# Third, we have to pull the corresponding table data for those block groups. 
# The table data comes from the same source as the block groups,  
  # the 1940 U.S. Census from the Individual Public Use Microdata Series National Historical Geographic Information Systems database
# (last updated according to website 2025; accessed in 2025)
# The data is uploaded in two steps.
census_tbl <- read.csv(paste0(main_folder, "/1940 census data/nhgis0001_csv/", "nhgis0001_ds76_1940_tract.csv"))
census1940_sf <- dplyr::left_join(census1940_sf, census_tbl, by = "GISJOIN")
#The following is a graph to test the sf block groups with the different data labels
census1940_sf %>%
  ggplot(aes(fill = BUQ001)) + 
  geom_sf(color = NA) + 
  geom_sf(data = spatial_context, color = "blue", fill = NA) +
  scale_fill_viridis_c(option = "magma", direction = -1)

# Fourth, the table data columns need to be labeled appropriately. 

colnames(census1940_sf)[which(names(census1940_sf) == "BUQ001")] <- "white_pop"
colnames(census1940_sf)[which(names(census1940_sf) == "BUQ002")] <- "nonwhite_pop"
colnames(census1940_sf)[which(names(census1940_sf) == "BU5001")] <- "native_white_born_pop"
colnames(census1940_sf)[which(names(census1940_sf) == "BVG001")] <- "Black_pop"
colnames(census1940_sf)[which(names(census1940_sf) == "BVQ001")] <- "person_per_occupied_dwelling_unit"
colnames(census1940_sf)[which(names(census1940_sf) == "BUH006")] <- "male_greater_4yrs_highschool"
colnames(census1940_sf)[which(names(census1940_sf) == "BUH007")] <- "male_college_1to3"
colnames(census1940_sf)[which(names(census1940_sf) == "BUH008")] <- "male_greater_4yrs_college"
colnames(census1940_sf)[which(names(census1940_sf) == "BUH015")] <- "female_greater_4yrs_highschool"
colnames(census1940_sf)[which(names(census1940_sf) == "BUH016")] <- "female_college_1to3"
colnames(census1940_sf)[which(names(census1940_sf) == "BUH017")] <- "female_greater_4yrs_college"
colnames(census1940_sf)[which(names(census1940_sf) == "BVC001")] <- "Median_Home_Value"
colnames(census1940_sf)[which(names(census1940_sf) == "BVM001")] <- "Radio_Ownership"

census1940_sf$MaleHighSchoolEdu <- census1940_sf$male_greater_4yrs_highschool + census1940_sf$male_college_1to3 + census1940_sf$male_greater_4yrs_college
census1940_sf$FemaleHighSchoolEdu <- census1940_sf$female_greater_4yrs_highschool + census1940_sf$female_college_1to3 + census1940_sf$female_greater_4yrs_college
census1940_sf$TotHighSchoolEdu <- census1940_sf$MaleHighSchoolEdu + census1940_sf$FemaleHighSchoolEdu
census1940_sf <- subset(census1940_sf, select = -c( BUH001,BUH002,BUH003, BUH004, BUH005, male_greater_4yrs_highschool, 
                                                   male_college_1to3, male_greater_4yrs_college, BUH009, BUH010, BUH011, BUH012,
                                                         BUH013, BUH014, female_greater_4yrs_highschool, female_college_1to3, 
                                                   female_greater_4yrs_college, BUH018, STATE, COUNTY, YEAR, STATEA, COUNTYA, 
                                                   PRETRACTA, TRACTA,POSTTRCTA, BU7001, BU7002, BU7003, BU7004, BU7005, BU7006, 
                                                   BU7007,BU7008, BU7009, BU7010, BU7011, BVM002, BVM003, MaleHighSchoolEdu, 
                                                   FemaleHighSchoolEdu) )


#second download contains total population, employment number, the total number of occupied dwelling units,
    # and the median rent.
census_tbl_two <- read.csv(paste0(main_folder, "1940 census data/nhgis0002_csv/", "nhgis0002_ds76_1940_tract.csv"))
census_tbl_two <- subset(census_tbl_two, select = -c(STATE, COUNTY, YEAR, STATEA, COUNTYA,
                                                     PRETRACTA, TRACTA, POSTTRCTA, AREANAME, BVC001)  )
census1940_sf <- dplyr::left_join(census1940_sf, census_tbl_two, by = "GISJOIN")


# Again, the columns must be labeled appropriately.
colnames(census1940_sf)[which(names(census1940_sf) == "BUB001")] <- "Tot_Pop"
colnames(census1940_sf)[which(names(census1940_sf) == "BUD001")] <- "Male_Employed"
colnames(census1940_sf)[which(names(census1940_sf) == "BUD002")] <- "Female_Employed"
colnames(census1940_sf)[which(names(census1940_sf) == "BUE001")] <- "Tot_Occupied_Dwelling_Units"
colnames(census1940_sf)[which(names(census1940_sf) == "BUF001")] <- "Male_Foreign_Born_White_Pop"
colnames(census1940_sf)[which(names(census1940_sf) == "BUF002")] <- "Female_Foreign_Born_White_Pop"
colnames(census1940_sf)[which(names(census1940_sf) == "BU1001")] <- "Tot_Dwelling_Units"
colnames(census1940_sf)[which(names(census1940_sf) == "BVJ001")] <- "Median_Gross_Monthly_Rent"
colnames(census1940_sf)[which(names(census1940_sf) == "BVK002")] <- "Tot_Dwelling_Units_Needing_Repairs"

census1940_sf$Employed <- census1940_sf$Male_Employed + census1940_sf$Female_Employed
census1940_sf$Foreign_Born_White_Pop <- census1940_sf$Male_Foreign_Born_White_Pop + census1940_sf$Female_Foreign_Born_White_Pop

census1940_sf <- subset(census1940_sf, select = -c(Male_Employed, Female_Employed, Male_Foreign_Born_White_Pop, Female_Foreign_Born_White_Pop,
                                                   BVK001,BVK003))

# Now, we will take the data set, and form a data set with just the proportions

census2019_sf_prop <- subset(census1940_sf, select = c(GISJOIN, geometry))

# changed this - may change back
census2019_sf_prop$prop_non_white <- census1940_sf$nonwhite_pop/ census1940_sf$Tot_Pop
census2019_sf_prop$prop_black <- census1940_sf$Black_pop/ census1940_sf$Tot_Pop
census2019_sf_prop$prop_foreign_born_white <- census1940_sf$Foreign_Born_White_Pop/ census1940_sf$Tot_Pop
census2019_sf_prop$prop_employed <- census1940_sf$Employed/ census1940_sf$Tot_Pop
census2019_sf_prop$prop_high_school <- census1940_sf$TotHighSchoolEdu/ census1940_sf$Tot_Pop
census2019_sf_prop$prop_homes_major_repairs <- census1940_sf$Tot_Dwelling_Units_Needing_Repairs/ census1940_sf$Tot_Dwelling_Units
census2019_sf_prop$prop_homes_radios <- census1940_sf$Radio_Ownership/ census1940_sf$Tot_Occupied_Dwelling_Units
census2019_sf_prop$people_per_unit <- census1940_sf$person_per_occupied_dwelling_unit
census2019_sf_prop$median_home_value <- census1940_sf$Median_Home_Value
census2019_sf_prop$Tot_Pop <- census1940_sf$Tot_Pop/ st_area(census1940_sf$geometry)

# remove unnecessary variables
rm(sf_county_shapes, sf_county_geometry, sf_county_polygon, poly_graph, sf_county_subset)
rm(us_census_gis,census_gis_cntxt)
rm(census1940_sf_a)
rm(census_tbl)
rm(census_tbl_two)
