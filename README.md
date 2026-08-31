# redlining-community-severance

## This is the code required to reproduce the results, tables, and figures in the study: The Association Between Redlining and Community Severance in San Francisco.

## CSI Folder (Community Severance Index) Finals/CSI/
Creates the Community Severance Index (CSI) for San Francisco, using the methodology of Benavides et. al.  The original code can be found here: https://github.com/jaime-benavides/community_severance_us/tree/main.  The following has been copied for ease of reference.

To run the code, you need to download PCPhelpers (https://github.com/Columbia-PRIME/PCPhelpers) and pcpr (https://github.com/Columbia-PRIME/pcpr dev branch most updated March 2025) from github before running the models.
Functions and packages folders are from Benavides et. al.

### Code and data generated stored at CSI/Generated/ (file name - short description)
Data preparation (data_prep) list including tables/figures: (code can be found at CSI/CSI Code/)

#### a_01_preproc_smart_location_dta_sf.R - preprocess smart location database
smart_location_data_subset.rds - smart location data subset for each census block group
smart_location_data_subset_desc.rds - description of smart location data subset for use in this project

#### a_02_prep_osm_data_sf.R
osm_driving_network_california.rds - osm roads (also adaptable to other regions)

#### a_03_prep_traffic_sf.R - download to local machine traffic count data from esri
traffic_counts_esri.rds - object containing traffic counts from esri

#### a_04_road_infrastructure_dist_to_cbg_sf.R - estimate distance from each type of road infrastructure to census block group and estimate proximity metric
road_inf_dist_2_grid_sld_sf.rds - proximity to different type of road infrastructure for each census block group

#### a_05_traffic_co2_emissions_to_cbg_sf.R - estimate traffic co2 emissions for each census block group and estimate co2 emissions / area
traffic_co2_emis_sf.rds - traffic co2 emissions for each census block group

#### a_06_traffic_count_esri_to_cbg_sf.R - interpolate traffic count esri data to census block group centroids
traffic_count_2_grid_sld_sf.rds - object containing traffic count for each census block group centroids

#### a_07_traffic_segment_hpms_to_cbg_sf.R - interpolate traffic segment hpms data to census block group centroids
traffic_segment_2_grid_sld_sf.rds - object containing traffic segment for each census block group centroids

#### a_08_barrier_factor_prep_sf.R (time consuming: divided in 2 processes) - barrier factor for both osm and faf5 data
barrier_sp_units_sf.rds - barrier factor from both sources for each census block group

#### a_09_put_together_inputs_to_csi_sf.R - create dataframe containing all the necessary inputs to estimate community severance index
community_severance_sf_input_data.rds - input data for community severance index estimation

### Data (data) list (from Benavides et. al.)

#### The Smart Location Database 
A nationwide geographic data resource for measuring location efficiency - SmartLocationDatabase.gdb - https://www.epa.gov/smartgrowth/smart-location-mapping#SLD - data/raw/geometry/

#### SF geography
Defines spatial context. Produced by the Metropolitan Transportation Commission: https://www.arcgis.com/home/item.html?id=15e88533ab3c488a853ed32a438ad4c4#overview 
Uses San Francisco code 075

#### road infrastructure

##### OSM data
california-190101.osm.pbf - downloaded from geofabrik (search california) (https://download.geofabrik.de/north-america/us/california.html) (1.4Gb). - (from the raw directory index -  January 2019)

##### FAF 5 Network
FAF5Network.gdb - downloaded from faf 5 model - Freight Analysis Framework 5.0 Model Network Database - https://geodata.bts.gov/datasets/9343414b46794fb8be9867db2d1ccb75/about - data/raw/geometry

##### traffic activity
ESRI traffic counts
traffic intensity data from ESRI database - https://demographics5.arcgis.com/arcgis/rest/services/USA_Traffic_Counts/MapServer/0 - downloaded at a_03_prep_traffic.R and stored at data/generated/ as traffic_counts_esri.rds (can also find in Finals/pulledData/)

FHWA traffic intensity - https://geo.dot.gov/server/rest/services/Hosted/HPMS_FULL_NY_2019/FeatureServer/0 - downloaded at a_03_prep_traffic.R and stored at data/generated/ as aadt_ca_2019.rds (can also find in Finals/pulledData/)

Traffic co2 emissions at census block group from darte - DARTE_v2.gdb - downloaded from https://daac.ornl.gov/CMS/guides/CMS_DARTE_V2.html - stored at data/generated/ 


## CSI/ Redlining Comparison Code Folder - Finals/CSI/Redlining Comparison Code/
Used for combining the CSI with the HOLC and 1940 census data.

### Code and data generated stored at Finals/CSI/Redlining Comparison Code/ (file name - short description)
scripts 01-03 are meant to be run together in one session.

#### 01_Join_CSI_HOLC.grades.R - Assigns each 2019 block group with an HOLC grade and CSI value

#### 02_1940_census_data_prep.R - Preps the 1940 census data

#### 03_Join_1940.census_w_CSI.HOLC.R - Assigns the 2019 block groups (with HOLC grades and CSI values) 1940 census demographics
HOLC.CSI.1940Census_dta_us.rds - should be in the generated folder

#### 04_BoundedComparison_ps.model.R - Conducts main analysis (propensity score score analysis and matching)

#### Sensativity.Analysis_05_IPTW_R - Conducts IPTW sensitivity analysis

#### Sensitivity.Analysis_01_Join_CSI_HOLC.grades_Examines.01.R - Replaces 01_Join_CSI_HOLC.grades.R to create the dataset used in the HOLC grade adjustment sensativity analysis

#### Map.Script.R - Creates a map of the CSI and the 1940 socio-demographic variables - population density and "non-white" population.

### Data
CSI index created above - located in Finals/CSI/Generated/

#### SF geography
Defines spatial context. Produced by the Metropolitan Transportation Commission: (https://www.arcgis.com/home/item.html?id=15e88533ab3c488a853ed32a438ad4c4)
Uses San Francisco code 075

#### 2019 block groups 
The U.S. Census files for 2019 (last updated according to website 2021) https://catalog.data.gov/dataset/tiger-line-shapefile-2019-state-california-current-block-group-state-based

#### HOLC Redlining data 
The University of Richmond's data set: https://dsl.richmond.edu/panorama/redlining/data

#### 1940 Census Data 
IPUMS 1940 Census data from https://www.nhgis.org/

For this code, download two datasets as follows.

First dataset: 1. Population by Race (Source code: NT2 - NHGIS code: BUQ), 2. Native Born White Population (Source code: NT3 - NHGIS code: BU5), 3. Negro Population (Source code: NT4 - NHGIS code: BVG), 4. Population per Occupied Dwelling Unit (Source code: NT6 - NHGIS code: BVQ), 5. Persons 25 Years and Over by Sex by Years of School Completed (Source code: NT15 - NHGIS code: BUH), 6. Occupied Dwelling Units by Number of Occupants (Source code: NT31 - NHGIS code: BU7), 7. Median Value of Homes for Which Value was Reported (Source code: NT36 - NHGIS code: BVC), 8. Occupied Dwelling Units by Radio Ownership (Source code: NT45 - NHGIS code: BVM).


Second dataset: 1. Population (Source code: NT1 - NHGIS code: BUB), 2. Employed Persons by Sex (Source code: NT11 - NHGIS code: BUD), 3. Total Occupied Dwelling Units (Source code: NT12 - NHGIS code: BUE), 4. Foreign Born White Population by Sex (Source code: NT13 - NHGIS code: BUF), 5. Total Dwelling Units (Source code: NT26 - NHGIS code: BU1), 6. Median Value of Homes for Which Value was Reported (Source code: NT36 - NHGIS code: BVC), 7. Median Gross Monthly Rent (Source code: NT42 - NHGIS code: BVJ), 8. Dwelling Units by State of Repair (Source code: NT43 - NHGIS code: BVK).



