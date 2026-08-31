## Code is set up so that this does not have to be run.  
###  For accessibility, the files created by this script have been uploaded into the git hub.
### This is the outline of the script that created those files from the methodology of Benavides et al.

# traffic aadt esri
url <- "https://demographics5.arcgis.com/arcgis/rest/services/USA_Traffic_Counts/MapServer/0"
tok <- "0E0uMkcA0SwWVbeHW5BNKNh55fkPUkiHHy8e6F7TL-DlIkVGxPYHfMb0mKESeAWPhlEW73kNLAR9PyLtL7xgWeoYji8m7dRCTTckdmuPSBZNEB0iSg6PhFe9EebI18Int2_A4tc6trEZBvu2HVkcDw.."
df <- esri2sf::esri2sf(url, token = tok)
saveRDS(df, paste0(generated.data.folder, "traffic_counts_esri.rds"))

# fhwa traffic segment aadt
year <- 2019
# states to be downloaded in this project
name_states <- c("CA", "TX", "WA", "NY") #todo: download texas
# some states have different naming
name_diff <- c("AL", "CA", "IL", "KY", "MI", "NC", "TX", "VA")
for(i in 1:length(name_states)){ 
  if(year == 2019){
    # which states not available yet
    not_av <- name_states[which(!file.exists(paste0(generated_data_folder, "aadt_", tolower(name_states),"_", year, ".rds")))]
    if(name_states[i] %in% not_av){
      skip_to_next <- FALSE
      if(name_states[i] %in% name_diff){
        file_name <- paste0("HPMS_", name_states[i], "_GeoIntersections_", year)
      } else {
        file_name <- paste0("HPMS_full_", name_states[i], "_", year)
      }
      url_state <- paste0("https://geo.dot.gov/server/rest/services/Hosted/",
                          file_name, "/FeatureServer/0")
      
      tryCatch(df_state <- esri2sf::esri2sf(url_state), error = function(e) { skip_to_next <<- TRUE})
      if(skip_to_next) { next } else {
        saveRDS(df_state, paste0(generated_data_folder, "aadt_", tolower(name_states[i]),"_", year, ".rds"))
        rm(df_state)
      }  
      
    } else {
      print(paste0(tolower(name_states[i]), " already present"))
    }
  }
}


