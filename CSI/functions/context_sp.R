#' get spatial context
#'
#' @export context_sp
#'
#########################################################################
context_sp <- function(area_type = "municipality", area_name = "Barcelona", use_address = FALSE, address = "Carrer de Valencia 445, 08013 Barcelona, Spain",
                           lon = 2.155278, lat = 41.38611, coords = NULL, geom_path = geom_path, buffer = NULL, area_shape = "circle") {
    if ((use_address == FALSE) && (is.numeric(lat) == TRUE)) {
      if(length(lon) == 1){
        coords_sp <- sf::st_sf(a=1, geom = sf::st_sfc(sf::st_point(c(lon,lat))), crs = "+proj=longlat +datum=WGS84")
        coords_sp <- sf::st_transform(coords_sp, "+proj=utm +zone=31 +ellps=intl +units=m +no_defs")
      } else {
        coords_sp <- sf::st_sf(a=1, geom = sf::st_sfc(sf::st_multipoint(matrix(c(lon,lat), ncol = 2), dim = "XY")), crs = "+proj=longlat +datum=WGS84")
        coords_sp <- sf::st_transform(coords_sp, "+proj=utm +zone=31 +ellps=intl +units=m +no_defs")
      }
      } else if (use_address == TRUE){
      coords_sp <- get_loc_coord(address = address, spatial_df = TRUE, projection = "UTM")
      } else if (!is.null(coords) == TRUE){
        coords_sp <- coords
    }
  grid_model <-  sf::read_sf(paste0(geom_path, "Malla_CAT_1km.shp"))
  grid_model_ij <- grid_model %>%
    dplyr::select(I,J, geometry) %>%
    sf::st_transform("+proj=utm +zone=31 +ellps=intl +units=m +no_defs")
    if (area_type == "cell") {
    ref <- sf::st_intersection(grid_model_ij, coords_sp)
    if(length(unique(ref$I)) > 1){
      sp_context <- grid_model_ij[unique(sapply(sf::st_intersects(coords_sp,grid_model_ij), function(z) if (length(z)==0) NA_integer_ else z[1])),]
    } else {
      sp_context <- grid_model_ij[grid_model_ij$I == ref$I & grid_model_ij$J == ref$J,]
    }
    } else if ((area_type == "municipality") && (!is.null(area_name))) {
    cat_munic <- sf::st_read(paste0(geom_path, "Catalunya_munic.shp"))
    sp_context <- cat_munic %>%
      sf::st_transform("+proj=utm +zone=31 +ellps=intl +units=m +no_defs") %>%
      dplyr::filter(NAME == area_name) %>%
      sf:: st_as_sf( ) #%>%
      #sf::st_intersection(grid_model_ij)
    } else if ((area_type == "address") && (!is.null(address))) {
      ref <- sf::st_intersection(coords_sp,grid_model_ij)
      sp_context <- grid_model_ij[grid_model_ij$I == ref$I & grid_model_ij$J == ref$J,]
    } else if ((area_type == "district") && (!is.null(area_name))){
      districts <- sf::read_sf(paste0(geom_path, "BCN_Districte_ED50_SHP.shp"))
      sp_context <- districts %>%
        sf::st_transform(23031) %>%
        dplyr::filter(N_Distri == area_name) %>%
        sf::st_as_sf() %>%
        sf::st_transform("+proj=utm +zone=31 +ellps=intl +units=m +no_defs")
    } else if ((area_type == "district") && (is.null(area_name)) && (!is.null(lon) & !is.null(lat))){
      districts <- sf::read_sf(paste0(geom_path, "BCN_Districte_ED50_SHP.shp"))
      districts_sf <- districts %>%
        sf::st_transform(23031) %>%
        sf::st_as_sf() %>%
        sf::st_transform("+proj=utm +zone=31 +ellps=intl +units=m +no_defs")
      contains <- districts_sf %>%
        sf::st_contains(coords_sp) %>%
        as.numeric()
      sp_context <- districts_sf[which(contains == 1),]
    } else if ((area_type == "AMB") && (area_name == "primera_corona")) {
      VML_domain <- sf::st_read(paste0(geom_path, "/spatial_context_squared.shp"))
      sp_context <- VML_domain %>%
        sf::st_transform("+proj=utm +zone=31 +ellps=intl +units=m +no_defs")
    } else if ((area_type == "BCN") && (area_name == "intrarrondes")) {
      VML_domain <- sf::st_read(paste0(geom_path, "ewgt_spatial_context.shp"))
      sp_context <- VML_domain %>%
        sf::st_transform("+proj=utm +zone=31 +ellps=intl +units=m +no_defs")
    } else if (area_type == "buffer" && area_shape == "square"){
      sp_context <- sf::st_buffer(coords_sp, buffer, nQuadSegs = 1)
      } else if (area_type == "buffer" && area_shape == "circle"){
      sp_context <- sf::st_buffer(coords_sp, buffer)
  }else {
    print("we are not working yet in that spatial context. Do you think we should?")
    }
  # sp_context <- methods::as(sp_context, "Spatial")
  if(!is.null(buffer) && is.numeric(buffer) && area_type != "buffer"){
    #sp_context <- rgeos::gBuffer(sp_context,width=buffer,capStyle="FLAT")
    sp_context <- sf::st_buffer(sp_context, buffer, nQuadSegs = 30)
    }
  return(sp_context)
}
