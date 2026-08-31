#' get spatial context
#'
#' @export context_sp
#'
#########################################################################
context_sp <- function(area_type = "municipality", area_name = "Barcelona", use_address = FALSE, address = "Carrer de Valencia 445, 08013 Barcelona, Spain",
                       lon = 2.155278, lat = 41.38611, coords = NULL, geom_path = geom_path, buffer = NULL, area_shape = "circle", crs = 2163) {
  if ((use_address == FALSE) && (is.numeric(lat) == TRUE)) {
    if(length(lon) == 1){
      coords_sp <- sf::st_sf(a=1, geom = sf::st_sfc(sf::st_point(c(lon,lat))), crs = "+proj=longlat +datum=WGS84")
      coords_sp <- sf::st_transform(coords_sp, crs)
    } else {
      coords_sp <- sf::st_sf(a=1, geom = sf::st_sfc(sf::st_multipoint(matrix(c(lon,lat), ncol = 2), dim = "XY")), crs = "+proj=longlat +datum=WGS84")
      coords_sp <- sf::st_transform(coords_sp, crs)
    }
  } else if (use_address == TRUE){
    coords_sp <- get_loc_coord(address = address, spatial_df = TRUE, projection = "UTM")
  } else if (!is.null(coords) == TRUE){
    coords_sp <- coords
  }
  # grid_model <-  sf::read_sf(paste0(geom_path, "Malla_CAT_1km.shp"))
  # grid_model_ij <- grid_model %>%
  #   dplyr::select(I,J, geometry) %>%
  #   sf::st_transform("+proj=utm +zone=31 +ellps=intl +units=m +no_defs")
  if (area_type == "cell") {
    # ref <- sf::st_intersection(grid_model_ij, coords_sp)
    # if(length(unique(ref$I)) > 1){
    #   sp_context <- grid_model_ij[unique(sapply(sf::st_intersects(coords_sp,grid_model_ij), function(z) if (length(z)==0) NA_integer_ else z[1])),]
    # } else {
    #   sp_context <- grid_model_ij[grid_model_ij$I == ref$I & grid_model_ij$J == ref$J,]
    # }
  } else if ((area_type == "municipality") && (!is.null(area_name))) {
    # cat_munic <- sf::st_read(paste0(geom_path, "Catalunya_munic.shp"))
    # sp_context <- cat_munic %>%
    #   sf::st_transform("+proj=utm +zone=31 +ellps=intl +units=m +no_defs") %>%
    #   dplyr::filter(NAME == area_name) %>%
    #   sf:: st_as_sf( ) #%>%
    # #sf::st_intersection(grid_model_ij)
  } else if ((area_type == "address") && (!is.null(address))) {
    # ref <- sf::st_intersection(coords_sp,grid_model_ij)
    # sp_context <- grid_model_ij[grid_model_ij$I == ref$I & grid_model_ij$J == ref$J,]
  } else if ((area_type == "district") && (!is.null(area_name))){
    # districts <- sf::read_sf(paste0(geom_path, "BCN_Districte_ED50_SHP.shp"))
    # sp_context <- districts %>%
    #   sf::st_transform(23031) %>%
    #   dplyr::filter(N_Distri == area_name) %>%
    #   sf::st_as_sf() %>%
    #   sf::st_transform("+proj=utm +zone=31 +ellps=intl +units=m +no_defs")
  } else if ((area_type == "district") && (is.null(area_name)) && (!is.null(lon) & !is.null(lat))){
    # districts <- sf::read_sf(paste0(geom_path, "BCN_Districte_ED50_SHP.shp"))
    # districts_sf <- districts %>%
    #   sf::st_transform(23031) %>%
    #   sf::st_as_sf() %>%
    #   sf::st_transform("+proj=utm +zone=31 +ellps=intl +units=m +no_defs")
    # contains <- districts_sf %>%
    #   sf::st_contains(coords_sp) %>%
    #   as.numeric()
    # sp_context <- districts_sf[which(contains == 1),]
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

# MS? - TBH I need to review this lol.
# kriging from mid point 
# adapted from Criado et al. (2022) https://earth.bsc.es/gitlab/es/universalkriging/-/blob/production/general/UK_mean.R
regrid_ok <- function(non_uniform_data, target_grid,crs_sim = "+proj=utm +zone=31 +ellps=intl +units=m +no_defs"){
  if(isTRUE(class(non_uniform_data) != "SpatialPointsDataFrame")){
    non_uniform_data <- sp::SpatialPointsDataFrame(non_uniform_data[,c("X", "Y")],non_uniform_data)
    sp::proj4string(non_uniform_data) <- crs_sim
  }
  vf_ok      <- automap::autofitVariogram(aadt ~ 1, non_uniform_data)
  ok_regular <- gstat(formula = aadt ~ 1, data = non_uniform_data, model = vf_ok$var_model, nmax = 20) 
  regular <- predict(ok_regular, target_grid)
  regular_sf <- sf::st_as_sf(regular)
  # regular <- sp::spTransform(regular, sp::CRS("+proj=longlat"))
  # pixels <- sp::SpatialPixelsDataFrame(regular,tolerance = 0.99, as.data.frame(regular[,"var1.pred"]))
  # mean_raster <- raster::raster(pixels[,'var1.pred'])
  # return(mean_raster)}
  return(regular_sf)}



print_patterns_loc <- function (pats, colgroups = NULL, n = 1:6, pat_type = "pat", 
                                title = "", size_line = 1, size_point = 1) 
{
  if (!is.null(colgroups)) {
    colgroups <- colgroups %>% dplyr::rename(chem = !!names(colgroups)[1])
  }
  else {
    colgroups <- data.frame(chem = rownames(pats), group = "1")
  }
  if (n > ncol(pats)) 
    n <- ncol(pats)
  grouping <- names(colgroups)[2]
  colnames(pats) <- paste0(pat_type, stringr::str_pad(1:ncol(pats), 
                                                      width = 2, pad = "0", side = "left"))
  pats.df <- pats %>% tibble::as_tibble() %>% dplyr::mutate(chem = colgroups[[1]]) %>% 
    tidyr::pivot_longer(-chem, names_to = "pattern", values_to = "loading") %>% 
    dplyr::right_join(., colgroups, by = "chem")
  pats.df$chem <- factor(as.character(pats.df$chem), levels = unique(as.character(pats.df$chem)))
  loadings <- pats.df %>% dplyr::filter(pattern %in% paste0(pat_type, 
                                                            stringr::str_pad(n, width = 2, pad = "0", side = "left"))) %>% 
    ggplot(aes(x = chem, y = loading, color = !!sym(grouping))) + 
    geom_point(size = size_point) + geom_segment(aes(yend = 0, xend = chem), size = size_line) + 
    facet_wrap(~pattern) + theme_bw() + theme(legend.position = "bottom", legend.text = element_text(size=12), legend.title = element_text(size=14),
                                              axis.text.x = element_text(angle = 45, hjust = 1, size = 14), strip.background = element_rect(fill = "white"), 
                                              axis.title.x = element_blank(), axis.title.y = element_blank()) + 
    geom_hline(yintercept = 0, size = 0.2) #+ ggtitle(title)
  loadings
}

 
estimate_barrier_spatial_units  <- function(sp_unit_pos) { 
  sp_unit <- grid_contxt[sp_unit_pos[1],]
  influence_area <- sf::st_buffer(sp_unit, dist = 804.672)
  roads_buffer_id <- sapply(sf::st_intersects(roads_contxt, influence_area),function(x){length(x)>0})
  roads_local <- roads_contxt[roads_buffer_id, ]
  sp_unit_buffer_id <- sapply(sf::st_intersects(grid_contxt, influence_area),function(x){length(x)>0})
  sp_unit_buffer_cents <- grid_contxt[sp_unit_buffer_id, ]
  sp_unit_buffer_cents <- sp_unit_buffer_cents[-which(sp_unit_buffer_cents$id_local == sp_unit$id_local),]
  rm(roads_buffer_id, influence_area, sp_unit_buffer_id)
  if(nrow(sp_unit_buffer_cents) > 0) {
    # find visible centroids from the sp_unit segment
    # obtain blocked centroids from sp_unit segment and their view factor
    barrier_factor <- sapply(seq_along(1:length(sp_unit_buffer_cents$id_local)), function(r) { 
      ray <- sf::st_cast(sf::st_union(sp_unit,sp_unit_buffer_cents[r,]),"LINESTRING")
      inters_loc <- lengths(sf::st_intersects(ray, roads_local, sparse = TRUE)) > 0
      if(any(inters_loc == TRUE)){
        barrier_factor <- 1
      } else {
        barrier_factor <- 0
      }
      return(barrier_factor)
    })
    severed_sp_units  <- sp_unit_buffer_cents$id_local[which(barrier_factor != 0)]
    if(length(severed_sp_units) > 0){
      n_contxt_barr_sp_units <- length(sp_unit_buffer_cents$id_local[which(barrier_factor != 0)])
      barrier_sp_units <- paste(severed_sp_units, collapse = " ")
      barrier_factor <- 100 * length(severed_sp_units) / length(sp_unit_buffer_cents$id_local)
    } else {
      n_contxt_barr_sp_units <- 0
      barrier_factor <- 0
    }
  } else {
    n_contxt_barr_sp_units <- 0
    barrier_factor <- 0
  }
  barr <- c(sp_unit_pos[2], n_contxt_barr_sp_units, barrier_factor) #barrier_sp_units,
  return(barr)
}



