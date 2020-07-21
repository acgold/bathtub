#' Assemble bathtub inundation model
#'
#' @param pipes A \code{sf} object denoting stormwater pipes. Result of \code{setup_pipes}
#' @param structures A \code{sf} object denoting stormwater structures Result of \code{setup_structures}
#' @param type Type of invert survey data. 'depth', 'elevation', or 'none'
#' @param use_raster_elevation Extract elevation and use as surface elevations?
#' @param elev \code{RasterLayer} DEM object for inundation modeling.Result of \code{DEM_setup}
#' @param elev_units Units of \code{elev} values
#' @param min_elev_cutoff Minimum cutoff for elevation values
#' @param buffer Size of buffer for connecting structures to pipes
#' @param interp_rnds Rounds of interpolation for missing invert elevations
#' @param guess_connectivity Assume connectivity of pipes that overlap without nodes?
#' @param up_condition Column name of upstream pipe condition. Either text of condition to be
#' searched with \code{obstruction_keywords} or percent obstructions (\code{obstruction_percent})
#' @param dn_condition Column name of downstream pipe condition. Either text of condition to be
#' searched with \code{obstruction_keywords} or percent obstructions (\code{obstruction_percent})
#' @param structure_condition Column name of structure condition. Either text of condition to be
#' searched with \code{obstruction_keywords} or percent obstructions (\code{obstruction_percent})
#' @param obstruction_keywords Vector of keywords (in quotes) denoting obstruction to evaluate in
#' \code{up_condition}, \code{dn_condition}, \code{structure_condition}
#' @param obstruction_percent Should \code{up_condition}, \code{dn_condition}, and \code{structure_condition},
#' be evaluated as percent obstructed?
#' @param workspace bathtub workspace path
#' @param overwrite Overwrite existing model files in the workspace? Default is TRUE. If set to FALSE,
#' model will not be saved in output folder and will exist in memory (see load_model to load existing model).
#'
#' @return An list of three \code{sf} objects denoting Pipes, Nodes, and Structures. Files are saved in '/model/'
#' folder in workspace and by default overwrite existing model files
#' @examples
#'# bft_model <- assemble_net_model(
#'# pipes = pipes_n,
#'# structures = structures_n,
#'# type = "none",
#'# elev = bft_elev,
#'# elev_units = "m",
#'# use_raster_elevation = F,
#'# buffer = 1,
#'# guess_connectivity = T
#'# )

assemble_net_model <- function(pipes,
                                   structures,
                                   type = "elevation",
                                   use_raster_elevation = T,
                                   elev,
                                   elev_units = "m",
                                   min_elev_cutoff = -5,
                                   buffer = 0.5,
                                   interp_rnds = 5,
                                   guess_connectivity = T,
                                   up_condition = NULL ,
                                   dn_condition = NULL ,
                                   structure_condition = NULL,
                                   obstruction_keywords = NULL,
                                   obstruction_percent = F,
                                  overwrite = T,
                               workspace
) {
  units::units_options(set_units_mode = "standard")

  final_crs <- sf::st_crs(pipes)

  #Find invert units
  if(!is.null(pipes$from_depth)){
    invert_units <- units(pipes$from_depth)$numerator
  }

  if(!is.null(pipes$from_inv_elev) & is.null(pipes$from_depth)){
    invert_units <- units(pipes$from_inv_elev)$numerator
  }

  #Set CRS to same, and tell user that happened
  if(sf::st_crs(pipes) != sf::st_crs(structures)){
    structures <- structures %>%
      sf::st_transform(.,sf::st_crs(pipes))

    warning("Projected Structures to same CRS as Pipes")
  }

  nodes <- pipes %>%
    sf::st_coordinates() %>%
    tibble::as_tibble() %>%
    dplyr::rename(edgeID = ifelse("L2" %in% names(.), "L2", "L1")) %>%
    dplyr::group_by(edgeID) %>%
    dplyr::slice(c(1, n())) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(start_end = rep(c('start', 'end'), times = n()/2))

  nodes <- nodes %>%
    dplyr::mutate(xy = paste(round(.$X,6), round(.$Y, 6))) %>%
    dplyr::group_by(xy) %>%
    dplyr::mutate(nodeID = dplyr::cur_group_id()) %>%
    ungroup() %>%
    dplyr::select(X, Y, edgeID, start_end, nodeID)

  nodes <- nodes %>%
    sf::st_as_sf(coords = c('X', 'Y')) %>%
    sf::st_set_crs(sf::st_crs(pipes))

  source_nodes <- nodes %>%
    dplyr::filter(start_end == 'start')

  target_nodes <- nodes %>%
    dplyr::filter(start_end == 'end')

  pipes <-  pipes %>%
    dplyr::mutate(from = source_nodes %>% dplyr::pull(nodeID), to = target_nodes %>% dplyr::pull(nodeID))

  structures <- structures %>%
    dplyr::mutate(structureID = 1:n())

  structures_new <- structures

  if(use_raster_elevation == T){
    start_points_elev <- source_nodes %>%
      sf::st_as_sf(coords = c('X', 'Y')) %>%
      sf::st_set_crs(sf::st_crs(pipes)) %>%
      dplyr::mutate(start_elev = units::set_units(units::set_units(raster::extract(elev, source_nodes), value = !!elev_units), value = !!invert_units))%>%
      dplyr::select(edgeID, start_elev) %>%
      sf::st_buffer(buffer) %>%
      sf::st_join(structures %>% dplyr::select(structureID, s_elev, s_inv_elev)) %>%
      tibble::as_tibble()

    end_points_elev <- target_nodes %>%
      sf::st_as_sf(coords = c('X', 'Y')) %>%
      sf::st_set_crs(sf::st_crs(pipes)) %>%
      dplyr::mutate(end_elev = units::set_units(units::set_units(raster::extract(elev, target_nodes), !!elev_units), value = !!invert_units))%>%
      dplyr::select(edgeID, end_elev) %>%
      sf::st_buffer(buffer) %>%
      sf::st_join(structures %>% dplyr::select(structureID, s_elev, s_inv_elev)) %>%
      tibble::as_tibble()
  }

  if(use_raster_elevation == F){
    start_points_elev <- source_nodes %>%
      dplyr::left_join(pipes %>% tibble::as_tibble() %>%
                  dplyr::select(edgeID, from_elev), by = "edgeID") %>%
      dplyr::mutate(start_elev = from_elev) %>%
      sf::st_as_sf(coords = c('X', 'Y')) %>%
      sf::st_set_crs(sf::st_crs(pipes)) %>%
      sf::st_buffer(buffer) %>%
      sf::st_join(structures %>% dplyr::select(structureID, s_elev, s_inv_elev)) %>%
      dplyr::mutate(start_elev =  dplyr::if_else(!is.na(from_elev), from_elev, s_elev)) %>%  #prioritize using pipe end elevations over structures
      dplyr::select(edgeID, start_elev, structureID, s_elev, s_inv_elev) %>%
      tibble::as_tibble()

    end_points_elev <- target_nodes %>%
      dplyr::left_join(pipes %>% tibble::as_tibble() %>%
                  dplyr::select(edgeID, to_elev), by = "edgeID") %>%
      dplyr::mutate(end_elev = to_elev) %>%
      sf::st_as_sf(coords = c('X', 'Y')) %>%
      sf::st_set_crs(sf::st_crs(pipes)) %>%
      sf::st_buffer(buffer) %>%
      sf::st_join(structures %>% dplyr::select(structureID, s_elev, s_inv_elev)) %>%
      dplyr::mutate(end_elev = dplyr::if_else(!is.na(to_elev), to_elev, s_elev)) %>%  #prioritize using pipe end elevations over structures
      dplyr::select(edgeID, end_elev, structureID, s_elev, s_inv_elev) %>%
      tibble::as_tibble()

  }

  if (type == "depth") {
    pipes_elev <- pipes %>%
      tibble::as_tibble() %>%
      dplyr::left_join(start_points_elev,
                by = "edgeID") %>%
      dplyr::mutate(
        from_elev = start_elev,
        from_inv_elev = dplyr::if_else(!is.na(from_elev - from_depth),from_elev - from_depth, s_inv_elev),
        from_strucID = structureID,
        from_s_inv_elev = s_inv_elev
      ) %>%
      dplyr::select(-c(structureID, start_elev, s_elev, s_inv_elev, geometry.y)) %>%
      dplyr::mutate(geometry = geometry.x) %>%
      dplyr::select(-c(geometry.x)) %>%
      dplyr::left_join(end_points_elev,
                by = "edgeID") %>%
      dplyr::mutate(to_elev = end_elev,
             to_inv_elev = dplyr::if_else(!is.na(to_elev - to_depth),to_elev - to_depth, s_inv_elev),
             to_strucID = structureID,
             to_s_inv_elev = s_inv_elev) %>%
      dplyr::select(-c(structureID, geometry.y, end_elev, s_elev, s_inv_elev)) %>%
      dplyr::mutate(geometry = geometry.x) %>%
      dplyr::select(-c(geometry.x)) %>%
      sf::st_as_sf()
  }

  if(type == "elevation"){
    pipes_elev <- pipes %>%
      tibble::as_tibble() %>%
      dplyr::left_join(
        start_points_elev,
        by = "edgeID"
      ) %>%
      mutate(
        from_elev = start_elev,
        from_inv_elev = dplyr::if_else(is.na(from_inv_elev), s_inv_elev, from_inv_elev),
        from_strucID = structureID,
        from_s_inv_elev = s_inv_elev
      ) %>%
      dplyr::select(-c(structureID, start_elev, s_elev, s_inv_elev, geometry.y)) %>%
      dplyr::mutate(geometry = geometry.x) %>%
      dplyr::select(-c(geometry.x)) %>%
      dplyr::left_join(
        end_points_elev,
        by = "edgeID"
      ) %>%
      dplyr::mutate(
        to_elev = end_elev,
        to_inv_elev = dplyr::if_else(is.na(to_inv_elev), s_inv_elev, to_inv_elev),
        to_strucID = structureID,
        to_s_inv_elev = s_inv_elev
      ) %>%
      dplyr::select(-c(structureID, geometry.y, end_elev,s_elev, s_inv_elev)) %>%
      dplyr::mutate(geometry = geometry.x) %>%
      dplyr::select(-c(geometry.x)) %>%
      sf::st_as_sf()
  }

  if(type == "none"){
    pipes_elev <- pipes %>%
      tibble::as_tibble() %>%
      dplyr::left_join(
        start_points_elev,
        by = "edgeID"
      ) %>%
      dplyr::mutate(
        from_elev = start_elev,
        from_inv_elev = s_inv_elev,
        from_strucID = structureID,
        from_s_inv_elev = s_inv_elev
      ) %>%
      dplyr::select(-c(structureID, start_elev, s_elev, s_inv_elev, geometry.y)) %>%
      dplyr::mutate(geometry = geometry.x) %>%
      dplyr::select(-c(geometry.x)) %>%
      dplyr::left_join(
        end_points_elev,
        by = "edgeID"
      ) %>%
      dplyr::mutate(
        to_elev = end_elev,
        to_inv_elev = s_inv_elev,
        to_strucID = structureID,
        to_s_inv_elev = s_inv_elev
      ) %>%
      dplyr::select(-c(structureID, geometry.y, end_elev, s_elev, s_inv_elev)) %>%
      dplyr::mutate(geometry = geometry.x) %>%
      dplyr::select(-c(geometry.x)) %>%
      sf::st_as_sf()
  }

  new_nodes_start <- nodes %>%
    tibble::as_tibble() %>%
    dplyr::filter(start_end == "start") %>%
    dplyr::left_join(pipes_elev %>%
                       tibble::as_tibble() %>%
                dplyr::select(edgeID, from_elev, from_inv_elev, from_strucID, from_s_inv_elev) %>%
                dplyr::mutate(elev = from_elev, inv_elev = from_inv_elev, structureID = from_strucID, s_inv_elev = from_s_inv_elev) %>%
                dplyr::select(-c(from_elev, from_inv_elev, from_strucID, from_s_inv_elev)))

  new_nodes_end <- nodes %>%
    tibble::as_tibble() %>%
    dplyr::filter(start_end == "end") %>%
    dplyr::left_join(pipes_elev %>%
                       tibble::as_tibble() %>%
                dplyr::select(edgeID, to_elev, to_inv_elev, to_strucID, to_s_inv_elev) %>%
                dplyr::mutate(elev = to_elev, inv_elev = to_inv_elev, structureID = to_strucID, s_inv_elev = to_s_inv_elev) %>%
                dplyr::select(-c(to_elev, to_inv_elev, to_strucID, to_s_inv_elev)))

  new_nodes <- new_nodes_start %>%
    rbind(new_nodes_end) %>%
    sf::st_as_sf()

  interp <- bathtub::interpolate_network(
    pipes = pipes_elev,
    nodes = new_nodes,
    rounds = interp_rnds
  )

  pipes_interp <- interp[[1]] %>% sf::st_as_sf() #%>% sf::st_set_crs(final_crs)
  nodes_interp <- interp[[2]] %>% sf::st_as_sf() #%>% sf::st_set_crs(final_crs)

  if(guess_connectivity == T){
    cat("Analyzing network to identify connectivity errors...\n")
    nodes_interp <- nodes_interp %>%
      dplyr::group_by(nodeID) %>%
      sf::st_join(pipes_interp %>%
                    dplyr::mutate(alt_edgeID = edgeID) %>%
                    dplyr::select(alt_edgeID,from,to),
                  join = sf::st_is_within_distance,
                  dist = buffer) %>%
      dplyr::group_by(edgeID, start_end, nodeID, elev, inv_elev, structureID, s_inv_elev, min_inv_elev, interp) %>%
      dplyr::summarise(alt_edgeID = list(alt_edgeID)) %>%
      ungroup()
  }

  from_elevation = -3
  to_elevation = 1
  step = 1

  minimum_area <- units::set_units(0.1, "km^2")
  conv_fac <- 1 / units::drop_units(units::set_units(units::set_units(1, elev_units), units(pipes_interp$from_inv_elev)$numerator))

  DEM_adjusted <- raster::calc(
    elev,
    fun = function(x) {
      x / conv_fac
    }
  )

  DEM_adjusted[DEM_adjusted < min_elev_cutoff] <- min_elev_cutoff
  elev_seq = units::set_units(seq(from = from_elevation, to = to_elevation, by = step), value = units(pipes_interp$from_inv_elev)$numerator)

  pb <- progress::progress_bar$new(format = " Identifying outlets [:bar] :current/:total (:percent)", total = length(elev_seq))
  outlet_id <- tibble()

  for(i in units::drop_units(elev_seq)){

    select_rast <- raster::calc(
      DEM_adjusted,
      fun = function(x) {
        x < i
      }
    )

    select_rast[select_rast == 0] <- NA

    select_rast_stars <- stars::st_as_stars(select_rast)

    select_rast_sf <- sf::st_as_sf(select_rast_stars,
                                   as_points = FALSE,
                                   merge = TRUE,
                                   connect8 = T) %>%
      dplyr::mutate(area = sf::st_area(.)) %>%
      dplyr::filter(area > minimum_area)

    select_nodes <- nodes_interp %>%
      sf::st_join(select_rast_sf %>% sf::st_transform(sf::st_crs(nodes))) %>%
      dplyr::filter(!is.na(layer)) %>%
      dplyr::select(-c(layer,area))

    impacted_nodes <- propagate_flood_ts(pipes = pipes_interp, nodes= nodes_interp, structures = structures_new, select_nodes = select_nodes, water_elevation = i)

    outlet_id <- outlet_id %>%
      rbind(impacted_nodes %>% add_column(water_elevation = i))

    pb$tick(1)
    }

  nodes_interp <- nodes_interp %>%
    tibble::as_tibble() %>%
    dplyr::left_join(outlet_id %>%
                       tibble::as_tibble() %>%
      dplyr::group_by(nodeID) %>%
      dplyr::arrange(water_elevation) %>%
        dplyr::summarise(z_min = min(z,na.rm=T)) %>%
        dplyr::ungroup(),
      by="nodeID") %>%
    tidyr::replace_na(list(z_min = 5)) %>%
    dplyr::group_by(nodeID) %>%
    dplyr::mutate(outlet = ifelse((z_min == 0 & (!"start" %in% start_end))|(inv_elev < units::set_units(-1,units(nodes_interp$inv_elev)$numerator) & (!"start" %in% start_end)), T, F)) %>%
    tidyr::replace_na(list(outlet = F)) %>%
    dplyr::ungroup() %>%
    st_as_sf()


  with_obstructions <- bathtub::add_obstructions(
    pipes = pipes_interp,
    nodes = nodes_interp,
    structures = structures_new,
    up_condition = up_condition,
    dn_condition = dn_condition,
    obstruction_keywords = obstruction_keywords,
    structure_condition = structure_condition,
    obstruction_percent = obstruction_percent
  )


  if(overwrite == T){
  # Write model to bathtub output folder
    bathtub::save_w_units(x = with_obstructions[[1]], full_path = paste0(workspace,"/model/pipes.gpkg"))
    bathtub::save_w_units(x = with_obstructions[[2]] %>%
                            dplyr::group_by(edgeID, start_end, nodeID, elev, inv_elev, structureID, s_inv_elev, min_inv_elev, interp) %>%
                            dplyr::mutate(alt_edgeID = sapply(alt_edgeID, toString)), full_path = paste0(workspace,"/model/nodes.gpkg"))
    bathtub::save_w_units(x = with_obstructions[[3]], full_path = paste0(workspace,"/model/structures.gpkg"))
  }

  return(list(pipes = with_obstructions[[1]], nodes = with_obstructions[[2]], structures = with_obstructions[[3]]))
}
