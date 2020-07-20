#' Run bathtub inundation model
#'
#' @param model A \code{bathtub} model. Result of \code{assemble_network_model}
#' @param elev \code{RasterLayer} DEM object for inundation modeling. Result of \code{DEM_setup}.
#' @param elev_units Units of \code{elev} values
#' @param overlay \code{sf} object or \code{RasterLayer} of flooding extent (Optional)
#' @param overlay_quantile Quantile of elevation values flooded by \code{overlay} to use for
#'  modeling flooding (Optional)
#' @param from_elevation Lower bound of elevation for modeling. Units same as model inverts
#' @param to_elevation Upper bound of elevation for modeling. Units same as model inverts
#' @param step Step of sequence between \code{from_elevation} and \code{to_elevation}.
#' Units same as model inverts
#' @param minimum_area Minimum area of flooded area to keep
#' @param minimum_area_units Units of flooded area
#' @param min_elev_cutoff Minimum cutoff for elevation values
#' @param use_outlets Force outlets to be connected by receiving waters
#' (i.e., contiguous flooded area > min_elev_cutoff)
#' @param model_ponding Model surface ponding from structure surcharge?
#' @param site_name Name of site
#' @param workspace Path to bathtub folder
#'
#' @return A list of \code{sf} objects denoting impacted infrastructure from each step
#' of the model. Objects include impacted Pipes, Nodes, and Structures (using propagation
#' through network), Nodes and Structures impacted by overland flooding with no propagation
#' through network (e.g., "np_nodes" & "np_structures"), overland flooding, overland ponding.
#' Includes the overlay if used rather than a range of water levels.
#' @examples
#'bft_model_output <- model_inundation(
#'model = bft_model,
#'elev = bft_elev,
#'elev_units = "m",
#'from_elevation = -3,
#'to_elevation = 4,
#'step = 3/12,
#'model_ponding = T,
#'site_name = "beaufort",
#'overwrite = T,
#'minimum_area = 0.01,
#'workspace = workspace
#')


model_inundation <- function(model,
                             elev,
                             elev_units,
                             overlay = NULL,
                             overlay_quantile = 0.95,
                             from_elevation = -3,
                             to_elevation = 3,
                             step = 0.5,
                             minimum_area = 0.6,
                             minimum_area_units = "km^2",
                             min_elev_cutoff = -5,
                             use_outlets = T,
                             model_ponding = T,
                             site_name,
                             workspace,
                             overwrite = T){

  units::units_options(set_units_mode = "standard")

  pipes = model[[1]]
  nodes = model[[2]]
  structures = model[[3]]

  conv_fac <- 1 / units::drop_units(units::set_units(units::set_units(1, elev_units), units(pipes$from_inv_elev)$numerator))

  minimum_area <- units::set_units(minimum_area, minimum_area_units)

  DEM_adjusted <- raster::calc(
    elev,
    fun = function(x) {
      x / conv_fac
    }
  )

  DEM_adjusted[DEM_adjusted < min_elev_cutoff] <- min_elev_cutoff

  total_np_structures <- NULL
  total_np_nodes <- NULL
  total_impacted_nodes <- NULL
  total_impacted_structures <-NULL
  total_impacted_pipes <- NULL
  total_flooding <- NULL

  if(is.null(overlay)){
    cat("Using provided DEM and water level scenarios to estimate flooding extent...\n")

    elev_seq = units::set_units(seq(from = from_elevation, to = to_elevation, by = step), value = units(pipes$from_inv_elev)$numerator)

    pb <- progress::progress_bar$new(format = " Running the 1-D model [:bar] :current/:total (:percent)", total = length(elev_seq))

    for(i in elev_seq){
      start <- Sys.time()

      select_rast <- raster::calc(
        DEM_adjusted,
        fun = function(x) {
          x < i
        }
      )
      select_rast[select_rast == 0] <- NA

      select_rast_stars <- stars::st_as_stars(select_rast, crs = sf::st_crs(select_rast))

      select_rast_sf <- sf::st_as_sf(select_rast_stars,
                                 as_points = FALSE,
                                 merge = T,
                                 connect8 = T) %>%
        mutate(area = sf::st_area(.)) %>%
        filter(area > minimum_area)

      if(use_outlets == T){
        select_nodes <- nodes %>%
          sf::st_join(select_rast_sf %>% sf::st_transform(sf::st_crs(nodes))) %>%
          dplyr::filter(!is.na(layer) | (outlet == T & inv_elev < units::set_units(i, units(nodes$inv_elev)$numerator))) %>%
          dplyr::select(-c(layer,area))
      }

      if(use_outlets == F){
        select_nodes <- nodes %>%
          sf::st_join(select_rast_sf %>% sf::st_transform(sf::st_crs(nodes))) %>%
          dplyr::filter(!is.na(layer)) %>%
          dplyr::select(-c(layer,area))
      }

      impacted_nodes <- propagate_flood(pipes = model[[1]], nodes= model[[2]], structures = model[[3]], select_nodes = select_nodes, water_elevation = i)

      impacted_nodes <- impacted_nodes %>%
        dplyr::mutate(node_fill_height = dplyr::if_else(units::set_units(i,units(inv_elev)$numerator) > inv_elev, (units::set_units(i, units(inv_elev)$numerator) + abs(inv_elev)) - (inv_elev + abs(inv_elev)),units::set_units(0,units(inv_elev)$numerator)),
               node_perc_fill = dplyr::if_else(units::set_units(i, units(inv_elev)$numerator) < elev,(node_fill_height * 100)/((elev + abs(inv_elev)) - (inv_elev + abs(inv_elev))), 100))

      impacted_structures <- structures %>%
        dplyr::filter(structureID %in% impacted_nodes$structureID) %>%
        dplyr::mutate(structure_fill_height = dplyr::if_else(units::set_units(i,units(s_inv_elev)$numerator) > s_inv_elev,(units::set_units(i,units(s_inv_elev)$numerator) + abs(s_inv_elev)) - (s_inv_elev + abs(s_inv_elev)),units::set_units(0,units(s_inv_elev)$numerator)),
               structure_perc_fill = dplyr::if_else(units::set_units(i,units(s_inv_elev)$numerator) < s_elev,(structure_fill_height * 100)/((s_elev + abs(s_inv_elev)) - (s_inv_elev + abs(s_inv_elev))), 100),
               structure_surcharge = dplyr::if_else((units::set_units(i,units(s_inv_elev)$numerator)) > s_elev, (units::set_units(i,units(s_inv_elev)$numerator) - s_elev), (units::set_units(0,units(s_inv_elev)$numerator))))

      np_structures <- structures %>%
        filter(structureID %in% select_nodes$structureID) %>%
        mutate(structure_fill_height = dplyr::if_else(units::set_units(i,units(s_inv_elev)$numerator) > s_inv_elev,(units::set_units(i,units(s_inv_elev)$numerator) + abs(s_inv_elev)) - (s_inv_elev + abs(s_inv_elev)),units::set_units(0,units(s_inv_elev)$numerator)),
               structure_perc_fill = dplyr::if_else(units::set_units(i,units(s_inv_elev)$numerator) < s_elev,(structure_fill_height * 100)/((s_elev + abs(s_inv_elev)) - (s_inv_elev + abs(s_inv_elev))), 100),
               structure_surcharge = dplyr::if_else((units::set_units(i,units(s_inv_elev)$numerator)) > s_elev, (units::set_units(i, units(s_inv_elev)$numerator) - s_elev), (units::set_units(0, units(s_inv_elev)$numerator))))


      impacted_pipes <- pipes[pipes$edgeID %in% impacted_nodes$edgeID, ]

      total_impacted_nodes <- rbind(total_impacted_nodes, impacted_nodes %>% tibble::add_column(water_elevation = units::set_units(i, units(nodes$inv_elev)$numerator)))
      total_np_nodes <- rbind(total_np_nodes, select_nodes %>% tibble::add_column(water_elevation = units::set_units(i, units(nodes$inv_elev)$numerator)))
      total_impacted_structures <- rbind(total_impacted_structures, impacted_structures %>% tibble::add_column(water_elevation = units::set_units(i, units(nodes$inv_elev)$numerator)))
      total_np_structures <- rbind(total_np_structures, np_structures %>% tibble::add_column(water_elevation = units::set_units(i, units(nodes$inv_elev)$numerator)))
      total_impacted_pipes <- rbind(total_impacted_pipes, impacted_pipes %>% tibble::add_column(water_elevation = units::set_units(i, units(nodes$inv_elev)$numerator)))
      total_flooding <- rbind(total_flooding, select_rast_sf %>% tibble::add_column(water_elevation = units::set_units(i, units(nodes$inv_elev)$numerator)))

      pb$tick(1)

    }

    if(model_ponding == T){
      cat("")

      ponding_shps <- NULL

      pb <- progress::progress_bar$new(format = " Modeling ponding from structure overflow [:bar] :current/:total (:percent)", total = length(total_impacted_structures %>% pull(water_elevation) %>% unique()))

      for(i in elev_seq){
        ponded_structures <- total_impacted_structures %>%
          filter(water_elevation == units::set_units(i,units(total_impacted_structures$water_elevation)$numerator),
                 structure_surcharge > units::set_units(0, units(total_impacted_structures$structure_surcharge)$numerator))

        if(nrow(ponded_structures) > 0){
            select_rast_ponding <- raster::calc(
              DEM_adjusted,
              fun = function(x) {
                x < i
              }
            )
            select_rast_ponding[select_rast_ponding == 0] <- NA

            select_rast_stars <- stars::st_as_stars(select_rast_ponding, crs = sf::st_crs(select_rast_ponding))

            select_rast_sf <- sf::st_as_sf(select_rast_stars,
                                           as_points = FALSE,
                                           merge = T,
                                           connect8 = T)

            filtered_pond_shps <- select_rast_sf %>%
              sf::st_join(ponded_structures %>% st_buffer(dist=(raster::res(select_rast_ponding)[1]/2)+raster::res(select_rast_ponding)[1]/20)) %>%
              dplyr::filter(!is.na(structureID)) %>%
              # sf::st_cast("MULTIPOLYGON")
              sf::st_union() %>%
              st_as_sf()

            other_flooding <- total_flooding %>%
              filter(water_elevation == units::set_units(i, units(total_flooding$water_elevation)$numerator)) %>%
              sf::st_union() %>%
              st_as_sf()

            if(nrow(sf::st_difference(filtered_pond_shps,other_flooding))>0){

              filtered_pond_shps <- filtered_pond_shps %>%
                sf::st_difference(other_flooding) %>%
                mutate(water_elevation = i)

              ponding_shps <- rbind(ponding_shps, filtered_pond_shps)
            }

            rm(filtered_pond_shps)

        }
        pb$tick(1)
      }

      sf::st_write(obj = ponding_shps, dsn = paste0(workspace, "/structures/", site_name, "_ponding_extent.shp"), delete_layer = overwrite, quiet = T)
      sf::st_write(obj = total_impacted_nodes, dsn = paste0(workspace, "/structures/", site_name, "_imp_nodes.shp"), delete_layer = overwrite, quiet = T)
      sf::st_write(obj = total_impacted_structures, dsn = paste0(workspace, "/structures/", site_name, "_imp_struc.shp"), delete_layer = overwrite, quiet = T)
      sf::st_write(obj = total_np_structures, dsn = paste0(workspace, "/structures/", site_name, "_np_struc.shp"), delete_layer = overwrite, quiet = T)
      sf::st_write(obj = total_impacted_pipes, dsn = paste0(workspace, "/structures/", site_name, "_imp_pipes.shp"), delete_layer = overwrite, quiet = T)
      sf::st_write(obj = total_flooding, dsn = paste0(workspace, "/structures/", site_name, "_flooding_extent.shp"), delete_layer = overwrite, quiet = T)

      return(list(pipes = total_impacted_pipes,
                  nodes = total_impacted_nodes,
                  np_nodes = total_np_nodes,
                  structures = total_impacted_structures,
                  np_structures = total_np_structures,
                  flooding = total_flooding,
                  ponding = ponding_shps))

    }

    sf::st_write(obj = total_impacted_nodes, dsn = paste0(workspace, "/structures/", site_name, "_imp_nodes.shp"), delete_layer = overwrite, quiet = T)
    sf::st_write(obj = total_impacted_structures, dsn = paste0(workspace, "/structures/", site_name, "_imp_struc.shp"), delete_layer = overwrite, quiet = T)
    sf::st_write(obj = total_np_structures, dsn = paste0(workspace, "/structures/", site_name, "_np_struc.shp"), delete_layer = overwrite, quiet = T)
    sf::st_write(obj = total_impacted_pipes, dsn = paste0(workspace, "/structures/", site_name, "_imp_pipes.shp"), delete_layer = overwrite, quiet = T)
    sf::st_write(obj = total_flooding, dsn = paste0(workspace, "/structures/", site_name, "_flooding_extent.shp"), delete_layer = overwrite, quiet = T)

    return(list(pipes = total_impacted_pipes,
                nodes = total_impacted_nodes,
                np_nodes = total_np_nodes,
                structures = total_impacted_structures,
                np_structures = total_np_structures,
                flooding = total_flooding))
  }

  if(!is.null(overlay)){
    cat("Using provided overlay layer to estimate flooding extent including SW network...")

    if(class(overlay)[1] == "RasterLayer"){
      ext <- sf::st_make_grid(model[[1]], n = 1) %>% sf::st_buffer(dist = 1000)

      overlay_ext <- sf::st_transform(ext, raster::crs(overlay)) %>% sf::as_Spatial()
      overlay_clip <- raster::crop(overlay, overlay_ext)

      overlay_clip_proj <- raster::projectRaster(from = overlay_clip,
                                         to = DEM_adjusted,
                                         method = "ngb")
      select_rast_stars <- stars::st_as_stars(overlay_clip_proj)

      select_rast_sf <- sf::st_as_sf(select_rast_stars,
                                 as_points = FALSE,
                                 merge = TRUE,
                                 connect8 = T)
    }

    if(class(overlay)[1] != "RasterLayer"){
      select_rast_sf <- overlay %>% sf::st_transform(crs=raster::crs(DEM_adjusted))
    }


    clipped_elev <- raster::mask(DEM_adjusted, select_rast_sf)
    select_elev <- quantile(clipped_elev[], overlay_quantile, na.rm=T)
    rast_name <- names(overlay)

    select_nodes <- nodes %>%
      sf::st_join(select_rast_sf %>% sf::st_transform(sf::st_crs(nodes))) %>%
      dplyr::mutate(join_col = !!sym(rast_name)) %>%
      dplyr::select(-c(!!rast_name)) %>%
      dplyr::mutate(join_col = ifelse(is.na(join_col), F, T)) %>%
      dplyr::filter(join_col == T)

    impacted_nodes <- propagate_flood(pipes = model[[1]], nodes= model[[2]], structures = model[[3]], select_nodes = select_nodes, water_elevation = select_elev)

    impacted_nodes <- impacted_nodes %>%
      mutate(node_fill_height = if_else(units::set_units(select_elev,units(inv_elev)$numerator) > inv_elev, (units::set_units(select_elev,units(inv_elev)$numerator) + abs(inv_elev)) - (inv_elev + abs(inv_elev)),units::set_units(0,units(inv_elev)$numerator)),
             node_perc_fill = if_else(units::set_units(select_elev,units(inv_elev)$numerator) < elev,(node_fill_height * 100)/((elev + abs(inv_elev)) - (inv_elev + abs(inv_elev))), 100))

    impacted_structures <- structures %>%
      dplyr::filter(structureID %in% impacted_nodes$structureID) %>%
      dplyr::mutate(structure_fill_height = dplyr::if_else(units::set_units(select_elev,units(s_inv_elev)$numerator) > s_inv_elev,(units::set_units(select_elev,units(s_inv_elev)$numerator) + abs(s_inv_elev)) - (s_inv_elev + abs(s_inv_elev)),units::set_units(0,units(s_inv_elev)$numerator)),
             structure_perc_fill = dplyr::if_else(units::set_units(select_elev,units(s_inv_elev)$numerator) < s_elev,(structure_fill_height * 100)/((s_elev + abs(s_inv_elev)) - (s_inv_elev + abs(s_inv_elev))), 100),
             structure_surcharge = dplyr::if_else((units::set_units(select_elev,units(s_inv_elev)$numerator)) > s_elev, (units::set_units(select_elev,units(s_inv_elev)$numerator) - s_elev), (units::set_units(0,units(s_inv_elev)$numerator))))

    np_structures <- structures %>%
      dplyr::filter(structureID %in% select_nodes$structureID) %>%
      dplyr::mutate(structure_fill_height = dplyr::if_else(units::set_units(select_elev,units(s_inv_elev)$numerator) > s_inv_elev,(units::set_units(select_elev,units(s_inv_elev)$numerator) + abs(s_inv_elev)) - (s_inv_elev + abs(s_inv_elev)),units::set_units(0,units(s_inv_elev)$numerator)),
             structure_perc_fill = dplyr::if_else(units::set_units(select_elev,units(s_inv_elev)$numerator) < s_elev,(structure_fill_height * 100)/((s_elev + abs(s_inv_elev)) - (s_inv_elev + abs(s_inv_elev))), 100),
             structure_surcharge = dplyr::if_else((units::set_units(select_elev,units(s_inv_elev)$numerator)) > s_elev, (units::set_units(select_elev,units(s_inv_elev)$numerator) - s_elev), (units::set_units(0,units(s_inv_elev)$numerator))))


    impacted_pipes <- pipes[pipes$edgeID %in% impacted_nodes$edgeID, ]

    total_impacted_nodes <- rbind(total_impacted_nodes, impacted_nodes %>% tibble::add_column(water_elevation = select_elev))
    total_np_nodes <- rbind(total_np_nodes, select_nodes %>% tibble::add_column(water_elevation = select_elev))
    total_impacted_structures <- rbind(total_impacted_structures, impacted_structures %>% tibble::add_column(water_elevation = select_elev))
    total_np_structures <- rbind(total_np_structures, np_structures %>% tibble::add_column(water_elevation = select_elev))
    total_impacted_pipes <- rbind(total_impacted_pipes, impacted_pipes %>% tibble::add_column(water_elevation = select_elev))
    total_flooding <- rbind(total_flooding, select_rast_sf %>% tibble::add_column(water_elevation = select_elev))

    return(list(pipes = total_impacted_pipes,
                nodes = total_impacted_nodes,
                np_nodes =total_np_nodes,
                structures = total_impacted_structures,
                np_structures = total_np_structures,
                flooding = total_flooding,
                overlay = overlay_clip_proj))
  }
}
