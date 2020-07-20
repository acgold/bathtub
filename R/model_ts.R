model_ts <- function(model,
                     elev,
                     elev_units,
                     ts,
                     time_step,
                     ts_datum = "MHHW",
                     ts_units = "m",
                     use_outlets = T,
                     minimum_area = 0.6,
                     minimum_area_units = "km^2",
                     min_elev_cutoff = -5,
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

  if(ts_datum == "NAVD88"){
    stop("bathtub is currently only able to use MHHW time series data. Please tranform your data to MHHW.")
  }

  wl_conv_fac <- 1 / units::drop_units(units::set_units(units::set_units(1, ts_units), units(pipes$from_inv_elev)$numerator))

  ts <- tibble::as_tibble(ts)
  ts[,2] <- ts[,2] / wl_conv_fac

  max_wl <- ceiling(max(ts[,2], na.rm=T))
  min_wl <- floor(min(ts[,2], na.rm=T))
  step = 2/12

    cat("Using provided DEM and water level time series to estimate flooding extent...\n")

    elev_seq = units::set_units(seq(from = min_wl, to = max_wl, by = step), value = units(pipes$from_inv_elev)$numerator)

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

      select_rast_stars <- stars::st_as_stars(select_rast)

      select_rast_sf <- sf::st_as_sf(select_rast_stars,
                                     as_points = FALSE,
                                     merge = TRUE,
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
#
#     sf::st_write(obj = total_impacted_nodes, dsn = paste0(workspace, "/structures/", site_name, "_imp_nodes.shp"), delete_layer = overwrite, quiet = T)
#     sf::st_write(obj = total_impacted_structures, dsn = paste0(workspace, "/structures/", site_name, "_imp_struc.shp"), delete_layer = overwrite, quiet = T)
#     sf::st_write(obj = total_np_structures, dsn = paste0(workspace, "/structures/", site_name, "_np_struc.shp"), delete_layer = overwrite, quiet = T)
#     sf::st_write(obj = total_impacted_pipes, dsn = paste0(workspace, "/structures/", site_name, "_imp_pipes.shp"), delete_layer = overwrite, quiet = T)
#     sf::st_write(obj = total_flooding, dsn = paste0(workspace, "/structures/", site_name, "_flooding_extent.shp"), delete_layer = overwrite, quiet = T)
    spatial_list <- list(
      pipes = total_impacted_pipes,
      nodes = total_impacted_nodes,
      np_nodes = total_np_nodes,
      structures = total_impacted_structures,
      np_structures = total_np_structures,
      flooding = total_flooding
    )


    return(spatial_list)
}
