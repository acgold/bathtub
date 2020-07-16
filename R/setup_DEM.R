#' Setup DEM for inundation modeling
#'
#' @param pipes A \code{sf} object denoting stormwater pipes.
#' @param other_network_layer Vector of \code{sf} objects to be included in network
#' @param large_DEM \code{RasterLayer} DEM to be processed
#' @param conversion_raster \code{RasterLayer} Conversion from NAVD88 to MHHW
#' @param res Desired spatial resolution of output DEM (meters)
#' @param trace_upstream Logical. Trace NAVD88/MHHW conversion factor upstream?
#' @param trace_buffer Buffer distance (map units of pipes) around pipes
#' to search for receiving waters
#' @param minimum_area km^2. Minimum area of flooding to keep
#' @param workspace Path to floodr folder
#' @param overwrite Logical. Overwrite existing data?

#' @return DEM clipped to stormwater network, reprojected to projection
#' of \code{pipes}, and values converted from NAVD88 to MHHW
#' @examples
#'site_DEM <- floodr::DEM_setup(
#'pipes = site_pipes,
#'large_DEM = NC_DEM,
#'conversion_raster = NAVD88_MHHW_conv,
#'res = 5,
#'workspace = site_workspace
#' )


DEM_setup <-
  function(pipes,
           other_network_layers = NULL,
           large_DEM,
           elev_units = "m",
           conversion_raster,
           res = 9.81,
           trace_upstream = F,
           trace_buffer = 1.5,
           extent_buffer = 1000,
           minimum_area = 2,
           workspace,
           overwrite = F) {

    #Conversion factor to make large_DEM units match conv_factor (always "m")
    units::units_options(set_units_mode = "standard")
    conv_fac <-  units::set_units(units::set_units(1, value = elev_units),value = "m") %>% units::drop_units()

    #Ff there are other network layer (culverts, virtual drainlines, etc.), they will be combined with pipes here using only spatial attributes
    if(!is.null(other_network_layers)) {
      num <- length(other_network_layers)
      list_other_layers <- list(NULL)

      for (i in 1:num) {
        list_other_layers[[i]] <-
          get(other_network_layers[i]) %>% dplyr::select()
      }

      pipes_empty <- pipes %>% dplyr::select()
      for (i in 1:num) {
        pipes_empty <- base::rbind(pipes_empty, list_other_layers[[i]])
      }

      pipes <- pipes_empty
    }

    #Create extents for clipping using generous 1000 ft butter
    ext <- sf::st_make_grid(pipes, n = 1) %>% sf::st_buffer(dist = extent_buffer)
    final_extent <- raster::raster(ext %>% sf::as_Spatial(), res = res)

    #reproject extent to crs of large_DEM, clip large_DEM, convert crs to pipe crs
    if (base::file.exists(base::paste0(workspace, "/DEMs/elev_clip_proj.tif")) & overwrite == F) {
      elev_navd_small_proj <-
        raster::raster(base::paste0(workspace, "/DEMs/elev_clip_proj.tif"))
    }

    if (!base::file.exists(base::paste0(workspace, "/DEMs/elev_clip_proj.tif")) | overwrite == T) {
      elev_ext <- sf::st_transform(ext, raster::crs(large_DEM)) %>% sf::as_Spatial()
      elev_navd_small <- raster::crop(large_DEM, elev_ext)

      elev_navd_small_proj <-
        raster::projectRaster(
          from = elev_navd_small,
          to = final_extent,
          filename = base::paste0(workspace, "/DEMs/elev_clip_proj.tif"),
          overwrite = T
        )
    }

    #reproject extent to crs of conversion_raster,  clip conversion_raster, convert crs to pipe crs
    if (base::file.exists(base::paste0(workspace, "/DEMs/conv_clip_proj.tif")) & overwrite == F) {
      conversion_small_proj <-
        raster::raster(paste0(workspace, "/DEMs/conv_clip_proj.tif"))
    }

    if (!base::file.exists(base::paste0(workspace, "/DEMs/conv_clip_proj.tif")) | overwrite == T) {
      conversion_ext <-
        sf::st_transform(ext, raster::crs(conversion_raster)) %>% sf::as_Spatial()
      conversion_small <- raster::crop(conversion_raster, conversion_ext)

      conversion_small_proj <-
        raster::projectRaster(
          from = conversion_small,
          to = final_extent,
          filename = base::paste0(workspace, "/DEMs/conv_clip_proj.tif"),
          overwrite = T
        )
    }

    #create MHHW elevation raster
    if (base::file.exists(base::paste0(workspace, "/DEMs/MHHW_clip_proj.tif")) & overwrite == F) {
      elev <-
        raster::raster(base::paste0(workspace, "/DEMs/MHHW_clip_proj.tif"))
    }

    if (!base::file.exists(base::paste0(workspace, "/DEMs/MHHW_clip_proj.tif")) | overwrite == T) {
      elev <-
        raster::overlay(
          elev_navd_small_proj,
          conversion_small_proj,
          fun = function(x, y) {
            x - (y/conv_fac)
          },
          filename = base::paste0(workspace, "/DEMs/MHHW_clip_proj.tif"),
          overwrite = T
        )
    }

    #Trace upstream if wanting to propagate conversion factor upstream (like for Nags Head)
    if(trace_upstream == F){
      return(elev)
    }

    if(trace_upstream == T){
      if (base::file.exists(paste0(workspace, "/DEMs/MHHW_clip_proj_traced.tif")) & overwrite == F) {
        new_elev <- raster::raster(base::paste0(workspace, "/DEMs/MHHW_clip_proj_traced.tif"))
        return(new_elev)
      }

      if (!base::file.exists(base::paste0(workspace, "/DEMs/MHHW_clip_proj_traced.tif")) | overwrite == T) {
        elev[elev<-5] <- NA
        water_level <- elev < 0
        water_level[water_level == 0] <- NA

        clumps <- raster::clump(water_level,directions=8)

        clump_freq <- freq(clumps)
        clump_freq_tibble <- tibble::as_tibble(clump_freq) %>%
          dplyr::mutate(count_km2 = (count * (res(water_level)[1]^2))/1.076e+7) %>%
          dply::filter(count_km2 > minimum_area) %>%
          stats::na.omit()

        large_select_raster <- clumps %in% c(clump_freq_tibble$value)
        large_select_raster[large_select_raster == 0] <- NA

        large_select_raster <- raster::projectRaster(large_select_raster,
                                                     final_extent,
                                                     filename = base::paste0(workspace, "/DEMs/water_level.tif"),
                                                     overwrite = T)

        pipe_buffer <- sf::st_buffer(pipes, dist = trace_buffer) %>%
          sf::st_union() %>%
          sf::st_cast("POLYGON") %>%
          tibble::as_tibble() %>%
          dplyr::mutate("ID" = row_number()) %>%
          sf::st_as_sf()

        pipe_merge_test <- pipes %>%
          sf::st_join(pipe_buffer) %>%
          sf::st_cast("LINESTRING") %>%
          dplyr::arrange(ID)

        point_extract <- pipe_merge_test %>%
          sf::st_cast("POINT")

        point_extract <- point_extract %>%
          dply::mutate(water = raster::extract(large_select_raster,point_extract),
                 conv = raster::extract(conversion_small_proj,point_extract),
                 DEM = raster::extract(elev_navd_small_proj, point_extract)) %>%
          tibble::as_tibble() %>%
          dplyr::filter(water == 1) %>%
          dplyr::group_by(ID) %>%
          dplyr::summarise(conv = median(conv))

        pipe_merge_test <- pipe_merge_test %>%
          dplyr::left_join(point_extract)

        rl <- raster::rasterize(pipe_merge_test,
                                 conversion_small_proj,
                                 field = "conv",
                                 update = T,
                                 filename = base::paste0(workspace,"/DEMs/traced_conversion_rast.tif"), overwrite = T)

        new_elev <-
          raster::overlay(
            elev_navd_small_proj,
            rl,
            fun = function(x, y) {
              x - y
            },
            filename = base::paste0(workspace, "/DEMs/MHHW_clip_proj_traced.tif"),
            overwrite = T
          )

        return(new_elev)
      }
    }
  }
