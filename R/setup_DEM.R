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
#' @param minimum_area km^2. Minimum area of flooding to keep as "downstream flooding"
#' @param workspace Path to bathtub folder
#' @param overwrite Logical. Overwrite existing data?

#' @return DEM clipped to stormwater network, reprojected to projection
#' of \code{pipes}, and values converted from NAVD88 to MHHW
#' @examples
#'# site_DEM <- bathtub::DEM_setup(
#'# pipes = site_pipes,
#'# large_DEM = NC_DEM,
#'# conversion_raster = NAVD88_MHHW_conv,
#'# res = 5,
#'# workspace = site_workspace
#'# )


DEM_setup <-
  function(pipes,
           other_network_layers = NULL,
           large_DEM,
           elev_units = "m",
           conversion_raster,
           res,
           trace_upstream = F,
           trace_buffer = 1.5,
           extent_buffer = 1000,
           minimum_area = 2,
           workspace,
           overwrite = F) {

    #Conversion factor to make large_DEM units match conv_factor (always "m")
    units::units_options(set_units_mode = "standard")
    conv_fac <-  units::set_units(units::set_units(1, value = elev_units),value = "m") %>% units::drop_units()
    minimum_area <- units::set_units(minimum_area, "km^2")

    if(class(large_DEM)[1] != "SpatRaster"){
      large_DEM <-  terra::rast(large_DEM)
    }

    if(class(conversion_raster)[1] != "SpatRaster"){
      conversion_raster <- terra::rast(conversion_raster)
    }

    #Ff there are other network layer (culverts, virtual drainlines, etc.), they will be combined with pipes here using only spatial attributes
    if(!is.null(other_network_layers)) {
      cat("Combining all network layers...\n")

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
    extent_sf <- pipes %>% sf::st_make_grid(n=1) %>% sf::st_buffer(dist = extent_buffer) %>%  sf::as_Spatial()
    final_extent <-terra::vect(extent_sf)

    final_extent_bbox <- extent_sf %>% sf::st_as_sf() %>% sf::st_bbox()

    xmin <- final_extent_bbox[1]
    xcells <- ceiling((final_extent_bbox[3] - final_extent_bbox[1])/res)
    xmax <- xmin + (xcells * res)

    ymin <- final_extent_bbox[2]
    ycells <- ceiling((final_extent_bbox[4] - final_extent_bbox[2])/res)
    ymax <- ymin + (ycells * res)

    final_extent_rast <- terra::rast(xmin = xmin,
                                     xmax = xmax,
                                     ymin = ymin,
                                     ymax = ymax,
                                     ncols = xcells,
                                     nrows = ycells,
                                     crs = terra::crs(final_extent))

    #reproject extent to crs of large_DEM, clip large_DEM, convert crs to pipe crs
    if (base::file.exists(base::paste0(workspace, "/DEMs/elev_clip_proj.tif")) & overwrite == F) {
      elev_navd_small_proj <-
        terra::rast(base::paste0(workspace, "/DEMs/elev_clip_proj.tif"))
    }

    if (!base::file.exists(base::paste0(workspace, "/DEMs/elev_clip_proj.tif")) | overwrite == T) {
      cat("Reprojecting & cropping DEM...\n")

      elev_ext <- terra::project(final_extent, terra::crs(large_DEM))
      elev_navd_small <- terra::crop(large_DEM, elev_ext, filename = base::paste0(workspace, "/DEMs/elev_clip.tif"), overwrite=T)

      elev_navd_small_proj <-
        terra::project(
          x = elev_navd_small,
          y = final_extent_rast,
          method = "bilinear",
          # mask = T,
          filename = base::paste0(workspace, "/DEMs/elev_clip_proj.tif"),
          overwrite = T
        )

    }

    #reproject extent to crs of conversion_raster,  clip conversion_raster, convert crs to pipe crs
    if (base::file.exists(base::paste0(workspace, "/DEMs/conv_clip_proj.tif")) & overwrite == F) {
      conversion_small_proj <-
        terra::rast(paste0(workspace, "/DEMs/conv_clip_proj.tif"))
    }

    if (!base::file.exists(base::paste0(workspace, "/DEMs/conv_clip_proj.tif")) | overwrite == T) {
      cat("Reprojecting and cropping conversion raster...\n")

      conv_ext <- terra::project(final_extent, terra::crs(conversion_raster))
      conv_small <- terra::crop(conversion_raster, conv_ext, filename = base::paste0(workspace, "/DEMs/conv_clip.tif"), overwrite=T)

      conversion_small_proj <-
        terra::project(
          x = conv_small,
          y = final_extent_rast,
          filename = base::paste0(workspace, "/DEMs/conv_clip_proj.tif"),
          overwrite = T
        )

      # conversion_small_proj <-
      #   terra::resample(
      #     x = conversion_small_proj,
      #     y = elev_navd_small_proj,
      #     filename = base::paste0(workspace, "/DEMs/conv_clip_proj.tif"),
      #     overwrite = T
      #   )
    }

    #create MHHW elevation raster
    if (base::file.exists(base::paste0(workspace, "/DEMs/MHHW_clip_proj.tif")) & overwrite == F) {
      elev <-
        terra::rast(base::paste0(workspace, "/DEMs/MHHW_clip_proj.tif"))
    }

    if (!base::file.exists(base::paste0(workspace, "/DEMs/MHHW_clip_proj.tif")) | overwrite == T) {
      cat("Creating MHHW DEM...\n")

      conv_fac_rast <- elev_navd_small_proj
      values(conv_fac_rast) <- conv_fac

      conv_function <- function(x, y, z){x - (y/z)}

      # elev_navd_small_proj <- terra::expand(elev_navd_small_proj, conversion_small_proj)
      # conversion_small_proj <- terra::expand(conversion_small_proj,elev_navd_small_proj)

      elev <-
        terra::lapp(
          x = c(elev_navd_small_proj,
          conversion_small_proj,
          conv_fac_rast),
          fun = conv_function,
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
        new_elev <- terra::rast(base::paste0(workspace, "/DEMs/MHHW_clip_proj_traced.tif"))
        return(new_elev)
      }

      if (!base::file.exists(base::paste0(workspace, "/DEMs/MHHW_clip_proj_traced.tif")) | overwrite == T) {
        # elev[elev<-5] <- NA
        # water_level <- elev < 0
        # water_level[water_level == 0] <- NA

        cat("Tracing MHHW conversion upstream...\n")
        # select_rast <- terra::app(
        #   x = elev,
        #   fun = function(x) {
        #     x < 0
        #   }
        # )
        select_rast <- elev < 0
        select_rast[select_rast == 0] <- NA

        select_rast_stars <- stars::st_as_stars(raster::raster(select_rast), crs = sf::st_crs(select_rast))

        select_rast_sf <- sf::st_as_sf(select_rast_stars,
                                       as_points = FALSE,
                                       merge = T,
                                       connect8 = T) %>%
          mutate(area = sf::st_area(.)) %>%
          filter(area > minimum_area)

        # clumps <- raster::clump(water_level,directions=8)
        #
        # clump_freq <- freq(clumps)
        # clump_freq_tibble <- tibble::as_tibble(clump_freq) %>%
        #   dplyr::mutate(count_km2 = (count * (res(water_level)[1]^2))/1.076e+7) %>%
        #   dplyr::filter(count_km2 > minimum_area) %>%
        #   stats::na.omit()
        #
        # large_select_raster <- clumps %in% c(clump_freq_tibble$value)
        # large_select_raster[large_select_raster == 0] <- NA

        # large_select_raster <- raster::projectRaster(large_select_raster,
        #                                              final_extent,
        #                                              filename = base::paste0(workspace, "/DEMs/water_level.tif"),
        #                                              overwrite = T)

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

        colnames(select_rast_sf)[1] <- "lyr1"

        point_extract <- point_extract %>%
          sf::st_join(select_rast_sf) %>%
          dplyr::select(-area) %>%
          dplyr::mutate(conv = raster::extract(raster::raster(conversion_small_proj),point_extract),
                 DEM = raster::extract(raster::raster(elev_navd_small_proj), point_extract)) %>%
          tibble::as_tibble() %>%
          dplyr::filter(lyr1 == 1) %>%
          dplyr::group_by(ID) %>%
          dplyr::summarise(conv = median(conv))

        pipe_merge_test <- pipe_merge_test %>%
          dplyr::left_join(point_extract, by = "ID") %>%
          filter(!is.na(conv))

        vx <- velox::velox(raster::raster(conversion_small_proj))

        vx$rasterize(pipe_merge_test, field = "conv", band = 1)

        rl <- vx$as.RasterLayer(band=1)

        # rast_conv_traced <- fasterize::fasterize(pipe_merge_test, raster = conversion_small_proj, field = "conv")
        #
        # writeRaster(rl, filename = base::paste0(workspace,"/DEMs/traced_conversion_rast.tif"), overwrite = T)

        # rl <- raster::rasterize(pipe_merge_test,
        #                          conversion_small_proj,
        #                          field = "conv",
        #                          update = T,
        #                          filename = base::paste0(workspace,"/DEMs/traced_conversion_rast.tif"), overwrite = T)

        cat("Creating MHHW DEM with traced conversion...\n")

        rl <- terra::rast(rl)

        new_elev <- elev_navd_small_proj - rl
          # terra::lapp(
          #   x = c(elev_navd_small_proj,rl),
          #   fun = function(x) {
          #     x[[1]] - x[[2]]
          #   },
          #   filename = base::paste0(workspace, "/DEMs/MHHW_clip_proj_traced.tif"),
          #   overwrite = T
          # )

        return(new_elev)
      }
    }
  }
