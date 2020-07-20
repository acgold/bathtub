#' Setup structures for bathtub inundation modeling
#'
#' @param structures A \code{sf} object denoting stormwater structures
#' @param type Type of invert survey data. 'depth', 'elevation', or 'none'
#' @param invert Name of column denoting invert survey data
#' @param invert_units Units of \code{invert} elevation data
#' @param null_value Invert survey data value indicating missing data
#' @param elev Name of column denoting surface elevation of stormwater structure,
#' or \code{RasterLayer} NAVD88 DEM to extract elevation from
#' @param elev_units Units of \code{elev} values
#' @param vdatum Vertical datum of elevation data (only NAVD88 supported)
#' @param other_cols Other columns to preserve
#' @param workspace Path to bathtub folder
#'
#' @return An \code{sf} object denoting structures formatted with units
#' @examples
#'structures_n <-
#'setup_structures(
#'  structures = structures,
#'  type = "elevation",
#'  invert = "INVERTELEV",
#'  elev = "Elevation",
#'  elev_units = "ft",
#'  null_value = 99,
#'  other_cols = c("Code","Prcnt_Obst","Type_Obst"),
#'  workspace = workspace
#')

setup_structures<-
  function(structures,
           type = "depth",
           invert,
           invert_units = "ft",
           elev = NA,
           elev_units = "m",
           vdatum = "NAVD88",
           null_value = 99,
           other_cols = NULL,
           workspace) {
    units::units_options(set_units_mode = "standard")

    null_value <- units::set_units(null_value, value = invert_units)

    cols <- c(s_inv_elev = NA,
              s_elev = NA)

    if (type == "elevation") {
      if (class(elev)[1] == "RasterLayer") {

        structures_new <-  structures %>%
          dplyr::select(s_inv_elev = !!invert ,
                        dplyr::all_of(other_cols)) %>%
          dplyr::mutate(
            s_elev = units::set_units(
              units::set_units(raster::extract(elev, structures), !!elev_units),
              !!invert_units
            ),
            s_inv_elev = na_if(units::set_units(as.numeric(as.character(s_inv_elev)),!!invert_units),null_value),
            conv_mhhw = units::set_units(
              units::set_units(raster::extract(raster::raster(
                base::paste0(workspace, "/DEMs/conv_clip_proj.tif")
              ), .), value = "m"),
              value = !!invert_units
            ),

            s_inv_elev = s_inv_elev - conv_mhhw) %>%
          dplyr::select(s_elev, s_inv_elev, dplyr::all_of(other_cols)) #nodeID,
      }

      if (is.character(elev)) {
        if (vdatum != "NAVD88") {
          stop("floodR only supports user-supplied elevation in the NAVD88 vertical datum")
        }
        if (vdatum == "NAVD88" &
            !file.exists(paste0(workspace, "/DEMs/conv_clip_proj.tif"))) {
          stop("Run the 'DEM_setup' function to create a conversion raster from NAVD88 to MHHW")
        }
        structures_new <-  structures %>%
          dplyr::select(s_elev = !!elev,
                        s_inv_elev = !!invert ,
                        dplyr::all_of(other_cols)) %>%
          dplyr::mutate(
            # nodeID = c(1:n()),
            s_elev = units::set_units(as.numeric(as.character(s_elev)),!!invert_units),
            s_inv_elev = units::set_units(as.numeric(as.character(s_inv_elev)),!!invert_units),
            conv_mhhw = units::set_units(
              units::set_units(raster::extract(raster::raster(
                base::paste0(workspace, "/DEMs/conv_clip_proj.tif")
              ), .), value = "m"),
              value = !!invert_units
            ),
            s_elev = dplyr::na_if(units::set_units(as.numeric(as.character(s_elev)),!!invert_units), null_value),
            s_inv_elev = dplyr::na_if(units::set_units(as.numeric(as.character(s_inv_elev)),!!invert_units), null_value),

            s_elev = s_elev - conv_mhhw,
            s_inv_elev = s_inv_elev - conv_mhhw
          ) %>%
          dplyr::select(s_elev, s_inv_elev, dplyr::all_of(other_cols))
      }
    }

    if (type == "depth") {
      if (class(elev)[1] == "RasterLayer") {
        structures_new <-  structures %>%
          dplyr::select(s_depth = !!invert ,
                        dplyr::all_of(other_cols)) %>%
          dplyr::mutate(
            s_elev = units::set_units(
              units::set_units(raster::extract(elev, structures), !!elev_units),
              !!invert_units
            ),
            s_depth = dplyr::na_if(units::set_units(as.numeric(as.character(s_depth)),!!invert_units),null_value),
            s_inv_elev = s_elev - s_depth
          ) %>%
          dplyr::select(s_elev, s_inv_elev, dplyr::all_of(other_cols))
      }

      if (is.character(elev)) {
        if (vdatum != "NAVD88") {
          stop("floodR only supports user-supplied elevation in the NAVD88 vertical datum")
        }
        if (vdatum == "NAVD88" &
            !file.exists(paste0(workspace, "/DEMs/conv_clip_proj.tif"))) {
          stop("Run the 'DEM_setup' function to create a conversion raster from NAVD88 to MHHW")
        }
        structures_new <-  structures %>%
          dplyr::select(s_elev = !!elev,
                        s_depth = !!invert ,
                        dplyr::all_of(other_cols)) %>%
          dplyr::mutate(
            # nodeID = c(1:n()),
            s_elev = dplyr::na_if(units::set_units(as.numeric(as.character(s_elev)),!!invert_units), null_value),
            s_depth = dplyr::na_if(units::set_units(as.numeric(as.character(s_depth)),!!invert_units), null_value),
            s_inv_elev = s_elev - s_depth,
            conv_mhhw = units::set_units(
              units::set_units(raster::extract(raster::raster(
                base::paste0(workspace, "/DEMs/conv_clip_proj.tif")
              ), .), value = "m"),
              value = !!invert_units
            ),
            s_elev = s_elev - conv_mhhw,
            s_inv_elev = s_inv_elev - conv_mhhw
          ) %>%
          dplyr::select(s_elev, s_inv_elev, dplyr::all_of(other_cols)) #nodeID,
      }
    }
    return(structures_new)
  }
