#' Setup pipes for bathtub inundation modeling
#'
#' @param pipes A \code{sf} object denoting stormwater pipes
#' @param type Type of invert survey data. 'depth', 'elevation', or 'none'
#' @param up_invert Name of column denoting upstream invert survey data
#' @param dn_invert Name of column denoting downstream invert survey data
#' @param null_value Invert survey data value indicating missing data
#' @param up_elev Name of column denoting surface elevation of upstream pipe ends
#' @param dn_elev Name of column denoting surface elevation of downstream pipe ends
#' @param elev_units Units of \code{up/dn_invert} and \code{up/dn_elev}
#' @param vdatum Vertical datum of elevation data (only NAVD88 supported)
#' @param diam Name of column denoting pipe diameter
#' @param diam_units Units of \code{diam}
#' @param other_cols Other columns to preserve
#' @param workspace Path to bathtub folder

#' @return An \code{sf} object denoting pipes formatted with units
#' @examples
#'pipes_n <-
#'setup_pipes(pipes,
#'            type = "none",
#'            diam = "DIAMETER",
#'            diam_units = "in")

setup_pipes <- function(pipes,
                        type = "depth",
                        up_invert,
                        dn_invert,
                        null_value = 99,
                        invert_units = "ft",
                        up_elev = NA,
                        dn_elev= NA,
                        elev_units = "ft",
                        vdatum = "NAVD88",
                        diam,
                        diam_units,
                        other_cols = NULL,
                        workspace) {
  units::units_options(set_units_mode = "standard")

  null_value <- units::set_units(null_value, value = invert_units)

  if(type == "depth"){
    cols <- c(from_depth = NA,
              to_depth = NA,
              from_inv_elev = NA,
              to_inv_elev = NA,
              from_elev = NA,
              to_elev = NA,
              diam_raw = NA)

    pipes_new <- pipes %>%
      dplyr::select(from_depth = !!up_invert,
                    to_depth =!!dn_invert ,
                    diam_raw = !!diam,
                    all_of(other_cols)) %>%
      dplyr::mutate(from_elev = if_else(!is.na(!!up_elev), !!up_elev, up_elev),
             to_elev = if_else(!is.na(!!dn_elev), !!dn_elev, dn_elev)) %>%
      tibble::add_column(!!!cols[!names(cols) %in% names(.)]) %>%
      dplyr::mutate(
        edgeID = c(1:n()),
        diam_raw = as.numeric(as.character((diam_raw))),
        diam = units::set_units(as.numeric(as.character(diam_raw)), !!diam_units),
        from = NA,
        to = NA,
        from_elev = if_else(!is.na(from_elev),units::set_units(as.numeric(as.character(from_elev)),!!elev_units),units::set_units(NA,!!elev_units)),
        to_elev = if_else(!is.na(to_elev),units::set_units(as.numeric(as.character(to_elev)),!!elev_units),units::set_units(NA,!!elev_units)),
        from_depth = na_if(units::set_units(as.numeric(as.character(from_depth)),!!invert_units), null_value),
        to_depth = na_if(units::set_units(as.numeric(as.character(to_depth)),!!invert_units), null_value),
        from_inv_elev = units::set_units(NA, !!invert_units),
        to_inv_elev = units::set_units(NA, !!invert_units),
        length = sf::st_length(.)
      ) %>%
      dplyr::select(edgeID, diam, from, to, from_elev, to_elev, from_depth, to_depth, from_inv_elev, to_inv_elev, length, all_of(!!other_cols))

  }

  if(type == "elevation"){
    cols <- c(from_inv_elev = NA,
              to_inv_elev = NA,
              from_elev = NA,
              to_elev = NA,
              diam_raw = NA)

    pipes_new <- pipes %>%
      dplyr::select(from_inv_elev = !!up_invert,
                    to_inv_elev =!!dn_invert ,
                    diam_raw = !!diam,
                    all_of(other_cols)) %>%
      dplyr::mutate(from_elev = if_else(!is.na(!!up_elev), !!up_elev, up_elev),
             to_elev = if_else(!is.na(!!dn_elev), !!dn_elev, dn_elev)) %>%
      tibble::add_column(!!!cols[!names(cols) %in% names(.)]) %>%
      dplyr::mutate(
        edgeID = c(1:n()),
        diam_raw = as.numeric(as.character((diam_raw))),
        diam = units::set_units(as.numeric(as.character(diam_raw)), !!diam_units),
        from = NA,
        to = NA,
        from_elev = units::set_units(as.numeric(as.character(from_elev)),!!elev_units),
        to_elev =  units::set_units(as.numeric(as.character(to_elev)),!!elev_units),
        from_inv_elev = na_if(units::set_units(as.numeric(as.character(from_inv_elev)),!!invert_units), null_value),
        to_inv_elev = na_if(units::set_units(as.numeric(as.character(to_inv_elev)),!!invert_units), null_value),
        length = sf::st_length(.)
      ) %>%
      dplyr::select(edgeID, diam, from, to, from_elev, to_elev, from_inv_elev, to_inv_elev, length, all_of(!!other_cols))


    if(vdatum == "NAVD88") {
      #convert NAVD88 user-supplied elevation data to MHHW
      pipes_new <- pipes_new %>%
        dplyr::mutate(
          conv_mhhw = units::set_units(
            raster::extract(
              raster::raster(base::paste0(workspace, "/DEMs/conv_clip_proj.tif")),
              pipes_new %>%
                sf::st_cast(., "LINESTRING") %>%
                sf::st_cast(., "POINT") %>%
                tibble::as_tibble() %>%
                dplyr::group_by(edgeID) %>%
                dplyr::slice(c(2)) %>%
                dplyr::ungroup() %>%
                sf::st_as_sf()
            ) * 3.28084,
            value = "ft"
          ),
          from_elev = units::set_units(from_elev, value = !!invert_units) - conv_mhhw,
          from_inv_elev = units::set_units(from_inv_elev, value = !!invert_units) - conv_mhhw,
          to_elev = units::set_units(to_elev, value = !!invert_units) - conv_mhhw,
          to_inv_elev = units::set_units(to_inv_elev, value = !!invert_units) - conv_mhhw
        )
    }
  }

  if(type == "none"){
    cols <- c(from_depth = NA,
              to_depth = NA,
              from_inv_elev = NA,
              to_inv_elev = NA,
              from_elev = NA,
              to_elev = NA,
              diam_raw = NA)

    pipes_new <- pipes %>%
      dplyr::select(diam_raw = !!diam,
                    dplyr::all_of(other_cols)) %>%
      tibble::add_column(!!!cols[!names(cols) %in% names(.)]) %>%
      dplyr::mutate(
        edgeID = c(1:n()),
        diam_raw = as.numeric(as.character((diam_raw))),
        diam = units::set_units(as.numeric(as.character(diam_raw)), !!diam_units),
        from = NA,
        to = NA,
        from_elev = units::set_units(as.numeric(as.character(from_elev)),!!elev_units),
        to_elev =  units::set_units(as.numeric(as.character(to_elev)),!!elev_units),
        from_depth = na_if(units::set_units(as.numeric(as.character(from_depth)),!!invert_units), null_value),
        to_depth = na_if(units::set_units(as.numeric(as.character(to_depth)),!!invert_units), null_value),
        from_inv_elev = units::set_units(NA, !!invert_units),
        to_inv_elev = units::set_units(NA, !!invert_units),
        length = sf::st_length(.)
      ) %>%
      dplyr::select(edgeID, diam, from, to, from_elev, to_elev, from_depth, to_depth, from_inv_elev, to_inv_elev, length, all_of(!!other_cols))

  }

  return(pipes_new)
}
