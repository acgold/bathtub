#' Raster with conversion factor from NAVD88 to MHHW for North Carolina, USA.
#'
#'Values are in meters. To convert from NAVD88 to MHHW tidal datum,
#'subtract the conversion raster from a DEM with NAVD88 elevation values.
#'
#' @format RasterLayer:
#' \describe{
#'   \item{dimensions}{6197 X 6775 pixels}
#'   \item{resolution}{50 meter}
#'   \item{crs/projection}{UTM zone 18}
#'   \item{value range}{-0.99, 0.7606}
#'   ...
#' }
#' @source \url{https://www.fisheries.noaa.gov/inport/item/48104}
"NAVD88_MHHW_NC"
