#' Download NOAA SLR DEM
#'
#' @param x A \code{sf} object denoting stormwater pipes used to calculate extent
#' @param x_EPSG EPSG code of \code{x}
#' @param DEM_EPSG EPSG code of returned DEM. Default is same as \code{x_EPSG}
#' @param res Desired resolution of DEM in map units of \code{x}
#' @param min_elev_cutoff Minimum elevation to keep (meters). NOAA SLR DEM uses -99 to denote
#' permanent water surfaces
#' @param workspace Path to bathtub folder
#' @return NOAA SLR DEM clipped to stormwater network extent, reprojected to projection
#' of \code{x}
#' @examples
#'noaa_elev <- get_NOAA_elevation(x = pipes, workspace = workspace)

get_NOAA_elevation <-
  function(x,
           x_EPSG = 102719,
           DEM_EPSG = x_EPSG,
           res = 9.81,
           min_elev_cutoff = -5,
           workspace) {

    x_bbox <- x %>%
      sf::st_make_grid(n=1) %>%
      sf::st_buffer(dist = 1000)%>%
      sf::st_bbox()

  x_cell_num <- 2 * (round(((x_bbox[3] - x_bbox[1])/res)/2))
  y_cell_num <- 2 * (round(((x_bbox[4] - x_bbox[2])/res)/2))

  if(x_cell_num < 4000 & y_cell_num < 14000){
    api_url <- "https://coast.noaa.gov/arcgis/rest/services/Elevation/SLR_DEMs/ImageServer/exportImage?"
    url <- base::paste0(api_url,
                  "&bbox=",base::paste(x_bbox[1],x_bbox[2],x_bbox[3],x_bbox[4],sep = ","),
                  "&bboxSR=",x_EPSG,
                  "&imageSR=",DEM_EPSG,
                  "&format=tiff",
                  "&f=image",
                  "&size=",base::paste(x_cell_num, y_cell_num,sep=","))


    base::cat("Downloading raster from image service...")
    Sys.sleep(1/100)
    tmpfile <- base::tempfile()
    resp <- httr::GET(url,httr::write_disk(tmpfile,overwrite=TRUE),httr::progress())
    if (!base::grepl("image/tif", httr::http_type(resp))) {
      stop(base::paste("This url:", url,"did not return a tif"), call. = FALSE) #from elevatR
    }

    z <- raster::raster(tmpfile)
    z[z < min_elev_cutoff] <- min_elev_cutoff

    base::cat(paste0("\nSaving raster to: ",workspace,"/DEMs/NOAA_SLR_DEM.tif"))
    new_z <- raster::writeRaster(z, filename = base::paste0(workspace,"/DEMs/NOAA_SLR_DEM.tif"), overwrite = T)

    base::cat("\nDEM resolution is: ",raster::res(new_z)[1],sub(".*=",'',stringr::str_extract(raster::projection(new_z),"units=([^\\s]+\\s+){1}")))
    base::cat("\nVertical datum is NAVD88 (meters)")
    base::unlink(tmpfile)

    return(new_z)
  }

  if(x_cell_num > 4000 | y_cell_num > 14000){
    base::cat("Tiling input extent for image service requests...\n")

    if(x_cell_num > 4000 & y_cell_num < 14000){
      new_x_cell_num <- x_cell_num/2
      new_y_cell_num <-  y_cell_num/2

      if(new_x_cell_num > 4000){
        stop("Requested area too wide. Decrease resolution or width of extent.\n")
      }

      if(new_y_cell_num > 14000){
        stop("Requested area too tall Decrease resolution or height of extent.\n")
      }

      tiles <- x %>%
        sf::st_make_grid(n=1) %>%
        sf::st_buffer(dist = 1000) %>%
        sf::st_make_grid(n=2)

      new_rasts <- foreach(i = 1:length(tiles)) %do% {
        new_bbox <- tiles[i,] %>% sf::st_bbox()

        api_url <- "https://coast.noaa.gov/arcgis/rest/services/Elevation/SLR_DEMs/ImageServer/exportImage?"
        url <- base::paste0(api_url,
                      "&bbox=",base::paste(new_bbox[1],new_bbox[2],new_bbox[3],new_bbox[4],sep = ","),
                      "&bboxSR=",x_EPSG,
                      "&imageSR=",DEM_EPSG,
                      "&format=tiff",
                      "&f=image",
                      "&size=",base::paste(new_x_cell_num, new_y_cell_num,sep=","))


        base::cat("Downloading raster",paste0("(",i,"/",length(tiles),")"),"from image service...\n")

        tmpfile <- base::tempfile()
        resp <- httr::GET(url,httr::write_disk(tmpfile,overwrite=TRUE),httr::progress())
        if (!base::grepl("image/tif", httr::http_type(resp))) {
          stop(base::paste("This url:", url,"did not return a tif"), call. = FALSE) #from elevatR
        }

        raster::raster(tmpfile)
      }


      origins<-t(data.frame(lapply(new_rasts,raster::origin)))
      min_origin<-c(min(origins[,1]),min(origins[,2]))
      change_origins <- function(x,y){
        raster::origin(x)<-y
        x
      }
      new_rasts <- lapply(new_rasts, function(x,y) change_origins(x,min_origin))

      set_minimum <- function(x, minimum_elev){
        x[x < minimum_elev] <- minimum_elev
        x
      }
      new_rasts <- lapply(new_rasts, function(x,y) set_minimum(x,min_elev_cutoff))

      new_rasts$filename <- base::paste0(workspace,"/DEMs/NOAA_SLR_DEM.tif")
      new_rasts$overwrite <- T
      base::cat(paste0("\nSaving merged raster to: \n",workspace,"/DEMs/NOAA_SLR_DEM.tif"))
      mos <- do.call(raster::merge, new_rasts)

      base::cat("\nDEM resolution is: ",raster::res(mos)[1],sub(".*=",'',stringr::str_extract(raster::projection(mos),"units=([^\\s]+\\s+){1}")))
      base::cat("\nVertical datum is NAVD88 (meters)")
      base::unlink(tmpfile)

      return(mos)
    }
  }
}
