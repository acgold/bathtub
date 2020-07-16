#' Download USGS 3DEP DEM (1/9 arc second ~ 10 m)
#'
#' @param x A \code{sf} object denoting stormwater pipes used to calculate extent.
#' @param topobathy Select only topographic+bathymetric data?
#' @param view View available data tile extent to assist in selecting data?
#' @return USGS 3DEP 1/9 arc-second DEM clipped to stormwater network extent, reprojected to projection
#' of \code{x}.
#' @examples
#'usgs_elev <- get_3DEP_elevation(x = pipes, topobathy = T, view = T)

get_3DEP_elevation <- function(x,
                               topobathy = F,
                               view = F) {

  pipe_bbox <- x %>% sf::st_transform(crs = 4269) %>% sf::st_bbox()
  cent_new <- c(mean(pipe_bbox[c(1,3)]), mean(pipe_bbox[c(2,4)]))

  api_url <- "https://viewer.nationalmap.gov/tnmaccess/api/products?"
  request <- httr::GET(url = api_url,
                 query = list(
                   datasets="National+Elevation+Dataset+%28NED%29+1%2F9+arc-second",
                   bbox = base::paste(pipe_bbox[1],pipe_bbox[2],pipe_bbox[3],pipe_bbox[4],sep = ","),
                   outputFormat="json",
                   # rasterIds = 1,
                   # geometryType = "esriGeometryEnvelope",
                   bboxSR = 4269)
  )

  response <- httr::content(request, as = "text", encoding = "UTF-8")
  tib <- jsonlite::fromJSON(response, flatten = TRUE)$items %>%
    tibble::as_tibble() %>%
    tibble::rowid_to_column("ID")

  if(nrow(tib) == 0){
    stop("No elevation tiles are currently available for this dataset")
  }

  if(topobathy == T){
    tib <- tib %>% dplyr::filter(stringr::str_detect(title, pattern = "topobathy"))

    if(nrow(tib) == 0){
      stop("No topobathy tiles available. Re-run with 'topobathy = F'")
    }
  }

  if(tib %>% dplyr::tally() > tib %>%
     dplyr::select(boundingBox.minX,
                   boundingBox.maxX,
                   boundingBox.minY,
                   boundingBox.maxY) %>% dplyr::distinct() %>% dplyr::tally()){

    warning("Multiple tiles have the same extent. Select tiles to exclude")
  }

  if(tib %>% dplyr::tally() > tib %>%
     dplyr::select(boundingBox.minX,
                   boundingBox.maxX,
                   boundingBox.minY,
                   boundingBox.maxY) %>% dplyr::distinct() %>% dplyr::tally() | view == T){
    bboxes <- list()
    for(i in 1:length(tib$downloadURL)){
      pts <- list(base::rbind(c(tib$boundingBox.minX[i], tib$boundingBox.minY[i]), c(tib$boundingBox.minX[i], tib$boundingBox.maxY[i]), c(tib$boundingBox.maxX[i], tib$boundingBox.maxY[i]), c(tib$boundingBox.maxX[i], tib$boundingBox.minY[i]), c(tib$boundingBox.minX[i], tib$boundingBox.minY[i])))
      bboxes[[i]] <- sf::st_polygon(x = pts) %>% sf::st_sfc(crs = 4269) %>% tibble::as_tibble() %>% dplyr::bind_cols(tib[i,]) %>% sf::st_as_sf()
    }

    combined_bboxes <- bboxes[[1]]
    if(length(tib$downloadURL) > 1){
      for(i in 2:length(tib$downloadURL)){
        combined_bboxes <- base::rbind(combined_bboxes, bboxes[[i]])
      }
    }

    m <- mapview::mapview(combined_bboxes, zcol = "ID", layer.name = "ID")+
      mapview::mapview(pipe_bbox, layer.name = "Pipe extent", color = "red", fill = F)

    print(tib %>% dplyr::select(ID, title, publicationDate, lastUpdated))
    print(m)
  }

  to_exclude <- base::readline("Exclude any tiles? (Y/N)")
  if(stringr::str_detect(base::tolower(to_exclude), pattern = "y")){
    excluded <- base::readline("What IDs to exclude? Separate with comma:")
  }

  if(stringr::str_detect(base::tolower(to_exclude), pattern = "n")){
    excluded <- NULL
  }

  if(!is.null(excluded)){
    tib <- tib %>%
      dplyr::filter(!ID %in% as.numeric(base::strsplit(excluded,split=",",fixed = F)[[1]]))
  }

  if(nrow(tib) == 0){
    stop("All tiles have been excluded. Exclude fewer tiles")
  }

  temp <- base::tempfile()
  tempd <- base::tempdir()
  keep_data <- list()

  for(i in 1:length(tib$downloadURL)){
    utils::download.file(tib$downloadURL[i],temp, mode = "wb")
    utils::unzip(temp, exdir=tempd)
    data <- raster::raster(base::file.path(tempd,base::paste0(tools::file_path_sans_ext(base::basename(tib$downloadURL[i])),".img")))
    base::unlink(temp)

    keep_data[[i]] <- data
  }

  if(length(keep_data) > 1){
    keep_data$fun <- max

    #run do call to implement mosaic over the list of raster objects.
    mos <- do.call(raster::mosaic, keep_data)
  }

  if(length(keep_data) == 1){
    mos <- keep_data[[1]]
  }

  raster_res <- raster::res(raster::projectExtent(mos, crs=raster::crs(x)))[1]

  #Create extents for clipping using generous 1000 ft butter
  ext <- sf::st_make_grid(x, n = 1) %>% sf::st_buffer(dist = 1000)
  final_extent <- raster::raster(ext %>% sf::as_Spatial(), res = raster_res)

  new_data <- raster::crop(raster::projectRaster(from = mos,
                                 to = final_extent),
                   final_extent)

  return(new_data)
}
