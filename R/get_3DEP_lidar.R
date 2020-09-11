get_3DEP_lidar <- function(x, res = 1, res_units = "m", lidar_proj4 = NULL, view = T, workspace){
  units::units_options(set_units_mode = "standard")

  crs_info <- sf::st_crs(x, parameters = T)

  if(is.null(lidar_proj4)){
    lidar_proj4 <- crs_info$proj4string
  }

  res_units_conv <- round(units::drop_units((units::set_units(1, res_units))/(units::set_units(1, crs_info$ud_unit))), digits = 5)

  res_conv = res * res_units_conv

  pipe_bbox <- x %>%
    sf::st_transform(crs = 4269) %>%
    sf::st_bbox()

  cent_new <- c(mean(pipe_bbox[c(1,3)]), mean(pipe_bbox[c(2,4)]))

  api_url <- "https://viewer.nationalmap.gov/tnmaccess/api/products?"
  request <- httr::GET(url = api_url,
                 query = list(
                   datasets="Lidar+Point+Cloud+%28LPC%29",
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

    m <- mapview::mapview(combined_bboxes, zcol = "ID", layer.name = "ID")+ # burst = "ID" : incorporate after Mapview updated from 2.7.8
      mapview::mapview(pipe_bbox, layer.name = "Pipe extent", color = "red", fill = F)

    print(tib %>% dplyr::select(ID, title, publicationDate, prettyFileSize, lastUpdated))
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
    tib <- tib %>% dplyr::filter(!ID %in% as.numeric(base::strsplit(excluded,split=",",fixed = F)[[1]]))
  }

  if(nrow(tib) == 0){
    stop("All tiles have been excluded. Exclude fewer tiles")
  }

  # tempd <- base::tempdir()
  # keep_data <- list()
  test_dir <- paste0(workspace,"/lidar")
  for(i in 1:length(tib$downloadURL)){
    temp <- base::tempfile()

    utils::download.file(tib$downloadURL[i], temp, mode = "wb")
    # utils::unzip(temp, exdir=tempd)
    cat("Unzipping .las file to:",test_dir)
    utils::unzip(temp, exdir=test_dir, overwrite = TRUE)

    # las_file <- lidR::readLAS(file.path(test_dir,base::paste0(tools::file_path_sans_ext(base::basename(tib$downloadURL[i])),".las")))
    # lidR::epsg(las_file) <- lidar_EPSG

  }

  cat("Processing .las files...\n")
  data <- lidR::readLAScatalog(folder = test_dir)
  lidR::projection(data) <- lidar_proj4

  pipe_extent <- x %>%
    sf::st_make_grid(n=1) %>%
    sf::st_buffer(dist = 500) %>%
    sf::as_Spatial()

  cat("Clipping .las files to area of interest...\n")
  lidar_extract <- lidR::lasclip(data,pipe_extent)

  cat("Creating DEM using nearest neighbor interpolation...\n")
  lidar_DEM <- lidR::grid_terrain(lidar_extract, res = res_conv, algorithm = lidR::knnidw(), keep_lowest = T)
  raster::crs(lidar_DEM) <- lidar_proj4

  cat("Writing DEM to:\n", paste0(test_dir,"/","lidar_DEM.tif"))
  raster::writeRaster(lidar_DEM, filename = paste0(test_dir,"/","lidar_DEM.tif"), overwrite = T)
  rm(lidar_DEM)
  lidar_DEM <- terra::rast(paste0(test_dir,"/","lidar_DEM.tif"))
  return(lidar_DEM)
}
