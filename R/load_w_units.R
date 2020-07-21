#' Load sf files with units
#'
#' @param x Full path for location of \code{sf} file saved with \code{save_w_units}
#'
#' @return A \code{sf} object with units from associated file. See \code{save_w_units} for more info
#' @examples
#'# x_units <- load_w_units("your_path_here")

load_w_units <- function(x){

y <- sf::st_read(x, quiet = T) %>%
  tibble::as_tibble()
y_units <- readr::read_csv(paste0(tools::file_path_sans_ext(x),"_units.csv"),
                           col_types = cols(
                             name = col_character(),
                             type = col_character()))
y_units <- y_units %>%
  dplyr::mutate(type = ifelse(substr(type, start = 1, stop = 1) == "[",gsub("\\[|\\]", "", type), NA))


for(i in 1:ncol(y)){
  if(!is.na(y_units$type[i])){
    y[[y_units$name[i]]] <- units::set_units(y %>% dplyr::pull(y_units$name[i]),y_units$type[i], mode="standard")
  }
}

y_sf <- sf::st_as_sf(y)

return(y_sf)
}
