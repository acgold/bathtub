#' Write sf files with units
#'
#' @param x \code{sf} object with units.
#' @param full_path Full path (including filename and extension) to write \code{sf} object.
#' CSV file with column names and units will be saved with the same path but with
#' "_units.csv" at the end.
#' @param overwrite Overwrite existing file? Default is T
#'
#' @return N/A
#' @examples
#'# save_w_units(
#'# x = pipes,
#'# full_path = "your_path_here/pipes.gpkg",
#'# overwrite = T)

save_w_units <- function(x,
                         full_path,
                         overwrite = T){

  sf::st_write(obj = x, dsn = full_path, quiet= T, delete_layer = T, delete_dsn = overwrite)

  col_types <- tibble::tibble("name" = colnames(x),"type" = x %>% head %>% collect %>% lapply(dplyr::type_sum) %>% unlist)
  col_type_path <- paste0(tools::file_path_sans_ext(full_path),"_units.csv")

  readr::write_csv(col_types, col_type_path)
}
