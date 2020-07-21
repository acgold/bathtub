#' Create folders for bathtub model and results
#'
#' @param path Full path for location of bathtub output folder
#'
#' @return Workspace path for bathtub models
#' @examples
#'# workspace <- folder_setup("your_path_here")

folder_setup <- function(path){

  if(dir.exists(paste0(path,"/bathtub_output"))){
    cat("bathtub output folder already exists\n")

    return(paste0(path,"/bathtub_output"))
  }

  if(!dir.exists(paste0(path,"/bathtub_output"))){

  dir.create(paste0(path,"/bathtub_output"))
  dir.create(paste0(path,"/bathtub_output", "/DEMs"))
  dir.create(paste0(path,"/bathtub_output",  "/figures"))
  dir.create(paste0(path,"/bathtub_output",  "/input"))
  dir.create(paste0(path,"/bathtub_output",  "/model"))
  dir.create(paste0(path,"/bathtub_output",  "/results"))

  cat("bathtub output folders created!\n")

  return(paste0(path,"/bathtub_output"))
  }

}
