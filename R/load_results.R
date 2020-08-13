#' Load existing bathtub model results from file
#'
#' @param workspace Full path for location of bathtub output folder
#'
#' @return bathtub model results
#' @examples
#'# bft_model_results <- load_results(workspace)

load_results <- function(workspace){
  results_exist <- length(list.files(paste0(workspace,"/results/")))


  if(results_exist > 0){

  model_results <- list(pipes = bathtub::load_w_units(paste0(workspace,"/results/imp_pipes.gpkg")),
                    nodes = bathtub::load_w_units(paste0(workspace,"/results/imp_nodes.gpkg")),
                    np_nodes = bathtub::load_w_units(paste0(workspace,"/results/np_nodes.gpkg")),
                    structures = bathtub::load_w_units(paste0(workspace,"/results/imp_struc.gpkg")),
                    np_structures = bathtub::load_w_units(paste0(workspace,"/results/np_struc.gpkg")),
                    flooding = bathtub::load_w_units(paste0(workspace,"/results/flooding_extent.gpkg")))

  info <- read.csv(paste0(workspace,"/results/results_info.csv"))
  colnames(info) <- c("parameters","values")
  print(info)

  if(file.exists(paste0(workspace,"/results/ponding_extent.gpkg"))){
    model_results$ponding <- bathtub::load_w_units(paste0(workspace,"/results/ponding_extent.gpkg"))
  }

  if(file.exists(paste0(workspace,"/results/overlay.gpkg"))){
    model_results$overlay <- bathtub::load_w_units(paste0(workspace,"/results/overlay.gpkg"))
  }
}
  if(results_exist == 0){
    stop("No model results to load. Follow workflow to run 'model_inundation'\n")
  }

  return(model_results)
}
