#' Load existing bathtub model from file
#'
#' @param workspace Full path for location of bathtub output folder
#'
#' @return bathtub model
#' @examples
#'# bft_model <- load_model(workspace)

load_model <- function(workspace){
  pipes_exist <- file.exists(paste0(workspace,"/model/pipes.gpkg"))
  nodes_exist <- file.exists(paste0(workspace,"/model/nodes.gpkg"))
  structures_exist <- file.exists(paste0(workspace,"/model/structures.gpkg"))

  if(pipes_exist == T & nodes_exist == T & structures_exist == T){

    tub_model <- list(pipes = bathtub::load_w_units(paste0(workspace,"/model/pipes.gpkg")),
                      nodes = bathtub::load_w_units(paste0(workspace,"/model/nodes.gpkg")) %>%
                        dplyr::group_by(edgeID, start_end, nodeID, elev, inv_elev, structureID, s_inv_elev, min_inv_elev, interp) %>%
                        dplyr::mutate(alt_edgeID = type.convert(strsplit(alt_edgeID, ","))) %>%
                        dplyr::ungroup(),
                      structures = bathtub::load_w_units(paste0(workspace,"/model/structures.gpkg")))

  }

  if(pipes_exist == F | nodes_exist == F | structures_exist == F){
    missing_files <- tibble("filename" = c("Pipes","Nodes","Structures"),
                            "exists" = c(pipes_exist, nodes_exist, structures_exist))

    print(missing_files %>% filter(exists == F))
    stop("Missing model files. See table above for missing files. Follow workflow to assemble model\n")
  }

  return(tub_model)
}
