propagate_flood <-
  function(pipes = model_1D[[1]],
           nodes = model_1D[[2]],
           structures = model_1D[[3]],
           select_nodes = select_nodes,
           water_elevation) {

  water_elevation = units::set_units(water_elevation, units(nodes$inv_elev)$numerator)

  z = 0
  node_list <- select_nodes %>%
    dplyr::pull(nodeID)

  pipe_list <- NULL

  while(!is.na(z)) {
    z = z + 1

    new_pipes <-
      pipes %>%
      dplyr::filter(to %in% node_list | from %in% node_list)

    new_pipe_list <- setdiff(unique(c(new_pipes$edgeID)), pipe_list)

    if(is.null(nodes$alt_edgeID)){
      new_node_list <-
        setdiff(unique(c(new_pipes$to, new_pipes$from)), node_list)
    }

    if(!is.null(nodes$alt_edgeID)){
      new_node_list <-
        setdiff(unique(
          c(
            new_pipes$to,
            new_pipes$from,
            nodes %>% dplyr::group_by(nodeID) %>% dplyr::filter(sum(unlist(alt_edgeID) %in% new_pipe_list) > 0) %>% dplyr::pull(nodeID)
          )
        ), node_list)
    }

    new_nodes <- nodes %>%
      dplyr::filter(nodeID %in% new_node_list)

    filtered_nodes <- new_nodes %>%
      dplyr::filter(inv_elev <= water_elevation & (s_inv_elev <= water_elevation | is.na(s_inv_elev)))

    filtered_nodes <- obstruct_propagation(filtered_nodes, water_elevation = water_elevation)

    filtered_node_list <- unique(filtered_nodes$nodeID)

    filtered_pipes <- new_pipes %>%
      dplyr::filter(to %in% filtered_node_list | from %in% filtered_node_list | edgeID %in% c(filtered_nodes %>% dplyr::pull(alt_edgeID) %>% unlist() %>% unique()))

    filtered_pipe_list <- unique(filtered_pipes$edgeID)

    if(sum(filtered_node_list) == 0){
      z = NA
      break()
    }

    node_list <- c(node_list, filtered_node_list)
    pipe_list <- c(pipe_list, filtered_pipe_list)
  }

  final_nodes <- nodes %>%
    dplyr::filter(nodeID %in% node_list)

  return(final_nodes)
}
