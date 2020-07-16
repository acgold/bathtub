propagate_flood_ts <-
  function(pipes = model_1D[[1]],
           nodes = model_1D[[2]],
           structures = model_1D[[3]],
           select_nodes = select_nodes,
           water_elevation,
           time_step) {

    water_elevation = units::set_units(water_elevation, units(nodes$inv_elev)$numerator)

    z = 0
    node_list <- select_nodes %>%
      dplyr::pull(nodeID)
    node_z <- rep(0,length(node_list))

    pipe_list <- NULL
    pipe_z <- NULL

    while(!is.na(z)) {
      z = z + 1

      new_pipes <-
        pipes %>%
        dplyr::filter(to %in% node_list | from %in% node_list) %>%
        mutate(z=1)

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
      filtered_node_z <- rep(z, length(filtered_node_list))

      filtered_pipes <- new_pipes %>%
        dplyr::filter(to %in% filtered_node_list | from %in% filtered_node_list | edgeID %in% c(filtered_nodes %>% dplyr::pull(alt_edgeID) %>% unlist() %>% unique()))

      filtered_pipe_list <- unique(filtered_pipes$edgeID)
      filtered_pipe_z <- rep(z, length(filtered_pipe_list))

      if(sum(filtered_node_list) == 0){
        z = NA
        break()
      }

      node_list <- c(node_list, filtered_node_list)
      node_z <- c(node_z, filtered_node_z)

      pipe_list <- c(pipe_list, filtered_pipe_list)
      pipe_z <- c(pipe_z, filtered_pipe_z)
    }

    final_nodes <- nodes %>%
      dplyr::filter(nodeID %in% node_list) %>%
      dplyr::left_join(tibble("nodeID" = node_list, "z" = node_z), by = "nodeID")

    return(final_nodes)
  }
