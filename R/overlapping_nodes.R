overlapping_nodes <- function(pipes, nodes){
  nodes <- tibble::as_tibble(nodes)
  pipes <- tibble::as_tibble(pipes)

  # convert nodes and pipes to DT objects
  new_nodes <- nodes
  new_nodes_DT <- data.table::setDT(data.table::copy(new_nodes))

  # # fixing issue with overlapping_nodes
  # new_nodes <- new_nodes %>%
  #   dplyr::group_by(nodeID) %>%
  #   dplyr::mutate(min_inv_elev = dplyr::na_if(min(inv_elev, na.rm=T), units::set_units(Inf, units(new_nodes$inv_elev)))) %>%
  #   dplyr::mutate(inv_elev =  dplyr::if_else(is.na(inv_elev) & !is.na(min_inv_elev), min_inv_elev, inv_elev))
  #
  # start_nodes <- new_nodes %>%
  #   dplyr::filter(start_end == "start") %>%
  #   dplyr::group_by(nodeID, edgeID) %>%
  #   dplyr::mutate(min_inv_elev = dplyr::na_if(min(inv_elev, na.rm=T), units::set_units(Inf, units(new_nodes$inv_elev)))) %>%
  #   dplyr::mutate(inv_elev =  dplyr::if_else(is.na(inv_elev) & !is.na(min_inv_elev), min_inv_elev, inv_elev))
  #
  # end_nodes <- new_nodes %>%
  #   dplyr::filter(start_end == "end") %>%
  #   dplyr::group_by(nodeID, edgeID) %>%
  #   dplyr::mutate(min_inv_elev = dplyr::na_if(min(inv_elev, na.rm=T), units::set_units(Inf, units(new_nodes$inv_elev)))) %>%
  #   dplyr::mutate(inv_elev =  dplyr::if_else(is.na(inv_elev) & !is.na(min_inv_elev), min_inv_elev, inv_elev))


  # find minimum elevation for each nodeID. Remove infinites
  new_nodes_DT[,`:=`(min_inv_elev = inv_elev)]
  new_nodes_DT[,`:=`(num_nodes = .N), by = nodeID]
  new_nodes_DT[,`:=`(num_NAs = sum(is.na(inv_elev))), by = nodeID]
  new_nodes_DT[,`:=`(calc_min = (num_nodes > num_NAs))]

  new_nodes_DT[(calc_min == T), min_inv_elev := min(inv_elev, na.rm=T), by = nodeID] #(ifelse(length(inv_elev) == sum(is.na(inv_elev)), inv_elev,
  # invisible(set(new_nodes_DT, which(is.infinite(new_nodes_DT$min_inv_elev)), j = ("min_inv_elev"),value =NA))
  new_nodes_DT[is.na(inv_elev) & !is.na(min_inv_elev),inv_elev := min_inv_elev]

  start_nodes <- new_nodes_DT[start_end == "start"]
  start_nodes[calc_min == T, min_inv_elev := min(inv_elev, na.rm=T), by = .(nodeID, edgeID)]
  start_nodes[,`:=`(num_nodes = NULL,num_NAs = NULL,calc_min = NULL)]

  # invisible(set(start_nodes, which(is.infinite(start_nodes$inv_elev)), j = ("inv_elev"),value =NA))
  start_nodes <- unique(start_nodes, by = c("edgeID","nodeID","inv_elev"))
  start_nodes <- tibble::as_tibble(start_nodes)

  end_nodes <- new_nodes_DT[start_end == "end"]
  end_nodes[calc_min == T, min_inv_elev := min(inv_elev, na.rm=T), by = .(nodeID, edgeID)]
  end_nodes[,`:=`(num_nodes = NULL,num_NAs = NULL,calc_min = NULL)]
  # invisible(set(end_nodes, which(is.infinite(end_nodes$inv_elev)), j = ("inv_elev"),value =NA))
  end_nodes <- unique(end_nodes, by = c("edgeID","nodeID","inv_elev"))
  end_nodes <- tibble::as_tibble(end_nodes)

  nodes <- start_nodes %>%
    rbind(end_nodes)

  pipes <- pipes %>%
    dplyr::left_join(start_nodes %>%
                       dplyr::select(edgeID, nodeID, inv_elev),
                     by=c("edgeID","from"="nodeID")) %>%
    dplyr::mutate(from_inv_elev = inv_elev) %>%
    dplyr::select(-inv_elev) %>%
    dplyr::left_join(end_nodes %>%
                       dplyr::select(edgeID, nodeID, inv_elev),
                     by=c("edgeID","to"="nodeID")) %>%
    dplyr::mutate(to_inv_elev = inv_elev) %>%
    dplyr::select(-inv_elev)

  return(list(pipes, nodes))
}
