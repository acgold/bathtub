adjust_pipes <- function(pipes, nodes, adjustment = units::set_units(0.1,"ft")){

  pipes <- pipes %>%
    dplyr::mutate(interp_up = dplyr::if_else(is.na(from_inv_elev) & !is.na(to_inv_elev), "adjust", interp_up)) %>%
    dplyr::mutate(interp_dn = dplyr::if_else(is.na(to_inv_elev) & !is.na(from_inv_elev), "adjust", interp_dn)) %>%
    dplyr::mutate(from_inv_elev = dplyr::if_else(is.na(from_inv_elev) & !is.na(to_inv_elev), to_inv_elev + adjustment, from_inv_elev)) %>%
    dplyr::mutate(to_inv_elev = dplyr::if_else(is.na(to_inv_elev) & !is.na(from_inv_elev), from_inv_elev - adjustment, to_inv_elev))

  start_nodes <- nodes %>%
    dplyr::filter(start_end == "start") %>%
    dplyr::left_join(pipes %>% dplyr::select(edgeID, from, from_inv_elev, interp_up), by = c("edgeID","nodeID" = "from")) %>%
    dplyr::mutate(interp = interp_up) %>%
    dplyr::select(-interp_up)

  # start_nodes <- start_nodes %>%
  #   dplyr::mutate(inv_elev = dplyr::if_else(is.na(inv_elev),from_inv_elev, inv_elev)) %>%
  #   dplyr::select(-from_inv_elev) %>%
  #   dplyr::group_by(nodeID, edgeID) %>%
  #   dplyr::mutate(inv_elev = dplyr::na_if(min(inv_elev, na.rm=T), units::set_units(Inf, units(start_nodes$inv_elev))))

  start_nodes <- data.table::as.data.table(start_nodes)
  start_nodes[is.na(inv_elev), inv_elev := from_inv_elev]
  start_nodes <- start_nodes[,`:=`(from_inv_elev = NULL)]
  # start_nodes[,inv_elev := min(inv_elev, na.rm=T), by = c("edgeID","nodeID")]
  # invisible(set(start_nodes, which(is.infinite(start_nodes$inv_elev)), j = ("inv_elev"),value =NA))
  start_nodes <- tibble::as_tibble(start_nodes)

  end_nodes <- nodes %>%
    dplyr::filter(start_end == "end") %>%
    dplyr::left_join(pipes %>% dplyr::select(edgeID, to, to_inv_elev, interp_dn), by = c("edgeID","nodeID" = "to")) %>%
    dplyr::mutate(interp = interp_dn) %>%
    dplyr::select(-interp_dn)


  # end_nodes <- end_nodes %>%
  #   dplyr::mutate(inv_elev = dplyr::if_else(is.na(inv_elev),to_inv_elev, inv_elev)) %>%
  #   dplyr::select(-to_inv_elev) %>%
  #   dplyr::group_by(nodeID, edgeID) %>%
  #   dplyr::mutate(inv_elev = dplyr::na_if(min(inv_elev, na.rm=T), units::set_units(Inf, units(end_nodes$inv_elev))))

  end_nodes <- data.table::as.data.table(end_nodes)
  end_nodes[is.na(inv_elev), inv_elev := to_inv_elev]
  end_nodes <- end_nodes[,`:=`(to_inv_elev = NULL)]
  # end_nodes[,inv_elev := min(inv_elev, na.rm=T), by = c("edgeID","nodeID")]
  # invisible(set(end_nodes, which(is.infinite(end_nodes$inv_elev)), j = ("inv_elev"),value =NA))
  end_nodes <- tibble::as_tibble(end_nodes)

  nodes <- start_nodes %>%
    rbind(end_nodes)

  return(list(pipes, nodes))
}
