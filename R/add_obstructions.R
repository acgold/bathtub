add_obstructions <- function(pipes,
                             nodes,
                             structures,
                             up_condition = NULL,
                             dn_condition = NULL,
                             obstruction_keywords = NULL,
                             structure_condition = NULL,
                             obstruction_percent = F){

  if((!is.null(up_condition) | !is.null(dn_condition)) & obstruction_percent == F){
    pipes <- pipes %>%
      dplyr::mutate(blocked_dn = stringr::str_detect(pipes[[dn_condition]], pattern = paste(obstruction_keywords,collapse = '|')),
             blocked_up = stringr::str_detect(pipes[[up_condition]], pattern = paste(obstruction_keywords,collapse = '|')))

    nodes <- nodes %>%
      tibble::as_tibble() %>%
      dplyr::left_join(pipes %>%  tibble::as_tibble() %>% dplyr::ungroup() %>% dplyr::select(from,blocked_up), by = c("nodeID" = "from")) %>%
      dplyr::left_join(pipes %>%  tibble::as_tibble() %>% dplyr::ungroup() %>% dplyr::select(to,blocked_dn), by = c("nodeID" = "to")) %>%
      dplyr::mutate(blocked_up = tidyr::replace_na(blocked_up, F),
             blocked_dn = tidyr::replace_na(blocked_dn, F),
             obstructed = (blocked_dn + blocked_up) > 0) %>%
      dplyr::select(-c(blocked_up, blocked_dn)) %>%
      sf::st_as_sf()
  }

  if((!is.null(up_condition) | !is.null(dn_condition)) & obstruction_percent == T){
    pipes <- pipes %>%
      dplyr::mutate(blocked_dn = as.numeric(stringr::str_extract(pipes[[dn_condition]], pattern = "(\\d)+")),
             blocked_up = as.numeric(stringr::str_extract(pipes[[up_condition]], pattern = "(\\d)+")))

    nodes <- nodes %>%
      tibble::as_tibble() %>%
      dplyr::left_join(pipes %>%  tibble::as_tibble() %>% dplyr::ungroup() %>% dplyr::select(from,blocked_up,diam_up), by = c("nodeID" = "from")) %>%
      dplyr::left_join(pipes %>%  tibble::as_tibble() %>% dplyr::ungroup() %>% dplyr::select(to,blocked_dn,diam_dn), by = c("nodeID" = "to")) %>%
      dplyr::group_by(nodeID) %>%
      dplyr::mutate(diam = max(diam_up, diam_dn, na.rm=T),
             blocked_up = tidyr::replace_na(blocked_up, 0),
             blocked_dn = tidyr::replace_na(blocked_dn, 0),
             obstructed_pipe = max(blocked_dn,blocked_up, na.rm=T)) %>%
      dplyr::select(-c(blocked_up, blocked_dn, diam_up, diam_dn)) %>%
      sf::st_as_sf()

    nodes$obstructed_pipe <- units::set_units(nodes$obstructed_pipe,"%")
  }

  if(!is.null(structure_condition) & obstruction_percent == F){
    structures <- structures %>%
      dplyr::mutate(obstructed_struc = stringr::str_detect(structures[[structure_condition]], pattern = paste(obstruction_keywords,collapse = '|')))

    nodes <- nodes %>%
      tibble::as_tibble() %>%
      dplyr::left_join(structures %>% tibble::as_tibble() %>% dplyr::ungroup() %>% dplyr::select(structureID, obstructed_struc), by = c("structureID")) %>%
      dplyr::mutate(obstructed_struc = tidyr::replace_na(obstructed_struc, F)) %>%
      sf::st_as_sf()
  }

  if(!is.null(structure_condition) & obstruction_percent == T){
    structures <- structures %>%
      dplyr::mutate(obstructed_struc = as.numeric(stringr::str_extract(structures[[structure_condition]], pattern = "(\\d)+")))

    nodes <- nodes %>%
      tibble::as_tibble() %>%
      dplyr::left_join(structures %>% tibble::as_tibble() %>% dplyr::ungroup() %>% dplyr::select(structureID, obstructed_struc), by = c("structureID")) %>%
      dplyr::mutate(obstructed_struc = tidyr::replace_na(obstructed_struc, 0)) %>%
      sf::st_as_sf()

    nodes$obstructed_struc <- units::set_units(nodes$obstructed_struc,"%")
  }

  return(list(pipes, nodes, structures))
}
