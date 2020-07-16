obstruct_propagation <- function(nodes, water_elevation){
  # return unaltered nodes if no obstruction data found
  if(is.null(nodes$obstructed_struc) & is.null(nodes$obstructed_pipe)){
    return(nodes)
  }

  # return error if pipe and structure obstructions found
  if(!is.null(nodes$obstructed_struc) & !is.null(nodes$obstructed_pipe)){
    stop("floodR currently does not handle both pipe and structure obstructions. Please pick one or the other and re-run")
  }

  # Obstructed structures
  if(!is.null(nodes$obstructed_struc) & is.null(nodes$obstructed_pipe)){
    if(is.logical(nodes$obstructed_struc)){
      nodes <- nodes %>%
        dplyr::filter(obstructed_struc == F | is.na(obstructed_pipe))
      return(nodes)
    }
    if(units(nodes$obstructed_struc)$numerator == "%"){
      nodes <- nodes %>%
        dplyr::filter((s_inv_elev + (obstructed_struc * (elev - s_inv_elev))) <= water_elevation | is.na((s_inv_elev + (obstructed_struc * (elev - s_inv_elev)))))
      return(nodes)
    }
  }

  # Obstructed pipes
  if(is.null(nodes$obstructed_struc) & !is.null(nodes$obstructed_pipe)){
    if(is.logical(nodes$obstructed_pipe)){
      nodes <- nodes %>%
        dplyr::filter(obstructed_pipe == F | is.na(obstructed_pipe))
      return(nodes)
    }

    if(units(nodes$obstructed_pipe)$numerator == "%"){
      nodes <- nodes %>%
        dplyr::filter((inv_elev + (obstructed_struc * diam)) <= water_elevation | is.na((inv_elev + (obstructed_struc * diam))))
      return(nodes)
    }
  }
}
