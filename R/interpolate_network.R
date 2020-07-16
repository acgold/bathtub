interpolate_network <- function(pipes,
                                nodes,
                                rounds = 5,
                                adjustment = 0.1,
                                backup_depth = 1.5,
                                adjustment_units = "ft",
                                up_condition = NULL,
                                dn_condition = NULL,
                                obstruction_keywords = NULL,
                                structure_condition = NULL,
                                obstruction_percent = F
){

  units::units_options(set_units_mode = "standard")

  adjustment <- units::set_units(adjustment, adjustment_units)
  backup_depth <- units::set_units(backup_depth, adjustment_units)

  pipes <- pipes %>%
    dplyr::mutate(interp_dn = "none",
           interp_up = "none")

  if(rounds == 0){
    return(list(pipes, nodes))
  }

  O <- floodr::overlapping_nodes(pipes = pipes, nodes = nodes)

  pb <- progress::progress_bar$new(format = " Interpolating missing elevations [:bar] :current/:total (:percent)", total = rounds)

  i = 1
  while(is.na(i) | i <= rounds){

    A <- floodr::adjust_pipes(pipes = O[[1]], nodes = O[[2]], adjustment = adjustment)

    if(sum(is.na(A[[1]]$from_inv_elev) | is.na(A[[1]]$to_inv_elev)) == 0){
      i = NA
      break()
    }

    O <- floodr::overlapping_nodes(pipes = A[[1]], nodes = A[[2]])
    # cat(paste0(i,"/",rounds))
    pb$tick(1)
    i = i+1
  }

  return(list(O[[1]], O[[2]]))
}
