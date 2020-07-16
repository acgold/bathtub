extract_ts <- function(model,
                       model_output,
                       ts,
                       ts_units = "m",
                       ts_datum = "MHHW",
                       type = "node",
                       id,
                       compensate = F){
  units::units_options(set_units_mode = "standard")

  pipes = model_output[[1]]
  nodes = model_output[[2]]
  structures = model_output[[3]]

  wl_conv_fac <- 1 / units::drop_units(units::set_units(units::set_units(1, ts_units), units(pipes$from_inv_elev)$numerator))

  ts <- tibble::as_tibble(ts)
  ts[,2] <- ts[,2] / wl_conv_fac
  colnames(ts)[2] <- "water_elevation"

  if(compensate == F){
    if(type == "node"){
      selected <- nodes %>%
        filter(nodeID %in% id)

      out_ts <- foreach(i = 1:length(id), .combine = "bind_rows") %do% {
        fit <- loess(node_fill_height~water_elevation, data = selected)
        predicted_vals <- predict(fit, newdata = ts)
        new_ts <- cbind(ts[,1:2],"predicted_fill_height" = predicted_vals) %>%
          tibble::as_tibble() %>%
          mutate(predicted_fill_height = replace_na(predicted_fill_height, 0)) %>%
          mutate(predicted_fill_height = ifelse(predicted_fill_height < 0, 0, predicted_fill_height))
        new_ts
      }

      return(out_ts)
    }

    if(type == "structure"){
      selected <- nodes %>%
        filter(structureID %in% id) %>%
        arrange(water_elevation) %>%
        units::drop_units() %>%
        mutate(structure_fill_height = ifelse(s_inv_elev > water_elevation, 0, water_elevation - s_inv_elev))

      out_ts <- foreach(i = 1:length(id), .combine = "bind_rows") %do% {
        fit <- loess(structure_fill_height~water_elevation, data = selected %>% filter(structureID == id[i]))
        predicted_vals <- predict(fit, newdata = ts)
        new_ts <- cbind(ts[,1:2],"predicted_fill_height" = predicted_vals) %>%
          tibble::as_tibble() %>%
          mutate(diff_water_elev = water_elevation - lag(water_elevation)) %>%
          mutate(predicted_fill_height = replace_na(predicted_fill_height, 0)) %>%
          # mutate(predicted_fill_height = ifelse(diff_water_elev > 0, predicted_fill_height - ((selected %>% filter(structureID == id[i]) %>% pull(z) %>% max())*0.15), predicted_fill_height)) %>%
          # mutate(predicted_fill_height = ifelse(diff_water_elev < 0, predicted_fill_height - ((selected %>% filter(structureID == id[i]) %>% pull(z) %>% max())*0.05), predicted_fill_height)) %>%
          mutate(predicted_fill_height = ifelse(predicted_fill_height < 0, 0, predicted_fill_height))
        new_ts
      }
      return(out_ts)
    }
    }

  if(compensate == T){
    if(type == "node"){
      selected <- nodes %>%
        filter(nodeID %in% id)

      out_ts <- foreach(i = 1:length(id), .combine = "bind_rows") %do% {
        fit <- loess(node_fill_height~water_elevation, data = selected)
        predicted_vals <- predict(fit, newdata = ts)
        new_ts <- cbind(ts[,1:2],"predicted_fill_height" = predicted_vals) %>%
          tibble::as_tibble() %>%
          mutate(predicted_fill_height = replace_na(predicted_fill_height, 0)) %>%
          mutate(predicted_fill_height = predicted_fill_height - (selected %>% filter(nodeID == id[i]) %>% pull(z) %>% max())*0.10) %>%
          # mutate(predicted_fill_height = ifelse(diff_water_elev < 0, predicted_fill_height - ((selected %>% filter(nodeID == id[i]) %>% pull(z) %>% max())*0.05), predicted_fill_height)) %>%
          mutate(predicted_fill_height = ifelse(predicted_fill_height < 0, 0, predicted_fill_height))
        new_ts
      }

      return(out_ts)
    }

    if(type == "structure"){
      selected <- nodes %>%
        filter(structureID %in% id) %>%
        arrange(water_elevation) %>%
        units::drop_units() %>%
        mutate(structure_fill_height = ifelse(s_inv_elev > water_elevation, 0, water_elevation - s_inv_elev))

      out_ts <- foreach(i = 1:length(id), .combine = "bind_rows") %do% {
        fit <- loess(structure_fill_height~water_elevation, data = selected %>% filter(structureID == id[i]))
        predicted_vals <- predict(fit, newdata = ts)
        new_ts <- cbind(ts[,1:2],"predicted_fill_height" = predicted_vals) %>%
          tibble::as_tibble() %>%
          mutate(diff_water_elev = water_elevation - lag(water_elevation)) %>%
          mutate(predicted_fill_height = replace_na(predicted_fill_height, 0)) %>%
          mutate(predicted_fill_height = predicted_fill_height - ((selected %>% filter(structureID == id[i]) %>% dplyr::pull(z) %>% max())*0.10)) %>%
          # mutate(predicted_fill_height = ifelse(diff_water_elev < 0, predicted_fill_height - ((selected %>% filter(structureID == id[i]) %>% pull(z) %>% max())*0.05), predicted_fill_height)) %>%
          mutate(predicted_fill_height = ifelse(predicted_fill_height < 0, 0, predicted_fill_height))
        new_ts
      }
      return(out_ts)
    }
  }
}
