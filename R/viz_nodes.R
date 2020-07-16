viz_nodes <- function(model_output,
                      model,
                      elev,
                      type = "plot",
                      hide_labels = F,
                      simplify_labels = T,
                      label_size = 2,
                      filename = NULL,
                      workspace,
                      ...){
  theme_params <- list(...)

  total_impacted_pipes <- model_output[[1]]
  total_impacted_nodes <- model_output[[2]]
  total_np_nodes <- model_output[[3]]
  total_impacted_structures <- model_output[[4]]
  total_np_structures <- model_output[[5]]
  total_flooding <- model_output[[6]]
  overlay <- NULL


  if(length(model_output) == 7){
    overlay <- model_output[[7]]
  }

  n_nodes = nrow(model[[2]] %>% dplyr::filter(nodeID %in% unique(model[[2]] %>% dplyr::pull(nodeID))))

  if(type == "plot" & is.null(overlay)){
    plot_data <- total_impacted_nodes %>%
      dplyr::mutate(binned_perc = forcats::fct_explicit_na(cut(node_perc_fill , breaks = c(0,20,40,60,80,99,100), include.lowest = T, right = T))) %>%
      dplyr::group_by(water_elevation, binned_perc) %>%
      dplyr::summarise(n_perc = n()/n_nodes) %>%
      dplyr::filter(!is.na(water_elevation)) %>%
      dplyr::ungroup()

    agg_data <- total_impacted_nodes %>%
      dplyr::mutate(binned_perc = forcats::fct_explicit_na(cut(node_perc_fill , breaks = c(0,20,40,60,80,99,100), include.lowest = T, right = T))) %>%
      dplyr::group_by(water_elevation) %>%
      dplyr::summarise(n_perc = n()/n_nodes) %>%
      dplyr::filter(!is.na(water_elevation)) %>%
      dplyr::ungroup()

    cols <- c("[0,20]" = "#440154FF",
              "(20,40]" = "#414487FF",
              "(40,60]" = "#2A788EFF",
              "(60,80]" = "#22A884FF",
              "(80,99]" = "#7AD151FF",
              "(99,100]" = "#FDE725FF",
              "(Missing)" = "dark grey")

    label_data <- total_impacted_nodes %>%
      dplyr::group_by(water_elevation) %>%
      dplyr::summarise(n_perc = n()/n_nodes) %>%
      dplyr::filter(!is.na(water_elevation)) %>%
      dplyr::group_by(water_elevation) %>%
      dplyr:: mutate(label = ifelse(sum(hide_labels) == 0, round((n_perc * 100), digits = 1), "")) %>%
      dplyr::group_by(label) %>%
      dplyr::mutate(min_of_label = (water_elevation == min(water_elevation))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(label = ifelse(min_of_label==F & simplify_labels == T, "", label))

    impacted_node_total <- ggplot2::ggplot(data = plot_data)+
      geom_col(aes(x=water_elevation, y = n_perc*100, fill = binned_perc), position = "stack")+
      ggrepel::geom_text_repel(data = label_data,
                      aes(x = water_elevation, y = n_perc * 100, label = label),
                      vjust = -.5, size = label_size, color = "grey20", direction = "y", box.padding = 0)+
      geom_line(data = total_np_nodes %>%
                  dplyr::group_by(water_elevation) %>%
                  dplyr::summarise(n_perc = n()/n_nodes) %>%
                  dplyr::filter(!is.na(water_elevation)), aes(x=water_elevation, y = n_perc*100,color = "No Pipes"))+
      geom_point(data = total_np_nodes %>%
                   dplyr::group_by(water_elevation) %>%
                   dplyr::summarise(n_perc = n()/n_nodes) %>%
                   dplyr::filter(!is.na(water_elevation)), aes(x=water_elevation, y = n_perc*100,color = "No Pipes"))+
      scale_fill_manual(values = cols)+
      scale_x_continuous(breaks= scales::pretty_breaks())+
      scale_y_continuous(breaks= scales::pretty_breaks(), limits = c(NA, max(agg_data$n_perc)*103))+
      scale_color_manual(values = c("black"), name = "",labels = "No Pipes")+
      ylab("Impacted inlets (% of total)")+
      xlab(paste0("MHHW (",units(total_impacted_nodes$s_inv_elev)$numerator,")"))+
      theme_bw()+
      theme(legend.background = element_blank(),
            legend.key = element_blank())+
      guides(fill = F)+
      theme(legend.position = c(.3,.9))+
      theme(...)


    impacted_node_ratio <- total_impacted_nodes %>%
      dplyr::mutate(binned_perc = forcats::fct_explicit_na(cut(node_perc_fill , breaks = c(0,20,40,60,80,99,100), include.lowest = T, right = T))) %>%
      dplyr::group_by(water_elevation, binned_perc) %>%
      dplyr::summarise(n_perc = n()/n_nodes) %>%
      dplyr::filter(!is.na(water_elevation)) %>%
      dplyr::ungroup() %>%
      ggplot2::ggplot()+
      geom_col(aes(x=water_elevation, y = n_perc*100, fill = binned_perc), position = "fill")+
      coord_cartesian(ylim=c(0,1))+
      scale_fill_manual(values = cols,name = "Node \nvolume \nfilled (%)", limits = names(cols), labels = c("0 - 20", "20 - 40","40 - 60","60 - 80","80 - 99","100", "Unknown"))+
      scale_x_continuous(breaks= scales::pretty_breaks())+
      scale_y_continuous(breaks= scales::pretty_breaks(), labels = scales::label_percent())+
      ylab("Relative amount")+
      xlab(paste0("MHHW (",units(total_impacted_nodes$s_inv_elev)$numerator,")"))+
      theme_bw()+
      theme(...)

    impact_plot <- cowplot::plot_grid(impacted_node_total, impacted_node_ratio, nrow =1)

    if(!is.null(filename)){
      ggplot::ggsave(filename = filename, plot = impact_plot, path = paste0(workspace,"/figures/"),
             width = 180, height = 70, units = "mm")
    }

    return(impact_plot)
  }

  if(type == "plot" & !is.null(overlay)){
    plot_data <- total_impacted_nodes %>%
      dplyr::mutate(binned_perc = forcats::fct_explicit_na(cut(nodes_perc_fill , breaks = c(0,20,40,60,80,99,100), include.lowest = T, right = T))) %>%
      dplyr::group_by(water_elevation, binned_perc) %>%
      dplyr::summarise(n_perc = n()/n_nodes) %>%
      dplyr::filter(!is.na(water_elevation)) %>%
      dplyr::ungroup()

    agg_data <- total_impacted_nodes %>%
      dplyr::mutate(binned_perc = forcats::fct_explicit_na(cut(nodes_perc_fill , breaks = c(0,20,40,60,80,99,100), include.lowest = T, right = T))) %>%
      dplyr::group_by(water_elevation) %>%
      dplyr::summarise(n_perc = n()/n_nodes) %>%
      dplyr::filter(!is.na(water_elevation)) %>%
      dplyr::ungroup()

    cols <- c("[0,20]" = "#440154FF",
              "(20,40]" = "#414487FF",
              "(40,60]" = "#2A788EFF",
              "(60,80]" = "#22A884FF",
              "(80,99]" = "#7AD151FF",
              "(99,100]" = "#FDE725FF",
              "(Missing)" = "dark grey")

    label_data <- total_impacted_nodes %>%
      dplyr::group_by(water_elevation) %>%
      dplyr::summarise(n_perc = n()/n_nodes) %>%
      dplyr::filter(!is.na(water_elevation)) %>%
      dplyr::group_by(water_elevation) %>%
      dplyr::mutate(label = ifelse(sum(hide_labels) == 0, round((n_perc * 100), digits = 1), "")) %>%
      dplyr::group_by(label) %>%
      dplyr::mutate(min_of_label = (water_elevation == min(water_elevation))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(label = ifelse(min_of_label==F & simplify_labels == T, "", label))

    impacted_node_total <- ggplot2::ggplot(data = plot_data)+
      geom_col(aes(x=factor(1), y = n_perc*100, fill = binned_perc), position = "stack", width = 0.4)+
      ggrepel::geom_text_repel(data = label_data,
                      aes(x = factor(1), y = n_perc * 100, label = label),
                      vjust = -.5, size = label_size, color = "grey20", direction = "y", box.padding = 0)+

      scale_fill_manual(values = cols)+
      scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(NA, 100))+
      scale_color_manual(values = c("black"), name = "",labels = "No Pipes")+
      ylab("Impacted nodes (% of total)")+
      xlab(names(overlay))+
      theme_bw()+
      theme(legend.background = element_blank(),
            legend.key = element_blank())+
      guides(fill = F)+
      theme(legend.position = c(.3,.9),
            axis.title.x = element_text(size = 12),
            axis.text.x = element_blank(),
            panel.grid.major.x = element_blank())

    if(nrow(total_np_nodes)>0){
      impacted_node_total <- impacted_node_total +
        geom_line(data = total_np_nodes %>%
                    dplyr::mutate(binned_perc =  forcats::fct_explicit_na(cut(node_perc_fill , breaks = c(0,20,40,60,80,99,100), include.lowest = T, right = T))) %>%
                    dplyr::group_by(water_elevation) %>%
                    dplyr::summarise(n_perc = n()/n_node) %>%
                    dplyr::filter(!is.na(water_elevation)),
                  aes(x=factor(1), y = n_perc*100, color = "No Pipes"))+
        geom_point(data = total_np_nodes %>%
                     dplyr::mutate(binned_perc =  forcats::fct_explicit_na(cut(node_perc_fill , breaks = c(0,20,40,60,80,99,100), include.lowest = T, right = T))) %>%
                     dplyr::group_by(water_elevation) %>%
                     dplyr::summarise(n_perc = n()/n_node) %>%
                     dplyr::filter(!is.na(water_elevation)),
                   aes(x=factor(1), y = n_perc*100 ,color = "No Pipes"))
    }

    impacted_node_ratio <- total_impacted_nodes %>%
      dplyr::mutate(binned_perc = forcats::fct_explicit_na(cut(node_perc_fill , breaks = c(0,20,40,60,80,99,100), include.lowest = T, right = T))) %>%
      dplyr::group_by(water_elevation, binned_perc) %>%
      dplyr::summarise(n_perc = n()/n_node) %>%
      dplyr::filter(!is.na(water_elevation)) %>%
      dplyr::ungroup() %>%
      ggplot2::ggplot()+
      geom_col(aes(x=factor(1), y = n_perc*100, fill = binned_perc), position = "fill", width = 0.4)+
      coord_cartesian(ylim=c(0,1))+
      scale_fill_manual(values = cols,name = "Node \nvolume \nfilled (%)", limits = names(cols), labels = c("0 - 20", "20 - 40","40 - 60","60 - 80","80 - 99","100", "Unknown"))+
      # scale_x_continuous(breaks= pretty_breaks())+
      scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::label_percent())+
      ylab("Relative amount")+
      xlab(names(overlay))+
      theme_bw()+
      theme(axis.title.x = element_text(size = 12),
            axis.text.x = element_blank(),
            panel.grid.major.x = element_blank())

    if(class(overlay)[1] == "RasterLayer"){
      static_map <- ggplot2::ggplot()+
        theme_minimal()+
        geom_sf(data = raster::rasterToContour(elev, nlevels = 5) %>% sf::st_as_sf() %>% dplyr::mutate(level = as.numeric(as.character(level))),aes(color = level))+
        geom_sf(data = model[[1]], size = 1)+
        geom_sf(data = overlay %>%
                  stars::st_as_stars() %>%
                  sf::st_as_sf(as_points = FALSE,
                           merge = TRUE,
                           connect8 = T), fill = "royal blue", color = NA, alpha = 0.8)+
        geom_sf(data = total_impacted_pipes, color = "red", size = 1)+
        geom_sf(data = total_impacted_nodes, color = "red", size = 2)+
        scale_color_distiller(palette = "Greys", name = "Elevation (ft, MHHW)")
    }


    impact_plot <- cowplot::plot_grid(impacted_node_total, impacted_node_ratio, static_map, rel_widths = c(1,1,3), nrow=1)

    if(!is.null(filename)){
      ggplot2::ggsave(filename = filename, plot = impact_plot, path = paste0(workspace,"/figures/"),
             width = 180, height = 70, units = "mm")
    }

    return(impact_plot)
  }

  if(type == "interactive_map"){
    min_wl <- min(total_impacted_nodes$water_elevation)
    max_wl <- max(total_impacted_nodes$water_elevation)
    node_list <- sort(unique(total_impacted_nodes$nodeID))
    wl_seq <- sort(unique(total_impacted_nodes$water_elevation))

    p <- NULL
    p <- foreach::foreach(i = node_list) %do% {

      d <- total_impacted_nodes %>%
        dplyr::filter(nodeID == i) %>%
        tibble::as_tibble() %>%
        units::drop_units()

      if(length(setdiff(wl_seq,d$water_elevation)) > 0){
        missing <- setdiff(wl_seq,d$water_elevation)

        added_rows <- d %>%
          dplyr::slice(rep(1, each = length(missing))) %>%
          dplyr::mutate(node_fill_height = 0,
                 node_perc_fill = 0,
                 water_elevation = missing)

        d <- added_rows %>%
          rbind(d)
      }

      disclaimer <- dplyr::if_else(is.na(sum(d$inv_elev)) | sum(d$inv_elev == d$elev) > 0 , "Warning", "")
      disclaimer_sub <- dplyr::if_else(is.na(sum(d$inv_elev))| sum(d$inv_elev == d$elev) > 0, "Flooding calculated using surface elevation \nbecause invert elevation is missing \nor same as surface elevation", "")

      ggplot2::ggplot(data = d)+
        geom_line(aes(x = water_elevation, y = node_perc_fill/100))+
        geom_point(aes(x = water_elevation, y = node_perc_fill/100))+
        ggtitle(disclaimer, subtitle = disclaimer_sub)+
        theme_light()+
        xlab(paste0("Water elevation (",units(total_impacted_nodes$inv_elev)$numerator,")"))+
        ylab("Node fill percent")+
        scale_x_continuous(breaks = scales::pretty_breaks(),limits = c(min_wl, max_wl))+
        scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0,1), labels = scales::label_percent())
    }


    int_map <- mapview::mapview(x=model[[1]],color="black", layer.name = "Pipes")+
      mapview::mapview(model[[2]] %>%
                units::drop_units() %>%
                  dplyr::mutate(issue_spots = (is.na(inv_elev) | inv_elev == elev)) %>%
                  dplyr::filter(issue_spots == T),
              color = "red",
              layer.name = "Warnings",
              fill = F)+
      mapview::mapview(total_impacted_nodes %>%
                         dplyr::arrange(nodeID, water_elevation) %>%
                         dplyr::group_by(nodeID) %>%
                         dplyr::slice(1) %>%
                         dplyr::arrange(nodeID) %>%
                         dplyr::ungroup() %>%
                         units::drop_units(),
              zcol = "water_elevation",
              layer.name = "Water level of first impact - Nodes",
              popup = leafpop::popupGraph(p, type = "svg"),
              cex = 5)

    return(int_map)
  }
}
