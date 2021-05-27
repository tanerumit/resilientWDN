

#' Visualize Water Distribution Network Simulation Results
#'
#' @param nodes.data 
#' @param pipes.data 
#' @param node.fill.var 
#' @param node.size.var 
#' @param edge.color.var 
#' @param edge.color.threshold 
#' @param edge.size.var 
#' @param background.map 
#' @param show.legend 
#' @param plot.title 
#'
#' @return
#' @export
#'
#' @examples
visualizeWDN <- function(nodes.data = NULL,
                         pipes.data = NULL,
                         node.fill.var = NULL,
                         node.size.var = NULL,
                         edge.color.var = NULL,
                         edge.color.threshold = 95,
                         edge.size.var = NULL,
                         background.map = NULL,
                         show.legend = TRUE,
                         plot.title = NULL)
  
{
  
  require(dplyr)
  require(ggplot2)
  require(ggmap)
  require(ggnetwork)
  require(ggnewscale)
  require(magrittr)
  require(scales)
  
  # Set ggplot2 theme
  ggtheme_network <- theme_light() + 
    theme(legend.background = element_rect(fill = "white"),
          legend.key=element_blank(),
          legend.key.size = unit(0.6, "cm"),
          legend.spacing.x = unit(0.2, 'cm'),
          legend.spacing.y = unit(0.5, 'cm'),
          #plot.title.position = "plot",
          plot.margin = unit(c(0.5, 0, 0, 0), "cm") #trbl 
    )

  # Supply node aesthetics
  snode_color <- "blue"
  snode_fill <- "#c6dbef"
  snode_size <- 8
  snode_shape <- 22
  snode_stroke <- 1.5
  
  # Demand node aesthetics
  dnode_color <- "gray50"
  dnode_fill <- "gray50"
  dnode_size <- 8
  dnode_shape <- 21
  dnode_stroke <- 1
  
  # Edge/link aeasthetics
  edge_size <- 1

  # Axis labels
  xlab <- expression("Lon "(degree))
  ylab <- expression("Lat "(degree))

  # Prepare node/pipe tables
  nodesDF <- nodes.data
  pipesDF <- pipes.data %>%
    left_join(y = (nodesDF %>% select(start = id, lon, lat)), by = "start") %>%
    left_join(y = (nodesDF %>% select(end = id, lon_end = lon, lat_end = lat)), by = "end") %>%
    mutate(lon_mid = (lon + lon_end)/2, 
           lat_mid = (lat + lat_end)/2)
  

  if(is.null(node.fill.var)) {
    nodesDF$nodefill <- dnode_fill
  } else {
    nodesDF$nodefill <- nodesDF[[node.fill.var]]
  }
  
  if(is.null(node.size.var)) {
    nodesDF$nodesize <- dnode_size
  } else {
    nodesDF$nodesize <- nodesDF[[node.size.var]]
  }
  
  if(is.null(edge.color.var)) {
    pipesDF$edgecol <- 0
  } else {
    pipesDF$edgecol <- pipesDF[[edge.color.var]]
  }
  
  pipesDF$edgecol <- ifelse(pipesDF[["disuse"]] > 0, "2", pipesDF$edgecol)
  
  if(is.null(edge.size.var)) {
    pipesDF$edgesize <- edge_size
  } else {
    pipesDF$edgesize <- pipesDF[[edge.size.var]]
  }
  
  edge_size_breaks <- c(100,200, 300,400, 500,600, 700) #pipesDF %>% pull(diameter) %>% pretty(3)
  node_size_breaks <- seq(0,2500,500)
  
  ##############################################################################
  
  # Main plot elements
  gg_layer0 <-  ggmap(background.map, extend = "device")  + ggtheme_network +
    labs(x = NULL, y = NULL) 

  # Edge layers
  gg_layer1 <-  list(
    geom_edges(aes_string(x = "lon", y = "lat", xend = "lon_end", 
                    yend = "lat_end", color = "edgecol", size = "edgesize"), 
               data = pipesDF),
    scale_size(
        name = "Pipe\nDiameter\n[mm]",
        limits = range(edge_size_breaks), 
        breaks = edge_size_breaks, 
        range = c(0.6, 4), 
        guide = guide_legend(order = 5)),
    
    scale_color_viridis(
          name = "Pipe\nUse [%]",
          limits = c(0,100),
          breaks = c(0,20, 40, 60, 80, 99, 100),
          labels = c(0, 20, 40, 60, 80,"", 100),
          guide = guide_colorbar(order = 3, barheight = unit(5, 'cm')), direction = -1)
          
    )
    
  # Node layers
  gg_layer2 <- list(
    geom_nodes(
          aes_string(x = "lon", y = "lat", size = "nodesize"),
    data = filter(nodesDF, type == "supply"), fill = snode_fill,
    stroke = snode_stroke, shape = dnode_shape, color = snode_color),
    
    # Draw demand nodes
    geom_nodes(
          aes_string(x = "lon", y = "lat", fill = "nodefill", size = "nodesize"),
      data = filter(nodesDF, type == "demand"),
      stroke = dnode_stroke, shape = dnode_shape, color = dnode_color),
    
    scale_fill_steps2(
      name = "Reliability\n[%]",
      low = "red", mid = "white", high = "white", midpoint = 99, 
      limits = c(0,100),
      breaks = c(0,20, 40, 60, 80, 99, 100),
      labels = c(0, 20, 40, 60, 80,"", 100),
      guide = guide_colorbar(order = 1, barheight = unit(5, 'cm'))),
    
    scale_size(
      name = "Water\nDemand\n[m3]",
      limits = range(node_size_breaks), 
      breaks = node_size_breaks, 
      range = c(5,14), 
      guide = guide_legend(order = 2, keyheight  = unit(1, 'cm')))
  )
    

  
  p0 <- gg_layer0 + gg_layer1 + new_scale("size") + gg_layer2 + 
    theme(legend.position = "none") +
    geom_text(aes(x = lon, y = lat, label = label), data = nodesDF, size=3.5) +
    ggtitle(label = plot.title)  


  # Display failures on figure
  if(nrow(nodesDF %>% filter(disuse == 1)) > 0) {
    
    p0 <- p0 + geom_label(aes(x = lon, y = lat, label = "X"), 
        size = 5, alpha = 0.5, color = "white", fill = "green",
        data = nodesDF %>% filter(disuse == 1))
  }
  if(nrow(pipesDF %>% filter(disuse == 1)) > 0) {
    
    p0 <- p0 + geom_label(aes(x = lon_mid, y = lat_mid, label = "X"), 
        size = 3, alpha = 0.5, color = "white", fill = "green",
        data = pipesDF %>% filter(disuse == 1))
}
 

  if (show.legend == FALSE) {
    
    return(p0)
  
  } else {
    
    p1 <-  gg_layer0 + gg_layer2 + theme(legend.justification = "top")
    p2 <-  gg_layer0 + gg_layer1 + theme(legend.justification = "top")
    
    leg <- plot_grid(get_legend(p1), get_legend(p2)  , ncol = 2)
    
    p <- plot_grid(p0, leg, ncol = 2, align = "v", axis = "t",
                   rel_widths = c(1, 0.25)) 
    
    return(p)
  }

}
