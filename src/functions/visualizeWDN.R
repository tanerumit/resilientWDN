

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
                         background.map = NULL)
  
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
          legend.key.size = unit(0.6, "cm"))

  # Supply node aesthetics
  snode_color <- "blue"
  snode_fill <- "#c6dbef"
  snode_size <- 8
  snode_shape <- 22
  snode_stroke <- 1.2
  
  # Demand node aesthetics
  dnode_color <- "gray50"
  dnode_fill <- "gray50"
  dnode_size <- 8
  dnode_shape <- 21
  dnode_stroke <- 1
  
  
  # Edge/link aeasthetics
  edge_size <- 1
  edge_color <- c("0" = "red", "1" = "black", "2" = "green")

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
    pipesDF$edgecol <- ifelse(pipesDF[[edge.color.var]] > edge.color.threshold, "0", "1")
  }
  
  pipesDF$edgecol <- ifelse(pipesDF[["disuse"]] > 0, "2", pipesDF$edgecol)
  
  if(is.null(edge.size.var)) {
    pipesDF$edgesize <- edge_size
  } else {
    pipesDF$edgesize <- pipesDF[[edge.size.var]]
  }
  
  edge_size_breaks <- c(100,300,500,700) #pipesDF %>% pull(diameter) %>% pretty(3)
  dnode_size_breaks <- nodesDF %>% filter(type == "demand") %>% pull(discharge) %>% pretty(5)

  nodesDF$nodefill[1] <- 100
  ##############################################################################
  
  # Main plot elements
  p <- ggmap(background.map, extend = "device")  + 
    ggtheme_network + 

    # Draw pipe network
    geom_edges(
        aes_string(x = "lon", y = "lat", xend = "lon_end", yend = "lat_end", color = "edgecol", size = "edgesize"),
        data = pipesDF) +
    scale_size(limits = range(edge_size_breaks), breaks = edge_size_breaks, range = c(0.6, 4), 
                guide = guide_legend(order = 3, title = "Pipe Diameter [mm]")) +
    scale_color_manual(values = edge_color, labels = c(" < 95", " >= 95", "NotInUse"),
                        guide = guide_legend(order = 4, title = "Pipe Use [%]",
                                             override.aes = list(size = 3))) +
    new_scale("size") +
    
    # Draw supply nodes
    geom_nodes(
      mapping = aes_string(x = "lon", y = "lat"),
      data = filter(nodesDF, type == "supply"),
      fill = snode_fill, size = snode_size, stroke = snode_stroke,
      shape = snode_shape, color = snode_color) +
    
    # Draw demand nodes
    geom_nodes(
        aes_string(x = "lon", y = "lat", fill = "nodefill", size = "nodesize"),
        data = filter(nodesDF, type == "demand"),
        stroke = dnode_stroke, shape = dnode_shape, color = dnode_color) + 
    
    scale_fill_steps2(low = "red", mid = "white", high = "white", midpoint = 99, 
                      limits = c(0,100),
                      breaks = c(0,20, 40, 60, 80, 99, 100),
                      labels = c(0, 20, 40, 60, 80,"", 100),
                         guide = guide_colorbar(order = 1, title = "Reliability [%]", 
                                                frame.color = "black", barheight = unit(4, 'cm'))) +

    scale_size(limits = range(dnode_size_breaks), breaks = dnode_size_breaks, range = c(5,12), 
        guide = guide_legend(order = 2, title = "Demand [m3]")) +

    # Node labeling
    geom_text(aes(x = lon, y = lat, label = label), data = nodesDF, size=3.5) +
    
    # Set axis/guide labels
    labs(x = NULL, y = NULL)

  if(nrow(nodesDF %>% filter(disuse == 1)) > 0) {
    
    p <- p + 

      geom_label(aes(x = lon, y = lat, label = "X"), 
                        size = 5, alpha = 0.5, color = "white", fill = "green",
                        data = nodesDF %>% filter(disuse == 1))
  }
  
  
  if(nrow(pipesDF %>% filter(disuse == 1)) > 0) {
    
    p <- p + 
      
      geom_label(aes(x = lon_mid, y = lat_mid, label = "X"), 
                        size = 3, alpha = 0.5, color = "white", fill = "green",
                        data = pipesDF %>% filter(disuse == 1))
  }
  

  return(p)
  
  
  
}