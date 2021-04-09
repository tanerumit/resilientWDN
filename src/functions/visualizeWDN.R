# 


# nodes.data = nodes_out 
# pipes.data = pipes_out 
# node.color.var = "coverage"
# node.size.var = "discharge"
# edge.color.var = "dummy"
# edge.size.var = NULL
# background.map = mapFriesland
# scale = FALSE
# fill = FALSE


#' Title
#'
#' @param nodes.data 
#' @param pipes.data 
#' @param node.color.var 
#' @param node.size.var 
#' @param edge.color.var 
#' @param edge.size.var 
#' @param background.map 
#'
#' @return
#' @export
#'
#' @examples
visualizeWDN <- function(nodes.data = NULL,
                         pipes.data = NULL,
                         node.color.var = NULL,
                         node.size.var = NULL,
                         edge.color.var = NULL,
                         edge.size.var = NULL,
                         background.map = NULL,
                         ...)
  
{
  
  require(dplyr)
  require(ggplot2)
  require(ggmap)
  require(ggnetwork)
  require(magrittr)
  
  # General parameters
  snode_color <- "gray90"
  snode_size <- 6
  
  dnode_color <- "blue"
  dnode_size <- 8
  
  edge_color <- "gray20"
  edge_size <- 1

  
  # Axis labels
  xlab <- expression("Lon "(degree))
  ylab <- expression("Lat "(degree))
  
  #nodes.data = nodes_out
  #node.color.var = "coverage"
  #node.color.var = NULL
  
  # Prepare pipe table
  pipes.data %<>%
    left_join(y = (nodes.data %>% select(start = id, lon, lat)), by = "start") %>%
    left_join(y = (nodes.data %>% select(
      end = id,
      lon_end = lon,
      lat_end = lat
    )), by = "end")
  
  if(is.null(node.color.var)) {
    nodes.data$nodecol <- dnode_color
  } else {
    nodes.data$nodecol <- nodes.data[[node.color.var]]
  }
  
  if(is.null(node.size.var)) {
    nodes.data$nodesize <- dnode_size
  } else {
    nodes.data$nodesize <- nodes.data[[node.size.var]]
  }
  
  if(is.null(edge.color.var)) {
    pipes.data$edgecol <- edge_color
  } else {
    pipes.data$edgecol <- pipes.data[[edge.color.var]]
  }
  
  if(is.null(edge.size.var)) {
    pipes.data$edgesize <- edge_size
  } else {
    pipes.data$edgesize <- pipes.data[[edge.size.var]]
  }
  
  wbreaks <- filter(nodes.data, type == "demand") %>% pull(discharge) %>% pretty(5)
  
  
  # Base layer & background MAP
  if (!is.null(background.map)) {
    p <- ggmap(background.map, extend = "device")
  } else {
    p <- ggplot()
  }
  
  
  # Main plot elements
  p <- p  + ggtheme_nw +
    
    # Draw pipe network
    geom_edges(
      aes_string(x = "lon", y = "lat", xend = "lon_end", yend = "lat_end", 
                 size = "edgesize", color = "edgecol"),
      data = pipes.data,
      size = edge_size,
    ) +
    
    # Draw demand/supply nodes
    geom_nodes(
      mapping = aes_string(x = "lon", y = "lat"),
      data = filter(nodes.data, type == "supply"),
      fill = snode_color,
      size = snode_size,
      stroke = 1,
      shape = 22,
      color = "gray30"
    ) +

    geom_nodes(
      aes_string(x = "lon", y = "lat", fill = "nodecol", size = "nodesize"),
      data = filter(nodes.data, type == "demand"),
      stroke = 1,
      shape = 21,
      color = edge_color
    ) +
    
    # Set scales (size/color)
    scale_size(
      limits = range(wbreaks),
      breaks = wbreaks,
      range = c(6, 12)
    ) +
    
    
    # Set guides/labels
    geom_text(aes(x = lon, y = lat, label = label), data = nodes.data) +
    labs(
      x = xlab,
      y = ylab,
      fill = "",
      size = ""
    ) 
  
    if(!is.null(node.color.var)) {
      p <- p + scale_fill_distiller(palette = "YlGnBu") 
    }
  
    if(!is.null(edge.color.var)) {
      p <- p + scale_color_brewer(palette = "Dark2") 
    }
  
    return(p + guides(...))
  
  
  
}
