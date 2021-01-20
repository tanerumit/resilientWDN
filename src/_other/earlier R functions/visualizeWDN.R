
#' Title
#'
#' @param nodes 
#' @param pipes 
#' @param backround.map 
#'
#' @return
#' @export
#'
#' @examples
visualizeWDN <- function(nodes = NULL, pipes = NULL, background.map = NULL) {
  
  require(dplyr)
  require(ggplot2)
  require(ggmap)
  require(ggnetwork)
  
  
  # Prepare node table
  nodesDF <- nodes %>% 
    select(node_id, name, type, weight, discharge, elevation, max_supply_head, 
           disuse, y = lat, x = lon)
  
  # Prepare pipe table
  pipesDF <- pipes %>%
    left_join(y = (nodesDF %>% select(start = node_id, x, y))) %>%
    left_join(y = (nodesDF %>% select(end = node_id, xend = x, yend = y)))
  
  # Set ggplot2 theme
  ggtheme_nw <- theme_light() + 
    theme(legend.background = element_blank()) +
    theme(legend.key=element_blank()) +
    theme(legend.position = c(0.1, 0.9)) 
  
  xlab <- expression("Longitude " ( degree))
  ylab <- expression("Latitude " ( degree))
  

  if(is.null(background.map)) {
    p <- ggplot()
  } else {
    p <- ggmap(background.map, extend = "device")
  }
  
  p <- p + 
    ggtheme_nw +
    # Define edges of the network
    geom_edges(aes(x=x, y=y, xend=xend, yend=yend), data=pipesDF, size=1, color = "gray30") +
    # Define nodes of the network
    geom_nodes(aes(x=x, y=y, fill=type), size = 8,
               data = nodesDF, stroke = 1.2, shape = 21, color ="gray30") +
    # Define labeling
    #geom_text_repel(aes(x=x, y=y, label=node_id), data = nodesDF,
    #box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50')
    geom_text(aes(x=x, y=y, label=node_id), data = nodesDF) +
    # Define aesthetics
    scale_fill_brewer(palette = "Set2") +
    #scale_fill_manual(values = c("orange","green")) +
    labs(x = xlab, y = ylab, fill = "Node type")
  
  
  return(p)  
  
}