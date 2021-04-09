
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
visualizeWDN <- function(
  nodes.df = NULL, 
  pipes.df = NULL, 
  background.map = NULL,
  metric = NULL,
  display.node.wgts = FALSE,
  display.constraints = FALSE) {
  
  require(dplyr)
  require(ggplot2)
  require(ggmap)
  require(ggnetwork)
  
  # General parameters
  snode_color <- "gray90"
  snode_size <- 6
  dnode_size <- 8
  edge_size <- 1
  
  # Prepare node table
  nodesDF <- nodes.df %>% select(everything(), var = all_of(metric)) %>%
    rename(x = lon, y = lat)
    
  # Prepare pipe table
  pipesDF <- pipes.df %>%
    left_join(y = (nodesDF %>% select(start = id, x, y)), by = "start") %>%
    left_join(y = (nodesDF %>% select(end = id, xend = x, yend = y)), by = "end")

  xlab <- expression("Lon "(degree))
  ylab <- expression("Lat "(degree))
  
  
  # Base layer & background MAP
  if(!is.null(background.map)) {
    p <- ggmap(background.map, extend = "device") + ggtheme_nw
  } else {
    p <- ggplot() + ggtheme_nw
  }

  # If pipe weights are displayed
  if(isTRUE(display.constraints)) {
    
    sdf1 <- nodesDF %>% filter(type == "supply" & var < -99.9)
    sdf2 <- nodesDF %>% filter(type == "supply" & var >= -99.9)
    
    # Draw edges in the network
    p <-  p + 
      
      geom_edges(aes(x=x, y=y, xend=xend, yend=yend), 
                 data = filter(pipesDF, coverage> 99), 
                 size = edge_size+0.5, color = "red") + 
      
      geom_edges(aes(x=x, y=y, xend=xend, yend=yend), 
                 data = filter(pipesDF, coverage <= 99), 
                 size = edge_size, color = "gray30") +
      
      # Draw supply nodes of the network
      geom_nodes(data = sdf1,
                 mapping = aes(x=x, y=y), 
                 fill = snode_color, size = snode_size, stroke = 1.2, shape = 22, 
                 color = "red") +
      
      # Draw supply nodes of the network
      geom_nodes(data = sdf2,
                 mapping = aes(x=x, y=y), 
                 fill = snode_color, size = snode_size, stroke = 1, shape = 22, 
                 color ="gray30")

  }  else {
    
    # Draw edges in the network
    p <- p + 
      
      geom_edges(aes(x=x, y=y, xend=xend, yend=yend), 
                data = pipesDF, size = edge_size, color = "gray30") +
      
      # Draw supply nodes of the network
      geom_nodes(data = filter(nodesDF, type == "supply"),
                 mapping =  aes(x=x, y=y), 
                 fill = snode_color, size = snode_size, stroke = 1, shape = 22, 
                 color ="gray30")
    
  } 
  
  # If node weights are displayed
  if(isTRUE(display.node.wgts)) {
      
      wbreaks <- filter(nodesDF, type == "demand") %>% pull(discharge) %>% pretty(5)
      
      p <- p + 
        
        geom_nodes(data = filter(nodesDF, type == "demand"),
                   mapping = aes(x=x, y=y, fill = var, size = discharge), 
                   stroke = 1, shape = 21, 
                   color ="gray30") +
        
        scale_size(limits = range(wbreaks), breaks = wbreaks, range = c(6,12)) 
        
    } else {
     p <- p + 
       
       geom_nodes(data = filter(nodesDF, type == "demand"),
                 mapping = aes(x=x, y=y, fill = var), 
                 stroke = 1, shape = 21, size = dnode_size, color ="gray30")
    }  

  return( 
    
    p + 
      
      geom_text(aes(x=x, y=y, label=label), 
                       data = filter(nodesDF, type == "demand")) +
    
      geom_text(aes(x=x, y=y, label=label), 
                      data = filter(nodesDF, type == "supply")) +
            
      labs(x = xlab, y = ylab, fill = "", size="") +
      
      scale_fill_distiller(palette = "YlGnBu") +
      #scale_fill_gradient(low = "red", high = "white") +
          
      guides(scale = FALSE)

  )  
  
}



# ggmap(background.map, extend = "device") + 
#   theme_light() + 
#   scale_fill_distiller(palette = "Reds", direction = 1, limits = c(0, 51), breaks = seq(0,50,10)) +
#   scale_size(range = c(2, 12)) +
#   labs(x = xlab, y = ylab, fill = "Shortage\nocurrance (%)") +
#   guides(color = FALSE, size = FALSE, fill = guide_colourbar(barwidth = 1, barheight = 15)) +
#   ggtitle(paste0("Design: ", x)) +
#   geom_edges(aes(x=x, y=y, xend = xend, yend=yend, color = colid), data=pipesDF_out, size=1) +
#   geom_nodes(aes(x=x, y=y), filter(nodeDF_out, type == "supply"), size = 5, stroke = 1.2, shape = 21, color ="gray30") +
#   geom_nodes(aes(x=x, y=y, fill = failRate, size = demand), filter(nodeDF_out, type == "demand"), stroke = 1.2, shape = 21, color ="gray30") +
#   geom_text(aes(x=x, y=y, label = node_id), data = nodesDF) 