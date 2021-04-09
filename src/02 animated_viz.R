

### Animated Version of network plot

year_range <- 2021:2071
frame_num <- length(year_range)
nodesDF_all <- NULL

for (i in 1:length(year_range)) {
  
  yearc <- year_range[i]
  
  result <- simulateWDN(
    dev_rate = 0.0005,  
    tmp_rate = 0.04,   
    prc_rate  = 0.01,   
    pop_rate  = 0.01,     
    dev_elast = 1.0,   
    prc_elast = -0.2,    
    tmp_elast = 0.03,   
    indf = 0.3,          
    year_sim = yearc,      
    year_ref = 2021,  
    nodes_ref = nodes_data,  
    pipes_ref = pipes_data   
  )
  
  # Performance per node
  nodesDF <- nodes_data %>% 
    mutate(coverage = 100 * result$final/result$initial) %>%
    mutate(diff = result$initial - result$final) %>% mutate(year = yearc)
  
  nodesDF_all <- bind_rows(nodesDF_all, nodesDF)
  
}

# Set ggplot2 theme
ggtheme_nw <- theme_light() + 
  theme(legend.background = element_blank()) +
  theme(legend.key=element_blank()) +
  theme(legend.position = c(0.1, 0.9)) 

# Prepare node table
nodesDF <- nodesDF_all %>% 
  select(node_id, name, type, weight, discharge, elevation, max_supply_head, 
         disuse, y = lat, x = lon, var = coverage, year) %>%
  mutate(var = round(var))

# Prepare pipe table
pipesDF <- pipes_data %>%
  left_join(y = (nodesDF %>% select(start = node_id, x, y)), by = "start") %>%
  left_join(y = (nodesDF %>% select(end = node_id, xend = x, yend = y)), by = "end")

xlab <- expression("Lon "(degree))
ylab <- expression("Lat "(degree))


p <- ggmap(mapFriesland, extend = "device") + ggtheme_nw +
  
  # Draw edges in the network
  geom_edges(aes(x=x, y=y, xend=xend, yend=yend), 
             data = pipesDF, size=1, color = "gray30") +
  
  # Draw supply nodes of the network
  geom_nodes(data = filter(nodesDF, type == "supply"),
             mapping =  aes(x=x, y=y), 
             fill = "steelblue1", size = 6, stroke = 1, shape = 22, 
             color ="gray30") +
  
  # Draw demand nodes of the network
  geom_nodes(data = filter(nodesDF, type == "demand"),
             mapping = aes(x=x, y=y, fill = var), 
             size = 8, stroke = 1, shape = 21, 
             color ="gray30") +
  
  # Define labeling
  geom_text(aes(x=x, y=y, label=node_id), data = filter(nodesDF, type == "demand")) +
  geom_text(aes(x=x, y=y, label=node_id), data = filter(nodesDF, type == "supply")) +
  
  # Define aesthetics
  scale_fill_gradient(low = "red", high = "white") +
  
  # GGanimate
  labs(title = 'Year: {frame_time}', x = xlab, y = ylab, fill = "") +
  transition_time(time = year) +
  ease_aes('linear')

anim_save("wdn_animated.gif", p, width = 1200, height = 1200, res = 150, 
          nframes = frame_num, fps = 3, end_pause = 6)

