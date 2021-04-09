


################################################################################
### Visualization of Design alternatives
################################################################################

source("./src/00 global.R")

# Saving directories
dataPath <- "./data/v7/"
resultsPath <- "./results/v7/"

# Source python script 
load(paste0(dataPath, "mapFriesland.rda"))

# Read_in node and pipe schematic from csv files: 
designNum <- 5
nodes  <- read.csv(paste0(dataPath,"nodes.csv"))
pipes  <- lapply(1:designNum, function(x) read.csv(paste0(dataPath,"pipes_d", x, ".csv")))



xlab <- expression("Longitude " ( degree))
ylab <- expression("Latitude " ( degree))
pheight <- 8
pwidth  <- 8

# Node data
friesland_nodes <- read.csv("./data/v7/nodes.csv")  
nodes <- friesland_nodes
nodesDF <- nodes %>% 
  select(node_id, name, type, weight, discharge, elevation, max_supply_head, 
         disuse, y = lat, x = lon)

# Background map
background.map <- mapFriesland

# template plot
p0 <- ggmap(background.map, extend = "device") + 
  ggtheme_nw + scale_fill_brewer(palette = "Set2") +
  labs(x = xlab, y = ylab, fill = "Node type") +
  guides(color = FALSE)

  
########## Only Nodes
p <- p0 +  
  geom_nodes(aes(x=x, y=y, fill=type), data = nodesDF, 
             stroke = 1.2, shape = 21, size = 8, color ="gray30") +
  geom_text(aes(x=x, y=y, label=node_id), data = nodesDF)
ggsave(filename = paste0(resultsPath, "friesland_nodes.png"), width = pwidth, height = pheight)


########## DESIGNS 
for (x in 1:designNum) {
  
  # Set pipe network
  pipesDF <- pipes[[x]] %>%
    left_join(y = (nodesDF %>% select(start = node_id, x, y))) %>%
    left_join(y = (nodesDF %>% select(end = node_id, xend = x, yend = y)))
  
  # Plot current design
  p <- p0 + #ggtitle(paste0("Friesland Network Design: ", x)) +
    geom_edges(aes(x=x, y=y, xend=xend, yend=yend, color = colid), data=pipesDF, size=1) +
    geom_nodes(aes(x=x, y=y, fill=type), size = 8, data = nodesDF, stroke = 1.2, shape = 21, color ="gray30") +
    geom_text(aes(x=x, y=y, label=node_id), data = nodesDF) 
  
  ggsave(filename = paste0(resultsPath, "friesland_design",x ,".png"), width = pwidth, height = pheight)
  
}

nodesDF2 <- nodesDF %>% filter(type == "demand")


# Plot only demand 
p <- ggmap(background.map, extend = "device") + 
  theme_light() + 
  scale_fill_distiller(palette = "Reds", direction = 1, limits = c(0, 720), breaks = seq(0,720,100)) +
  scale_size(range = c(3, 15)) +
  labs(x = xlab, y = ylab, fill = "Demand") +
  guides(color = FALSE, size = FALSE, fill = FALSE) + #fill = guide_colourbar(barwidth = 1, barheight = 15, legend.position="bottom")) +
  geom_nodes(aes(x=x, y=y, fill=discharge, size = discharge), data = nodesDF2, stroke = 1.2, shape = 21, color ="gray30") +
  geom_text(aes(x=x, y=y, label=node_id), data = nodesDF2) 
  
ggsave(paste0(resultsPath, "demand_viz.png", width = 7, height = 5))


 # nodesDF %>% select(ID = node_id, Name = name, Type = type) %>%
#   filter(ID <25) %>%
#   mutate(Type = cell_spec(Type, "html", color = ifelse(Type == "demand", "#66c2a5", "#fc8d62"))) %>%
#   kable(format = "html", escape = F) %>%
#   kable_styling(c("striped", "condensed"), full_width = F,font_size = 14) %>%
#   save_kable("./graphics/node_names_1.png")
# 
# nodesDF %>% select(ID = node_id, Name = name, Type = type) %>%
#   filter(ID >24) %>%
#   mutate(Type = cell_spec(Type, "html", color = ifelse(Type == "demand", "#66c2a5", "#fc8d62"))) %>%
#   kable(format = "html", escape = F) %>%
#   kable_styling(c("striped", "condensed"), full_width = F,font_size = 14) %>%
#   save_kable("./graphics/node_names_2.png")



