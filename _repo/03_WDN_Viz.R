

# Data tables for nodes & pipes (dummy version)
nodes <- read.csv("./data/vitens_nodes_dummy.csv")  
pipes_com <- read.csv("./data/vitens_pipes_dummy_combined.csv")
pipes_iso <- read.csv("./data/vitens_pipes_dummy_isolate.csv")

load("./data/mapFriesland.rda")
backround.map <- mapFriesland
nodes <- vitens_nodes
pipes <- vitens_pipes



#devtools::install_github("briatte/ggnetwork")

 

# Rescale demand sizes
#nodesDF$demand2 <- scales::rescale(nodesDF$demand, to = c(5, 10))

### PLotting with GGMAP (requires GOOGLE API)



##################################################################################



#vitens_nodes_df <- vitens_nodes
# vitens_pipes_df <- vitens_pipes
# 
# # network object
# WDnet <- network(vitens_pipes_df, directed = TRUE)
# rownames(vitens_nodes_df) <- vitens_nodes_df$name
# 
# # add geographic coordinates
# WDnet %v% "lat" <- vitens_nodes_df[ network.vertex.names(WDnet), "lat" ] 
# WDnet %v% "lon" <- vitens_nodes_df[ network.vertex.names(WDnet), "lon" ] 
# 
# # add node type
# WDnet %v% "demand" <- as.character(vitens_nodes_df[network.vertex.names(WDnet), "demand" ]) 
# WDnet %v% "nodetype" <- as.character(vitens_nodes_df[network.vertex.names(WDnet), "type" ]) 
# summary.network(WDnet)
# 
# # overlay network data to map
# ggnetworkmap(gg = p1, net = WDnet, 
#              size = 4, 
#              segment.color = "gray20", great.circles = TRUE,
#              segment.size = 1.5, 
#              label.nodes = TRUE, label.size = 4,
#              node.group = nodetype) +
#   scale_color_manual(values = c("green","orange"))
# 
