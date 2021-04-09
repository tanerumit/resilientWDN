

################################################################################
# Network Analysis based on Network Theory
################################################################################



# Load scripts & functions
source("./src/setup.R")

# Directories to read-in data and save results
inPath  <- "./data/v8_2021/"
outPath <- "./results/v8_2021/"

# Read_in node and pipe schematic from csv files: 
designNum <- 5



nodes  <- read.csv(paste0(data_dir,"nodes.csv"))
pipes  <- lapply(1:designNum, function(x) read.csv(paste0(data_dir,"pipes_d", x, ".csv")))

metricNames <- c("linkdens", "avgdegree", "diameter", "clustering", "articdens", "bridgedens")
designNames <- paste0("Design ", 1:designNum)

nMetrics <- replicate(designNum, list())
for (i in 1:designNum) {
  
  # Generate network
  nodes_df <- nodes %>% select(name = node_id, type:disuse, lat, lon)
  pipes_df <- pipes[[i]] %>% select(from = start, to = end, length:disuse)
  
  # Define network as undirected
  net <- graph_from_data_frame(d=pipes_df, vertices=nodes_df, directed = FALSE) 
  
  # Number of nodes & links
  nodeNum <- nrow(nodes_df)
  pipeNum <- nrow(pipes_df)
  
  ######### Statistical measures #########

  # linkDensity: most basic indicator of linkedness/sparseness of the network
  nMetrics[[i]]$linkdens   <- edge_density(net, loops = TRUE)

  # avgDegree:  average node degree within the network 
  nMetrics[[i]]$avgdegree  <- mean(degree(net))
  
  # distDegree:  node degree distribution within the network 
  #nMetrics[[i]]$distDegree <- degree_distribution(net, mode = "all")
  
  # diameter: max geodesic distance between any two nodes
  nMetrics[[i]]$diameter   <- diameter(net, directed = FALSE, weights = pipes[[i]]$diameter)   
  
  # clustring: quantifies the density of triangular loops and the degree to which junctions in a graph tend to be linked
  nMetrics[[i]]$clustering <- transitivity(net, type="global")
  
  #articDensity: Density of nodes, whose removal disconnects the network
  nMetrics[[i]]$articdens  <- length(articulation_points(net))/nodeNum
  
  #bridgeDensity: links whose removal disconnects the network) and articulation points 
  nMetrics[[i]]$bridgedens <- if(i %in% 1:2) {NA} else {
    length(which(bridge(net, directed = FALSE, normalize = TRUE)$`Bridge Strength` > 0))/pipeNum}
 
}

df <- data.frame(matrix(unlist(nMetrics), nrow=length(metricNames), byrow=F), stringsAsFactors=FALSE) 
colnames(df) <- designNames
df <- df %>% mutate_all(.funs = round, 2) %>% add_column(metric = metricNames,.before = 1) 


nMetrics[[1]][[1]]
nMetrics[[2]][[1]]
nMetrics[[3]][[1]]
nMetrics[[4]][[1]]
nMetrics[[5]][[1]]

#(nodes whose removal along with the removal of their incident links disconnects the network).
#nMetrics[[i]]$betweenness   <- max(betweenness(net, directed = F, normalized = TRUE)) 
#nMetrics[[i]]$closeness     <- closeness(net) #estimate_closeness(net, weights = NULL, cutoff = 0) 

#########

# Cost comparison 

# Capital Expenditure of design alternatives (in million EUROS)
#€1,-/mm/m' --> E.g.: a 275 mm pipe costs 275 € per m'


design_df1 <- tibble(design = paste0("Design ",1:designNum)) %>% 
  mutate(pipe_num = sapply(1:designNum, function(x) nrow(pipes[[x]]))) %>%
  mutate(pipe_len = sapply(1:designNum, function(x) sum(pipes[[x]]$length)/10^3)) %>%
  mutate(pipe_CAPEX = sapply(1:designNum, function(x) sum(pipes[[x]]$length * pipes[[x]]$diameter)/10^6))  

p <- ggplot(design_df1, aes(x = design, y = pipe_len, size = pipe_CAPEX, color = design)) +
  theme_light() + 
  theme(legend.background = element_blank()) +
  theme(legend.key=element_blank()) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(2, 10), name="Capital \nExpanditure \n(M.Euro)") +
  scale_color_viridis(discrete = TRUE, alpha=0.7) +
  labs(x = "", y = "pipe length (km)") +
  guides(color = FALSE)

ggsave(paste0(plot_dir,"./design_comparison.png"), height = 5, width = 6)





###Statistical measurements

# Link Density: is the most basic indicator of the overall linkedness or sparseness of the structure of a network. 
# This metric is not scale invariant and hence its magnitude may change by changing the network size

#Average node degree < k > accompanied by the degree distribution (i.e. histogram of the node degrees) 
# is a basic measure of the connectivity. It reflects the overall topological similarity of the network to 
# perfect grids or lattice-like structures, important toward equalized distribution of flow and pressure under varying demands.


# 
# distDegree:   degree distribution of nodes within the network 
# nDiamater:    the longest of shortest paths between two nodes within the network
# avgLength:    average number of links that need to be traversed in order to reach from one point to another
# clusterC:     quantifies the density of triangular loops and the degree to which junctions in a graph tend to be linked
# btween:       the number of shortest paths going through an edge.
# closeness:
# bridgeDen
# articulDens

