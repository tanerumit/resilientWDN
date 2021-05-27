

#::::::::::::::::::::: GRAPH THEORY ::::::::::::::::::::::::::::::::::::::::::::


metricNames <- c("linkdens", 
                 "avgdegree", 
                 "diameter", 
                 "clustering", 
                 "articdens", 
                 "bridgedens")

designNames <- paste0("Design ", 1:dmax)
nMetrics <- list()
edgeCon <- vector(mode = "list", length = dmax)


for (d in 1:dmax) {
  
  # Read-in network data 
  nodes_d <- nodes_data[[d]]
  pipes_d <- pipes_data[[d]]
  
  # Set demand and supply node indices
  snodes <- nodes_d$id[which(nodes_d$type == "supply")]
  snodes_labs <- nodes_d %>% filter(id %in% snodes) %>% pull(label)
  
  dnodes <- nodes_d$id[which(nodes_d$type == "demand")]
  
  # Number of nodes & links
  nnodes <- nrow(nodes_d)
  npipes <- nrow(pipes_d)
  
  
  # Set spare capacity
  nodes_d$discharge[snodes] <- nodes_d$discharge[snodes] * 1.05
  
  
  # Define network as undirected
  G <- graph_from_data_frame(d=pipes_d, vertices=nodes_d, directed = FALSE) 
  
  #### Edge connectivity matrix
  foo <- matrix(NA, nrow = length(dnodes), ncol = length(snodes))
  for (d in 1:length(dnodes)) {
    foo[d,] <- sapply(snodes, function(x) 
      edge_connectivity(G, source = dnodes[d], target = x))
  }
  edgeCon[[d]] <- tibble(id = dnodes) %>% bind_cols(as_tibble(foo)) %>% 
    setNames(c("id", snodes_labs))
  
  
  #### Bridges in the network 
  pipes_d$bridge <- 0
  num_comp <- length(decompose.graph(G))
  for (x in 1:npipes) {
    G_sub <- delete.edges(G, x)
    if (length(decompose.graph(G_sub)) > num_comp) pipes_d$bridge[x] <- 1
  }
  
  # Calculate Graph Measures
  nMetrics$linkdens   <- edge_density(G, loops = TRUE)
  nMetrics$avgdegree  <- mean(degree(G))
  nMetrics$diameter   <- diameter(G, directed = FALSE)   
  nMetrics$clustering <- transitivity(G, type="global")
  nMetrics$articdens  <- length(articulation_points(G))/nnodes
  nMetrics$bridgedens <- if(i %in% 1:2) {NA} else {
    length(which(bridge(G, directed = FALSE, normalize = TRUE)$`Bridge Strength` > 0))/npipes}
  
}




