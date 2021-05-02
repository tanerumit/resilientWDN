


#:::::::::::::::::::: MODEL SETUP ::::::::::::::::::::::::::::::::::::::::::::::

# Load scripts & functions
source("./src/setup.R")
source("./src/functions/simulateWDN.R")
source("./src/functions/visualizeWDN.R")

# Directories to read-in data and save results
inPath  <- "./data/v9/"
outPath <- "./results/v9/"

# Background map
load(file = paste0(inPath, "mapFriesland.rda"))

# Read_in base network from excel 
nodes_data_base <- read_excel(paste0(inPath,"nodes_d1.xlsx"))
pipes_data_base <- read_excel(paste0(inPath,"pipes_d1.xlsx"))

# Store all options in list
nodes_data <- list()
pipes_data <- list()
nodes_data[[1]] <- nodes_data_base
pipes_data[[1]] <- pipes_data_base


### Define alternative designs  

# Design 2: introduce s10 with capacity 750m3/yr
nodes_data[[2]] <- nodes_data[[1]] %>%
  add_row(id=49,	label='s10', lat=53.15, lon=5.75, name='new1', type='supply', 
          weight=1, discharge=750, elevation=0, max_supply_head =	10000, 
          disuse=0, use_pump_curve=0, ind_ratio=0)
pipes_data[[2]] <- pipes_data[[1]] %>%
  add_row(start=49, end=20, id=53, length=3000,diameter=700,disuse=0,q_max=1384.74, 
          flowdir=0, has_booster=0, booster_hmax=0)

# Design 3: increase all other supply by 10%
nodes_data[[3]] <- nodes_data[[1]] %>%
  mutate(discharge = ifelse(type == "supply", discharge * 1.10, discharge))        
pipes_data[[3]] <- pipes_data[[1]]

# Design 4: increase all other supply by 20%
nodes_data[[4]] <- nodes_data[[1]] %>%
  mutate(discharge = ifelse(type == "supply", discharge * 1.20, discharge))             
pipes_data[[4]] <- pipes_data[[1]]

dmax <- length(nodes_data)


#:::::::::::::::::::: Experimental Design ::::::::::::::::::::::::::::::::::::::

# Trend scenarios: demand increase  
# Shock scenarios: supply site, network failure

######### SHOCK SCENARIOS

# Supply Failure combinations
supplyIDs <- nodes_data_base %>% filter(type == "supply") %>% arrange(label) %>% pull(id)
supplyF_comb1 <- as.list(combn(supplyIDs,1))
supplyF_comb2 <- combn(supplyIDs, 2)
supplyF_comb2 <- lapply(1:ncol(supplyF_comb2), function(x) supplyF_comb2[, x])
supplyF_combs <- c(supplyF_comb1,supplyF_comb2)
supplyFn <- length(supplyF_combs)

# Connection failure combinations
pipeIDs <- pipes_data_base %>% pull(id)
pipeF_comb1 <- as.list(combn(pipeIDs,1))
pipeF_comb2 <- combn(pipeIDs, 2)
pipeF_comb2 <- lapply(1:ncol(pipeF_comb2), function(x) pipeF_comb2[, x])
pipeF_combs <- c(pipeF_comb1,pipeF_comb2)
pipeFn <- length(pipeF_combs)

scn_schockn <- supplyFn + pipeFn
scn_shock_tbl <- tibble(sid=1:scn_schockn, supplyF = as.list(0), pipeF = as.list(0))
scn_shock_tbl$supplyF[1:supplyFn] <- supplyF_combs
scn_shock_tbl$pipeF[(supplyFn+1):(supplyFn+pipeFn)] <- pipeF_combs

######### TREND SCENARIOS
demandT <- seq(0.5, 0.75, 1, 1.25, 1.5)
demandTn <- length(demandT)
scn_Tn <- demandTn
scn_Ttbl <- tibble(tid=1:scn_Tn, demandT = demandT)

######### COMBINED SCENARIOS
scn_tbl <- expandGridDf(scn_shock_tbl, scn_Ttbl) %>%
  add_column(id=NA, .before = "sid") %>%
  mutate(id = 1:n()) %>%
  mutate(demand = 0, available = 0, allocated = 0, reliability = 0, linkuse = 0) 
num_scn <- nrow(scn_tbl)

#Loop through actions
dmax <-1
imax <- num_scn
scnOut <- vector(mode = "list", length = num_scn) 
pb <- txtProgressBar(min = 1, max = num_scn*dmax, style = 3)
start_time <- Sys.time()
for (d in 1:dmax) {
  
  # Read-in network data for the current configuration (d)
  nodes_d <- nodes_data[[d]]
  pipes_d <- pipes_data[[d]]
  
  # Set demand and supply node indices
  snodes <- nodes_d$id[which(nodes_d$type == "supply")]
  dnodes <- nodes_d$id[which(nodes_d$type == "demand")]
  
  # Set spare supply capacity
  nodes_d$discharge[snodes] <- nodes_d$discharge[snodes] * 1.05

  #Loop through scenarios
  for (i in 1:imax) {
    
    # progress bar
    setTxtProgressBar(pb, i)
    
    #Reset node and link data
    nodes_scn <- nodes_d
    pipes_scn <- pipes_d
    
    # set current scenario name
    scnname <- paste0("scn",scn_tbl$id[i],"_opt",d)
    
    # Set supply node failures based on current scenario
    supplyF_i <- unlist(scn_tbl$supplyF[i])
    nodes_scn$disuse[which(nodes_scn$id %in% supplyF_i)] <- 1
    
    # Set pipe failures based on current scenario
    pipeF_i   <- unlist(scn_tbl$pipeF[i])
    pipes_scn$disuse[which(pipes_scn$id %in% pipeF_i)] <- 1
    
    # Set Demand nodes based on current scenario
    nodes_scn$discharge[dnodes] <- nodes_scn$discharge[dnodes] * scn_tbl$demandT[i]

    # Run simulation model
    scnOut[[i]] <- simulateWDN(
      edev.change = 0.0005,   
      tdev.change = 0.01,
      temp.change = 0.04,       
      price.change = 0.01,        
      pop.change  = 0.01, 
      dnetwork.change = 0.01,
      wqual.change = 0.005,
      edev.elasticity = 1.0,       
      price.elasticity = -0.2,      
      temp.elasticity = 0.03,     
      dom.peak.factor = 1,
      ind.peak.factor = 1,
      year.sim = 2021,       
      year.ref = 2021,        
      nodes.data = nodes_scn,       
      pipes.data = pipes_scn
    )
    
    #Save summary results
    scn_tbl[[i,"demand"]] <- scnOut[[i]]$summary$value[1]
    scn_tbl[[i,"available"]] <- scnOut[[i]]$summary$value[2]
    scn_tbl[[i,"allocated"]] <- scnOut[[i]]$summary$value[3]
    scn_tbl[[i,"reliability"]] <- scnOut[[i]]$summary$value[4]
    scn_tbl[[i,"linkuse"]] <- scnOut[[i]]$summary$value[5]

  }
  
}
Sys.time() - start_time
close(pb)

#7,000 scenario in 10 minutes
write_rds(scnOut, paste0(outPath, format(Sys.time(), "%m%d"),"_scnOut.rds"), "xz", compression = 9L)
write_rds(scn_tbl, paste0(outPath, format(Sys.time(), "%m%d"),"_scn_tbl.rds"), "xz", compression = 9L)


#::::::::::::   Network vizualization ::::::::::::::::::::::::::::::::::::::::::

p <- visualizeWDN(
  nodes.data = scnOut[[i]]$nodes,
  pipes.data = scnOut[[i]]$pipes,
  node.fill.var = "rel",
  node.size.var = "discharge",
  edge.color.var = "usage",
  edge.size.var = "diameter",
  background.map = mapFriesland)

  #ggtitle(paste0("Scenario index: ",i, " - Design: ",d))

df <- tibble(x = 0.02, y = 0.02, tb = list(scnOut[[i]]$summary))
p <- p + geom_table_npc(data = df, aes(npcx=x, npcy=y, label = tb),
                        size = 3, table.hjust = 0,
                        table.theme = ttheme_gtstripes)


ggsave(filename = paste0(outPath, scnname,".png"), height = 8, width = 11)



#::::::::::::   Results Analysis :::::::::::::::::::::::::::::::::::::::::::::::


scnOut <- read_rds(paste0(outPath, format(Sys.time(), "%m%d"),"_scnOut.rds"))

scn_tbl <- read_rds(paste0(outPath, format(Sys.time(), "%m%d"),"_scn_tbl.rds")) %>%
  mutate(demandT = factor(demandT, levels = seq(0.5,1.5,0.25), labels = seq(0.5,1.5,0.25) * 100 - 100))


# Plot range of reliablity accross scenarios

p <- ggplot(scn_tbl, aes(x = demandT, y = reliability)) +
  theme_light() +
  geom_point(alpha = 0.3) + geom_jitter() +
  labs(x = "Demand Change (%)", y = "Reliability (%)") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,110))

p <- visualizeWDN(
  nodes.data = scnOut[[i]]$nodes,
  pipes.data = scnOut[[i]]$pipes,
  background.map = mapFriesland,
  node.fill.var = "rel",
  node.size.var = NULL,
  edge.color.var = NULL,
  edge.size.var = NULL)



# Summary Results ..............................................................

# Calculate per pipe statistics
d<-1


demandT_range <- scn_tbl %>% pull(demandT) %>% unique()

for (x in 1:length(demandT_range)) {
  
  demandT_x <-demandT_range[x]
  
  scn_selection <- scn_tbl %>% filter(demandT == demandT_x) %>% pull(id)
  
  pipesOut <- lapply(1:num_scn, function(x) scnOut[[x]]$pipes) %>%
    bind_rows(.id = "scn")  %>% 
    filter(scn %in% scn_selection)  
  
  nodesOut <- lapply(1:num_scn, function(x) scnOut[[x]]$nodes) %>%
    bind_rows(.id = "scn") %>% 
    filter(scn %in% scn_selection) %>%
    group_by(id) %>%
    summarize(meanRel = mean(rel), countRel = sum(rel==100)) %>%
    mutate(countRel = round(countRel/length(scn_selection) * 100,0))
  
  
  nodesOut_summary <- nodes_data_base %>% 
    left_join(nodesOut, by = "id")
  
  pipesOut_summary <- pipes_data_base 
  
  ### Count of adeqauate performance outcomes
  
  p1 <- visualizeWDN(
    nodes.data = nodesOut_summary,
    pipes.data = pipesOut_summary,
    background.map = mapFriesland,
    node.fill.var = "countRel",
    node.size.var = "discharge",
    edge.color.var = "flowdir",
    edge.size.var = "diameter") +
    theme(legend.position = "none") +
    scale_fill_gradient2(low = "red", mid = "white", high = "white", midpoint = 99, 
                         limits = c(0, 100),
                         breaks = c(0, 20, 40, 60, 80, 99, 100)) 
  
  p2 <- ggplot(nodesOut_summary %>% filter(type == "demand"), 
               aes(x = reorder(id, -countRel), y = countRel)) +
    theme_bw() +
    geom_bar(aes(fill = countRel), stat="identity") +
    coord_flip() +
    scale_fill_gradient2(low = "red", mid = "white", high = "white", midpoint = 99, 
                         limits = c(0, 100),
                         breaks = c(0, 20, 40, 60, 80, 99, 100)) +
    labs(y = "Robustness score", x = "Node Ids", color = "") +
    guides(fill = FALSE)
  
  p <- cowplot::plot_grid(p1, p2, rel_widths = c(3,1), align = c("hv"))
  ggsave(filename = paste0("robustness_dem",demandT_x,".png"), height = 8, width = 11)
  
  
  ### Average reliablity accross scenarios 
  
  p1 <- visualizeWDN(
    nodes.data = nodesOut_summary,
    pipes.data = pipesOut_summary,
    background.map = mapFriesland,
    node.fill.var = "meanRel",
    node.size.var = "discharge",
    edge.color.var = "flowdir",
    edge.size.var = "diameter") +
    theme(legend.position = "none") +
    scale_fill_gradient2(low = "red", mid = "white", high = "white", midpoint = 99, 
                         limits = c(0, 100),
                         breaks = c(0, 20, 40, 60, 80, 99, 100)) 
  
  
  p2 <- ggplot(nodesOut_summary %>% filter(type == "demand"), 
               aes(x = reorder(id, -meanRel), y = meanRel)) +
    theme_bw() +
    geom_bar(aes(fill = meanRel), stat="identity", color = "gray90") +
    coord_flip() +
    scale_fill_gradient2(low = "red", mid = "white", high = "white", midpoint = 99, 
                         limits = c(0, 100),
                         breaks = c(0, 20, 40, 60, 80, 99, 100)) +
    labs(y = "Mean Reliability", x = "Node Ids", color = "") +
    guides(fill = FALSE)
  
  p <- cowplot::plot_grid(p1, p2, rel_widths = c(2,1), align = c("hv"))
  ggsave(filename = paste0("meanRel_dem",demandT_x,".png"), height = 8, width = 12)





}














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












