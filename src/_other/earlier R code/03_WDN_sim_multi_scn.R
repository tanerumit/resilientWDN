


source("./R/01_initialize.R")

# Define node/pipe schematic
node_df <- nodes_friesland16_dummy
pipe_df <- pipes_friesland16_dummy_iso

# number of demand and supply nodes
node_num <- nrow(node_df)
pipe_num <- nrow(pipe_df)

# index numbers of demand and supply nodes in the nodes master table
dindex <- node_df$node_id[which(node_df$type == "demand")]
sindex <- node_df$node_id[which(node_df$type == "supply")]

# Node discharge data for the entire simulation period
inNodes <- do.call("rbind", replicate(sim_yrs, inNodes_data, simplify = FALSE))

# Simulated discharges from/to nodes (supply/demand)
outNodes_scn <- matrix(data = 0, nrow = sim_len, ncol = node_num)
outPipes_scn <- matrix(data = 0, nrow = sim_len, ncol = pipe_num)

#Current discharge scenario
iniNodes_scn <- inNodes


############### Construct scenario matrix

#scenario_variables
supply_m <- c(1,1.2,1.5)
demand_m <- c(1,1.2,1.5)



# scenario matrix 
scn_num <- length(supply_m) * length(demand_m)
scn_mat <- expandGrid(supply_m = supply_m, demand_m = demand_m) %>%
  mutate(id = 1:n()) %>% dplyr::select(id, supply_m, demand_m)

# Simulated discharges from/to nodes (supply/demand)
outNodes_scn <- matrix(data = 0, nrow = sim_len, ncol = node_num)
outPipes_scn <- matrix(data = 0, nrow = sim_len, ncol = pipe_num)
iniNodes_scn_list <- vector("list", scn_num)

# Prepare scenario series
for (k in 1:scn_num) {
  
  supply_m_cur <- scn_mat$supply_m[k]
  demand_m_cur <- scn_mat$demand_m[k]
  
  #Annual sequence of changes
  supply_m_seq <- seq(supply_m_cur, supply_m_cur,  length = sim_len)
  demand_m_seq <- seq(demand_m_cur, demand_m_cur , length = sim_len)
  
  #Apply changes to climate data
  scn_cur <- inNodes
  scn_cur[,dindex] <- sapply(dindex, function(x) inNodes[,x] * demand_m_cur)
  scn_cur[,sindex] <- sapply(sindex, function(x) inNodes[,x] * supply_m_cur)
  iniNodes_scn_list[[k]] <- scn_cur
  
}

############### Simulate WDN
start_time <- Sys.time()
pb <- txtProgressBar(min = 1, max = scn_num, style = 3)

wrmetrics_agg <- scn_mat %>% mutate(REL = NA, VUL = NA, RES = NA)

# Loop through each scenario
for (k in 1:scn_num) {
  
  setTxtProgressBar(pb, k)
  
  # Current discharge data
  node_discharge_scn <- iniNodes_scn_list[[k]]
  
  # Loop through each time-step
  for (i in 1:sim_len) {
    
    # Update all demand/supply nodes at month i
    node_df$discharge <- node_discharge_scn[i,]
    
    # Calculate network 
    out <- calculate_network(node_df, pipe_df, 1.0, 1)
    
    # save results to node/pipe matrices
    outNodes_scn[i,] <- as.numeric(out[[2]])
    outPipes_scn[i,] <- as.numeric(out[[3]])
    
  }
  
  # Calculate scenario-specific metrics
  totalD <- rowSums(node_discharge_scn[,dindex])
  totalS <- rowSums(outNodes_scn[,dindex])
  
  # Combined results for all nodes
  wrmetrics_agg$REL[k] <- mReliability(totalD, totalS)
  wrmetrics_agg$VUL[k] <- mVulnerability(totalD, totalS)
  wrmetrics_agg$RES[k] <- mResilience(totalD, totalS)
  
}
close(pb)
end_time <- Sys.time()
end_time - start_time
