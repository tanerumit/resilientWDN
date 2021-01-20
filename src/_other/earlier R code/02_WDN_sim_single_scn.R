
################################################################################
# Drinking Water System Resilience Analysis
# Single scenario analysis
# 
# Date: 28/1/2020
# By: Deltares (Umit Taner)
################################################################################


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

# Loop through each time-step
pb <- txtProgressBar(min = 1, max = sim_len, style = 3)
for (i in 1:sim_len) {
  
  setTxtProgressBar(pb, i)
  
  # Update node target node discharges at month i
  node_df$discharge <- iniNodes_scn[i,]
  
  # Calculate discharges at month i 
  out <- calculate_network(node_df, pipe_df, 1.0, 1)
  
  # save results to output tables 
  outNodes_scn[i,] <- as.numeric(out[[2]])
  #outPipes_scn[i,] <- as.numeric(out[[3]])
  
}
close(pb)

# Demand and deliveries to all demand nodes
totalD <- rowSums(iniNodes_scn[,dindex])
totalS <- rowSums(outNodes_scn[,dindex])

# WR Metrics per demand node
wrmetrics_pernode <- tibble(node_id = node_df$node_id[dindex])
wrmetrics_pernode$REL <- sapply(dindex, function(x) mReliability(iniNodes_scn[,x], outNodes_scn[,x]))
wrmetrics_pernode$VUL <- sapply(dindex, function(x) mVulnerability(iniNodes_scn[,x], outNodes_scn[,x]))
wrmetrics_pernode$RES <- sapply(dindex, function(x) mResilience(iniNodes_scn[,x], outNodes_scn[,x]))

# WR Metrics aggregated
wrmetrics_agg <- tibble(scenario = "isolated", REL = NA, VUL = NA, RES = NA) 
wrmetrics_agg$REL <- mReliability(totalD, totalS)
wrmetrics_agg$VUL <- mVulnerability(totalD, totalS)
wrmetrics_agg$RES <- mResilience(totalD, totalS)


