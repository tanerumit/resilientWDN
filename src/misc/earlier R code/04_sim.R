
################################################################################
# Drinking Water System Resilience Analysis
# Main script 
# 
# Date: 28/1/2020
# By: Deltares (Umit Taner)
################################################################################

# Source data & phython scripts
library(reticulate)
library(R.utils); 
library(dplyr); library(tidyr)
library(ggplot2);library(lubridate); 
library(hydrosystems)

source("./R/functions.R")
source("./R/visualizeWDN.R")


# Specify the virtual python environment
use_miniconda(condaenv = "myEnv", required = TRUE)

# Source python script 
source_python("./Python/wdn_module_v5.py")


############### Network parameters

# Read_in node and pipe schematic: 
nodes <- read.csv("./data/vitens_nodes_dummy.csv")  
pipes_com <- read.csv("./data/vitens_pipes_dummy_combined.csv")
pipes_iso <- read.csv("./data/vitens_pipes_dummy_isolate.csv")
load("./data/mapFriesland.rda")

p2 <- visualizeWDN(nodes, pipes_iso, mapFriesland)  + ggtitle("Isolated scheme")
ggsave(filename = "./graphics/wdn_isolated.png", width = 8, height = 8)
p3 <- visualizeWDN(nodes, pipes_com, mapFriesland) + ggtitle("Integrated scheme")
ggsave(filename = "./graphics/wdn_combined.png", width = 8, height = 8)

# Choose schematic
node_df <- nodes
pipe_df <- pipes_com

# number of demand and supply nodes
node_num <- nrow(node_df)
pipe_num <- nrow(pipe_df)

# index numbers of demand and supply nodes
dem_index <- node_df$node_id[which(node_df$type == "demand")]
sup_index <- node_df$node_id[which(node_df$type == "supply")]


############### Simulation parameters

# Time-period for simulation
date_beg <- as.Date("2020/1/1")   
date_end <- as.Date("2024/12/30") #as.Date("2049/12/30")
date_vec <- seq.Date(date_beg, date_end, by = "month")
date_mat <- tibble(date = date_vec) %>% mutate(year = year(date), month = month(date))

# Total number of time-steps and years for the simulation period                              
sim_len <- length(date_vec)
sim_yrs <- length(date_vec)/12


################################################################################
# SINGLE SCENARIO/network performance ANALYSIS ---------------------------------

pipe_df <- pipes_iso

# Node discharge data for the entire simulation period
inNodes_data <- readr::read_csv("./data/vitens_discharge_dummy.csv")[,-1] %>% as.matrix()
inNodes <- do.call("rbind", replicate(sim_yrs, inNodes_data, simplify = FALSE))

# Simulated discharges from/to nodes (supply/demand)
outNodes <- matrix(data = 0, nrow = sim_len, ncol = node_num)
outPipes <- matrix(data = 0, nrow = sim_len, ncol = pipe_num)

#Current discharge scenario
inNodes_scn <- inNodes

# Loop through each time-step
pb <- txtProgressBar(min = 1, max = sim_len, style = 3)
for (i in 1:sim_len) {
  
  setTxtProgressBar(pb, i)
  
  # Update node target node discharges at month i
  node_df$discharge <- inNodes_scn[i,]
  
  # Calculate discharges at month i 
  out <- calculate_network(node_df, pipe_df, 1.0, 1)
  
  # save results to output tables 
  outNodes[i,] <- as.numeric(out[[2]])
  #outPipes[i,] <- as.numeric(out[[3]])
  
}
close(pb)

metrics_all <- tibble(scenario = "isolated", REL = NA, VUL = NA, RES = NA) 
metrics_all$REL <- mReliability(rowSums(inNodes_scn[,dem_index]), rowSums(outNodes[,dem_index]))
metrics_all$VUL <- mVulnerability(rowSums(inNodes_scn[,dem_index]), rowSums(outNodes[,dem_index]))
metrics_all$RES <- mResilience(rowSums(inNodes_scn[,dem_index]), rowSums(outNodes[,dem_index]))

m_isolated <- metrics_all





pipe_df <- pipes_com

# Node discharge data for the entire simulation period
inNodes_data <- readr::read_csv("./data/vitens_discharge_dummy.csv")[,-1] %>% as.matrix()
inNodes <- do.call("rbind", replicate(sim_yrs, inNodes_data, simplify = FALSE))

# Simulated discharges from/to nodes (supply/demand)
outNodes <- matrix(data = 0, nrow = sim_len, ncol = node_num)
outPipes <- matrix(data = 0, nrow = sim_len, ncol = pipe_num)

#Current discharge scenario
inNodes_scn <- inNodes

# Loop through each time-step
pb <- txtProgressBar(min = 1, max = sim_len, style = 3)
for (i in 1:sim_len) {
  
  setTxtProgressBar(pb, i)
  
  # Update node target node discharges at month i
  node_df$discharge <- inNodes_scn[i,]
  
  # Calculate discharges at month i 
  out <- calculate_network(node_df, pipe_df, 1.0, 1)
  
  # save results to output tables 
  outNodes[i,] <- as.numeric(out[[2]])
  #outPipes[i,] <- as.numeric(out[[3]])
  
}
close(pb)

metrics_all <- tibble(scenario = "combined", REL = NA, VUL = NA, RES = NA) 
metrics_all$REL <- mReliability(rowSums(inNodes_scn[,dem_index]), rowSums(outNodes[,dem_index]))
metrics_all$VUL <- mVulnerability(rowSums(inNodes_scn[,dem_index]), rowSums(outNodes[,dem_index]))
metrics_all$RES <- mResilience(rowSums(inNodes_scn[,dem_index]), rowSums(outNodes[,dem_index]))

m_combined <- metrics_all


bind_rows(m_isolated, m_combined)


# metrics_prn <- tibble(REL = rep(NA,length(dem_index)),
#                       VUL = rep(NA,length(dem_index)), 
#                       RES = rep(NA,length(dem_index))) 
# 
# 
# metrics_prn$REL <- round(sapply(dem_index, function(x) mReliability(inNodes_scn[,x], outNodes[,x])),2)
# metrics_prn$VUL <- round(sapply(dem_index, function(x) mVulnerability(inNodes_scn[,x], outNodes[,x])),2)
# metrics_prn$RES <- round(sapply(dem_index, function(x) mResilience(inNodes_scn[,x], outNodes[,x])),2)
# 
# 
# 


################################################################################



# MULTI SCENARIO ANALYSIS -----------------------------------------------------

############### Construct scenario matrix

#scenario_variables
supply_m <- c(1,1.2,1.5)
demand_m <- c(1,1.2,1.5)

# scenario matrix 
scn_num <- length(supply_m) * length(demand_m)
scn_mat <- expandGrid(supply_m = supply_m, demand_m = demand_m) %>%
  mutate(id = 1:n()) %>% dplyr::select(id, supply_m, demand_m)

scn_list <- vector("list", scn_num)

# Prepare scenario series
for (k in 1:scn_num) {
  
  supply_m_cur <- scn_mat$supply_m[k]
  demand_m_cur <- scn_mat$demand_m[k]
  
  #Annual sequence of changes
  supply_m_seq <- seq(supply_m_cur, supply_m_cur,  length = sim_len)
  demand_m_seq <- seq(demand_m_cur, demand_m_cur , length = sim_len)
  
  #Apply changes to climate data
  scn_cur <- inNodes
  scn_cur[,dem_index] <- sapply(dem_index, function(x) inNodes[,x] * demand_m_cur)
  scn_cur[,sup_index] <- sapply(sup_index, function(x) inNodes[,x] * supply_m_cur)

  scn_list[[k]] <- scn_cur
}

############### Simulate WDN
start_time <- Sys.time()
pb <- txtProgressBar(min = 1, max = scn_num, style = 3)


Results <- scn_mat %>% mutate(REL = NA, VUL = NA, RES = NA)

# Loop through each scenario
for (k in 1:scn_num) {
  
  setTxtProgressBar(pb, k)
  
  node_discharge_scn <- scn_list[[k]]
  
  # Loop through each time-step
  for (i in 1:sim_len) {
    
    # Update all demand/supply nodes at month i
    node_df$discharge <- node_discharge_scn[i,]
    
    # Calculate network 
    out <- calculate_network(node_df, pipe_df, 1.0, 1)
    
    # save results to node/pipe matrices
    outNodes[i,] <- as.numeric(out[[2]])
    outPipes[i,] <- as.numeric(out[[3]])
    
  }
  
  # Calculate scenario-specific metrics
  totalD <- rowSums(node_discharge_scn[,dem_index])
  totalS <- rowSums(outNodes[,dem_index])
  
  Results$REL[k] <- mReliability(totalD, totalS)
  Results$VUL[k] <- mVulnerability(totalD, totalS)
  Results$RES[k] <- mResilience(totalD, totalS)
  
}
close(pb)
end_time <- Sys.time()
end_time - start_time

Results %>% filter(supply_m == 1)
