
################################################################################
# Drinking Water System Resilience Analysis
# Main script 
# 
# Date: 28/1/2020
# By: Deltares (Umit Taner)
################################################################################

# Source data & phython scripts
library(reticulate)
library(readr); library(R.utils); library(dplyr); library(tidyr)
library(ggplot2);library(lubridate); library(hydrosystems)


# Specify the virtual python environment
use_miniconda(condaenv = "myEnv", required = TRUE)

# Source python script 
source_python("./Python/wdn_module_v5.py")

# Read_in node and pipe information: 
node_data <- read.csv("./data/vitens_nodes_dummy_v5.csv")
pipe_data <- read.csv("./data/vitens_pipes_dummy_v5.csv")

# Network parameters
node_df <- node_data
pipe_df <- pipe_data
node_num <- nrow(node_df) 
pipe_num <- nrow(pipe_df)
node_demand_ind <- node_df$node_id[which(node_df$type == "demand")]
node_supply_ind <- node_df$node_id[which(node_df$type == "supply")]


dem <- c(100, 100, 100, 100, 100, 100, 100, 100)
sup <- c(100, 100, 100, 100, 100, 100, 100, 100)

# Calculate reliability/vulnerability/resilience
calcRel1  <- function(x, y) {100 - 100 * (sum(x) - sum(y))/sum(x)}
calcRel2  <- function(x, y) {100 * length(which(y-x == 0))/length(x)}
calcVul1  <- function(x, y) {z <- x-y; mean(z[z!=0])}
calcVul2  <- function(x, y) {z <- x-y; if(length(which(z > 0)) > 0) max(z[z!=0]) else 0}
calcRes   <- function(x, y) {
  
  z <- x-y; 
  
  if(length(which(z > 0)) > 0) {
    
    rle_z <- rle(z>0)
    a <- length(which(z>0))
    b <- max(rle_z$length[which(rle_z$value == TRUE)]) 
    c <- b/a
  } else {
    c <- 1
  }
  
  return(c)
}

calcRel1(dem, sup)
calcRel2(dem, sup)
calcVul1(dem, sup) 
calcVul2(dem, sup) 
calcRes(dem, sup) 

################################################################################
# SINGLE SCENARIO ANALYSIS -----------------------------------------------------

# Simulation period
date_beg <- as.Date("2020/1/1")   
date_end <- as.Date("2049/12/30")
date_vec <- seq.Date(date_beg, date_end, by = "month")
date_mat <- tibble(date = date_vec) %>% mutate(year = year(date), month = month(date))

# Simulation parameters                                   
sim_len <- length(date_vec)
sim_yrs <- length(date_vec)/12

# Read-in demand/supply node data for a single year
node_data_yr <- read_csv("./data/vitens_nodes_data.csv")[,-1] %>% as.matrix()
node_data <- do.call("rbind", replicate(sim_yrs, node_data_yr, simplify = FALSE))

# Demand nodes
node_data[,node_demand_ind] <- node_data[,node_demand_ind]*0.5

# Actual Discharges (always positive: for supply: outflow / For demand: inflow)
outNodes <- matrix(data = 0, nrow = sim_len, ncol = node_num)
# Actual Dischages from each pipe (always positive)
outPipes <- matrix(data = 0, nrow = sim_len, ncol = pipe_num)

# Loop through each time-step
pb <- txtProgressBar(min = 1, max = sim_len, style = 3)
for (i in 1:sim_len) {
  
  setTxtProgressBar(pb, i)
  
  # Update all demand/supply nodes at month i
  node_df$discharge <- node_data_period[i,]
  
  # Calculate network 
  out <- calculate_network(node_df, pipe_df, 1.0, 1)
  
  # save results to node/pipe matrices
  outNodes[i,] <- as.numeric(out[[2]])
  outPipes[i,] <- as.numeric(out[[3]])
  
}
close(pb)

outREL1 <- sapply(node_demand_ind, function(x) calcRel1(node_data[,x], outNodes[,x]))
outREL2 <- sapply(node_demand_ind, function(x) calcRel2(node_data[,x], outNodes[,x]))
outVUL1 <- sapply(node_demand_ind, function(x) calcVul1(node_data[,x], outNodes[,x]))
outVUL2 <- sapply(node_demand_ind, function(x) calcVul2(node_data[,x], outNodes[,x]))
outRES  <- sapply(node_demand_ind, function(x) calcRes(node_data[,x], outNodes[,x]))

calcRel1(node_data[,2], outNodes[,2])


node_data[,1] - outNodes[,1]

sum(node_data[,2])
sum(outNodes[,2])

calcRel1(dem, sup)


calcRel2(dem, sup)
calcVul1(dem, sup) 
calcVul2(dem, sup) 
calcRes(dem, sup) 


#scenario_variables
supply_m <- c(1, 0.9, 0.8, 0.7, 0.6, 0.5)
demand_m <- c(1, 1.1, 1.2, 1.3, 1.5)

# scenario matrix 
scn_num <- length(supplyM) * length(demandM)
scn_mat <- expandGrid(supply_m = supply_m, demand_m = demand_m) %>%
  mutate(id = 1:n()) %>% dplyr::select(id, supply_m, demand_m)

node_input_data <- matrix(data = 0, nrow = sim_len, ncol = node_num)
scenario_series <- vector("list", length = scn_num)

node_base_1yr  <- read.csv("./data/vitens_baseline_demand.csv")[,-1]
node_base  <- do.call("rbind", replicate(30, node_base_1yr, simplify = FALSE))
node_base  <- bind_cols(date_mat, node_base)
  
demand_base <- node_base %>% select(date, year, N1:N12)  %>% 
  gather(key = node_id, value = discharge, N1:N12) %>%
  mutate(node_id = stri_sub(node_id,2) %>% as.numeric()) %>%
  mutate(year = year - min(year) + 1)

supply_base <- node_base %>% select(date, year, N13:N16) %>% 
  gather(key = node_id, value = discharge, N13:N16) %>%
  mutate(node_id = stri_sub(node_id,2) %>% as.numeric()) %>%
  mutate(year = year - min(year) + 1)

# Prepare scenario series
pb <- txtProgressBar(min = 1, max = scn_num, style = 3)
for (k in 1:scn_num) {
  
  setTxtProgressBar(pb, k)
  
  supply_m_cur <- scn_mat$supply_m[k]
  demand_m_cur <- scn_mat$supply_m[k]
  
  #Annual sequence of changes
  supply_m_seq <- seq(supply_m_cur, supply_m_cur,  length = sim_yrs)
  demand_m_seq <- seq(demand_m_cur, demand_m_cur , length = sim_yrs)
  
  #Apply changes to climate data
  demandX <- demand_base %>%  mutate(discharge = discharge * demand_m_seq[year]) %>% select(-year)
  supplyX <- supply_base %>%  mutate(discharge = discharge * supply_m_seq[year]) %>% select(-year)  
  
  scenario_series[[k]] <- bind_rows(demandX,supplyX) %>% spread(node_id, discharge)
}
close(pb)





# MultiScenario Analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_time <- Sys.time()
pb <- txtProgressBar(min = 1, max = scn_num, style = 3)

# Loop through each scenario
for (k in 1:scn_num) {
  
  # Loop through each time-step
  for (i in 1:sim_len) {
    
    # Update all demand/supply nodes at month i
    node_df$discharge <- as.numeric(scenario_series[[k]][i,-1])
    
    # Calculate network 
    out <- calculate_network(node_df, pipe_df, 1.0, 3)
    
    # save results to node/pipe matrices
    outNodes[i,] <- as.numeric(output[[2]])
    outPipes[i,] <- as.numeric(output[[3]])

  }
  
  # Calculate scenario-specific metrics
  
  # Reliability per node (total supply delivered/total demand)
  outNodes 
  
  
  
  
   
  
  setTxtProgressBar(pb, k)
}
close(pb)
end_time <- Sys.time()
end_time - start_time

