

######################### MODEL DATA ###########################################

source("./src/global.R")

# Saving directories
inPath  <- "./data/v8_2021/"
outPath <- "./results/v8_2021/"

# Read_in node and pipe schematic from csv files: 
nodes_ref  <- read.csv(paste0(inPath,"nodes.csv"))
pipes_ref  <- read.csv(paste0(inPath,"pipes_d1.csv"))


######################### MODEL PARAMETERS #####################################

#:::::::::::::::::::::: General parameters

year_ref <- 2021         # Reference year for data [-]

#:::::::::::::::::::::: Demand module parameters

dev_rate <- 0.0005       # economic_development [%/year]
tmp_rate <- 0.04         # annual increase in temperature [C/year]
indf <- 0.3              # ratio of industrial demand [-]

prc_rate  <- 0.01        # price development rate [%/year]
dev_elast <- 1.0         # Eco. development elasticity of demand [-]
prc_elast <- -0.2        # Price elesticity of demand [-]
tmp_elast <- 0.03        # Temperature elasticity of demand [C^-1]
pop_rate  <- 0.01        # population growth rate

#:::::::::::::::::::::: Supply module parameters

prb_supFail <- 0.05      # Supply site failure likelihood in any given year


#:::::::::::::::::::::: Network module parameters

prb_pFail <- 0.05        # Pipe failure likelihood in any given year



simulateWDN <- function(dev_rate = 0.0005,       
                        tmp_rate = 0.04,       
                        prc_rate  = 0.01,       
                        pop_rate  = 0.01,
                        nodes_ref = nodes_ref,
                        pipes_ref = pipes_ref,
                        sim_year = 2030)
{
  
  # Prepare table for initial datasets
  nodes_sim <- nodes_ref
  pipes_sim <- pipes_ref
  
  
  # Indices of demand nodes
  dnodes <- nodes_ref$node_id[which(nodes_ref$type == "demand")]
  dnodes_num <- length(dnodes)
  
  # Indices of supply nodes
  snodes <- nodes_ref$node_id[which(nodes_ref$type == "supply")]
  snodes_num <- length(snodes)
  
  # Current year index
  year_index <- year_sim - year_ref
  
  # Demand factors
  edev_factor  <- (1 + dev_rate) ^ year_index 
  temp_factor  <- 1 + year_index * tmp_rate * tmp_elast
  price_factor <- (1 + prc_rate * prc_elast) ^ year_index
  reuse_factor <- ifelse(year_index < 23, 1, 0.90)
  
  # Demand adjustment factors
  pop_rate_pernode <- rep(pop_rate, dnodes_num) 
  pop_factor <- (1 + pop_rate_pernode) ^ year_index 
  res_demand_adj <- temp_factor * price_factor * edev_factor * reuse_factor * pop_factor
  ind_demand_adj <- res_demand_adj  
  
  # Set demand per node 
  demand_per_node <- nodes_ref$discharge[dnodes] 
  demand_per_node_res <- demand_per_node * (1-indf) * res_demand_adj
  demand_per_node_ind <- demand_per_node * (indf) * ind_demand_adj
  node_demand_total <- round(demand_per_node_res + demand_per_node_ind,2)
  nodes_sim$discharge[dnodes] <- node_demand_total
  
  #### Water Distribution Network Simulation
  result <- calculate_network(nodes_sim, pipes_sim, 1.0, 1)
  
  return(list(demand = nodes_sim$discharge[dnodes], 
              delivery = as.numeric(result[[2]][dnodes])))
  
}




################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################






######################### MODULE CALCULATIONS ##################################




#:::::::::::::::::::::: Demand module calculations





#:::::::::::::::::::::: Supply module calculations




#:::::::::::::::::::::: Network module calculations

# Supply failures
node_df$disuse[snodes] <- par_supFail[, scn_mat$sample[i]]      

# pipe failures
pipe_df$disuse <- par_pFail[1:nrow(pipe_df), scn_mat$sample[i]]  






#### WR module parameters ######################################################

# Supply site failure likelihoood

par_supFail <- rbinom(supplyNum, size = 1, prob = prb_supFail)
par_pFail <- rbinom(pipeNum, size = 1, prob = prb_pFail)

# Supply availability change (climate change + permits)
par_supChange <- seq(0.70, 1.30, length.out = 5)
supplyVals <- sapply(par_supChange, function(x) nodes_ref$discharge[snodes] * x) 






#### Demand module parameters ##################################################









# Set demand/supply trend parameters
node_df$discharge[snodes] <- supplyVals[, scn_mat$supChange[i]]   # Set supply change
node_df$discharge[dnodes] <- demandVals[, scn_mat$demChange[i]]   # Set demand increase



#### Demand and delivered water PER node
outDemand[,i]    <- node_df$discharge 
outDelivered[,i] <- as.numeric(out[[i]][[2]])


# Reduce suppy to 80% of demand
node_df[snodes,"discharge"] <- node_df[snodes,"discharge"] * 0.8




