

# # annual economic development rate [%/year]
# edev.change = 0.0005
# # technological development rate [-]
# tdev.change = 0.01
# # annual increase in temperature [DegC/year]
# temp.change = 0.04
# # annual price development rate [%/year]
# price.change = 0.01
# # annual population growth rate [%/year]
# pop.change  = 0.01
# # network coverage change factor [%]
# dnetwork.change = 0
# # water quality change factor [%]
# wqual.change = 0.005
# # Economic development elasticity of water demand [-]
# edev.elasticity = 1.0
# # Price elesticity of water demand [-]
# price.elasticity = -0.2
# # Temperature elasticity of water demand [C^-1]
# temp.elasticity = 0.03  
# # ratio of industrial demand to total demand [-]
# industry.ratio = 0.3
# # peak factor for domestic demand
# dom.peak.factor = 1
# # peak factor for industrial demand
# ind.peak.factor = 1
# # future year to be simulated [-]
# year.sim = 2031
# # reference (benchmark) year [-]
# year.ref = 2021
# # node data as table
# nodes.data = nodes_data
# # pipe data as table
# pipes.data = pipes_data


#' Title
#'
#' @param edev.change 
#' @param tdev.change 
#' @param temp.change 
#' @param price.change 
#' @param pop.change 
#' @param dnetwork.change 
#' @param wqual.change 
#' @param edev.elasticity 
#' @param price.elasticity 
#' @param temp.elasticity 
#' @param industry.ratio 
#' @param dom.peak.factor 
#' @param ind.peak.factor 
#' @param year.sim 
#' @param year.ref 
#' @param nodes.data 
#' @param pipes.data 
#'
#' @return
#' @export
#'
#' @examples
simulateWDN <- function(edev.change = 0.0005,   
                        tdev.change = 0.01,
                        temp.change = 0.04,       
                        price.change = 0.01,        
                        pop.change  = 0.01, 
                        dnetwork.change = 0.01,
                        wqual.change = 0.005,
                        edev.elasticity = 1.0,       
                        price.elasticity = -0.2,      
                        temp.elasticity = 0.03,     
                        industry.ratio = 0.3,
                        dom.peak.factor = 1,
                        ind.peak.factor = 1,
                        year.sim = NULL,       
                        year.ref = 2021,        
                        nodes.data = NULL,       
                        pipes.data = NULL       
)

{
  
  # Demand & supply node indices
  dnodes <- nodes.data$id[which(nodes.data$type == "demand")]
  snodes <- nodes.data$id[which(nodes.data$type == "supply")]
  dnodes_num <- length(dnodes)
  snodes_num <- length(snodes)
  
  # Current year index
  yind <- year.sim - year.ref
  
  #:::::::::::::: DEMAND MODULE  :::::::::::::::::::::::::::::::::::::::::::::::
  
  # population growth rate per node (in the future, provide this as input)
  pop.change_pernode <- rep(pop.change, dnodes_num) 
  
  # Demand adjustment factors
  edev_factor <- (1 + edev.change) ^ yind  #economic development factor for future year
  temp_factor <-  1 + temp.change * temp.elasticity * yind  # temperature factor for future year
  tdev_factor <- (1 + tdev.change) ^ yind #technological development factor for future year (for industrial demand)
  price_factor <- (1 + price.change * price.elasticity) ^ yind # price price factor for future year
  pop_factor <- (1 + pop.change_pernode) ^ yind #population factor for future year (per node)
  reuse_factor <- ifelse(yind < 23, 1, 0.90) # water reuse factor for future year
  dnetwork_factor <- (1 + dnetwork.change) ^ yind  # change in network demand percentage for future year
  
   
  # Redidential demand adjustment per node
  res_demand_adj <- temp_factor * price_factor * edev_factor * reuse_factor * pop_factor * dom.peak.factor
  
  # Industrial demand adjustment per node
  ind_demand_adj <- edev_factor * tdev_factor * temp_factor * dnetwork_factor * ind.peak.factor 
  
  # Set demand per node 
  demand_per_node <- nodes.data$discharge[dnodes] 
  demand_per_node_res <- demand_per_node * (1-industry.ratio) * res_demand_adj
  demand_per_node_ind <- demand_per_node * (industry.ratio) * ind_demand_adj
  
  # Total demand per node
  node_demand_update <- round(demand_per_node_res + demand_per_node_ind,2)
  nodes.data$discharge[dnodes] <- node_demand_update
  
  #:::::::::::::: SUPPLY MODULE  :::::::::::::::::::::::::::::::::::::::::::::::
  
  quality_factor <- (1 +  wqual.change) ^ yind
  node_supply_update = nodes.data$discharge[snodes]/quality_factor
  nodes.data$discharge[snodes] <- node_supply_update

  #::::::::::::: NETWORK MODULE ::::::::::::::::::::::::::::::::::::::::::::::::
  
  # simulate network
  res <- calculate_network(nodes.data, pipes.data, 1.0, 1)
 
  return(
    list(nodes = nodes.data %>% mutate(sim = as.numeric(res[[2]])),
         pipes = pipes.data %>% mutate(sim = as.numeric(res[[3]]))
    )
  )
  
}

