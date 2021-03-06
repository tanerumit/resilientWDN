
# 
# edev.change = 0.0005   
# tdev.change = 0.01
# temp.change = 0.04       
# price.change = 0.01        
# pop.change  = 0.01 
# dnetwork.change = 0.01
# wqual.change = 0.005
# edev.elasticity = 1.0       
# price.elasticity = -0.2      
# temp.elasticity = 0.03     
# dom.peak.factor = 1
# ind.peak.factor = 1
# year.sim = 2021       
# year.ref = 2021        
# nodes.data = nodes_data[[d]]       
# pipes.data = pipes_data[[d]]
# global.output = TRUE

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
#' @param dom.peak.factor 
#' @param ind.peak.factor 
#' @param year.sim 
#' @param year.ref 
#' @param nodes.data 
#' @param pipes.data 
#' @param globa.output
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
                        dom.peak.factor = 1,
                        ind.peak.factor = 1,
                        year.sim = NULL,       
                        year.ref = 2021,        
                        nodes.data = NULL,       
                        pipes.data = NULL,
                        global.output = TRUE
)

{
  
  # Demand & supply node indices
  dnodes_index <- which(nodes.data$type == "demand")
  snodes_index <- which(nodes.data$type == "supply")  
  
  dnodes_num <- length(dnodes)
  snodes_num <- length(snodes)
  
  # Current year index
  yind <- year.sim - year.ref
  
  #:::::::::::::: DEMAND MODULE  :::::::::::::::::::::::::::::::::::::::::::::::
  
  # population growth rate per node (in the future, provide this as input)
  pop.change_pernode <- rep(pop.change, dnodes_num) 
  
  # Demand adjustment factors
  edev_factor     <- (1 + edev.change) ^ yind  #economic development factor for future year
  temp_factor     <-  1 + temp.change * temp.elasticity * yind  # temperature factor for future year
  tdev_factor     <- (1 + tdev.change) ^ yind #technological development factor for future year (for industrial demand)
  price_factor    <- (1 + price.change * price.elasticity) ^ yind # price price factor for future year
  pop_factor      <- (1 + pop.change_pernode) ^ yind #population factor for future year (per node)
  reuse_factor    <- ifelse(yind < 23, 1, 0.90) # water reuse factor for future year
  dnetwork_factor <- (1 + dnetwork.change) ^ yind  # change in network demand percentage for future year
  
  # Redidential demand adjustment per node
  res_demand_adj <- temp_factor * price_factor * edev_factor * reuse_factor * pop_factor * dom.peak.factor
  
  # Industrial demand adjustment per node
  ind_demand_adj <- edev_factor * tdev_factor * temp_factor * dnetwork_factor * ind.peak.factor 
  
  # Set demand per node 
  demand_per_node_res <- nodes.data$discharge[dnodes_index] * (1-nodes.data$ind_ratio[dnodes_index]) * res_demand_adj
  demand_per_node_ind <- nodes.data$discharge[dnodes_index] * nodes.data$ind_ratio[dnodes_index] * ind_demand_adj
  
  # Total demand per node
  nodes.data$discharge[dnodes_index]  <- round(demand_per_node_res + demand_per_node_ind,2)
   
  
  #:::::::::::::: SUPPLY MODULE  :::::::::::::::::::::::::::::::::::::::::::::::
  quality_factor <- (1 +  wqual.change) ^ yind
  nodes.data$discharge[snodes_index] = nodes.data$discharge[snodes_index]/quality_factor


  #::::::::::::: NETWORK MODULE ::::::::::::::::::::::::::::::::::::::::::::::::
  
  # simulate network
  res <- calculate_network(nodes.data, pipes.data, 1.0, 1)
  
  nodes_res = nodes.data %>%  as_tibble() %>%
    mutate(discharge = round(discharge, 2),
           sim = abs(round(as.numeric(res[[2]]), 2)),
           rel = round(100 * sim/discharge,0))
  
  pipes_res = pipes.data %>% as_tibble() %>%
    mutate(q_max = round(q_max, 2),
           sim = abs(round(as.numeric(res[[3]]), 2)),
           usage = round(100 * sim/q_max,0))
  
  
  if(global.output == TRUE) {
    
    # global_results
    result <- tibble(Parameter = c("T. Demand [m3]", "Available Sup. [m3]", 
            "Allocated Sup. [m3]", "Reliability [%]", "Link usage [%]"), 
                     value = NA)

    result$value[1] <- (nodes_res %>% filter(type == "demand") %>% 
        filter(disuse == 0) %>% pull(discharge) %>% sum()) 
    
    result$value[2] <- (nodes_res %>% filter(type == "supply") %>% 
        filter(disuse == 0) %>% pull(discharge) %>% sum())
        
    result$value[3] <- (nodes_res %>% filter(type == "supply") %>% 
        filter(disuse == 0) %>% pull(sim) %>% sum())
    
    result$value[4] <- (nodes_res %>% filter(type == "demand") %>% 
        filter(disuse == 0) %>% 
        summarize(val = sum(sim)/sum(discharge) * 100) %>% pull(val)) 
    
    result$value[5] <- (pipes_res %>% filter(disuse == 0) %>% 
        summarize(val = sum(sim)/sum(q_max) * 100) %>% pull(val)) 
    
    result$value <- as.numeric(round(result$value),2)
    
  } else {
    result <- NA  
  }
  
  return(
    list(nodes = nodes_res, pipes = pipes_res, summary = result)
  )
  
}

