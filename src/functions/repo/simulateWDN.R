

#' Simulation of Combined modules
#'
#' @param dev.rate economic_development [%/year]
#' @param tmp.rate annual increase in temperature [C/year] 
#' @param prc.rate price development rate [%/year] 
#' @param pop.rate population growth rate
#' @param dev.elast Eco. development elasticity of demand [-]
#' @param prc.elast Price elesticity of demand [-]
#' @param tmp.elast Temperature elasticity of demand [C^-1]
#' @param ind.frac ratio of industrial demand [-]
#' @param year.sim simulation year [-]
#' @param year.ref reference year [-] 
#' @param nodes.ref node data as table
#' @param pipes.ref pipe data as table
#'
#' @return list object with demand and delivered water
#' @export
#'
#' @examples


simulateWDN <- function(dev.rate = 0.0005,         
                        tmp.rate = 0.04,       
                        prc.rate  = 0.01,        
                        pop.rate  = 0.01,      
                        dev.elast = 1.0,       
                        prc.elast = -0.2,      
                        tmp.elast = 0.03,     
                        ind.frac = 0.3,
                        wqd.rate = 0.005,
                        year.sim = NULL,       
                        year.ref = 2021,        
                        nodes.ref = NULL,       
                        pipes.ref = NULL       
)

{
  
  # 
  # dev.rate = 0.0005   # economic_development [%/year]
  # tmp.rate = 0.04     # annual increase in temperature [C/year]
  # prc.rate  = 0.01    # price development rate [%/year]
  # pop.rate  = 0.01    # population growth rate
  # dev.elast = 1.0     # Eco. development elasticity of demand [-]
  # prc.elast = -0.2    # Price elesticity of demand [-]
  # tmp.elast = 0.03    # Temperature elasticity of demand [C^-1]
  # ind.frac = 0.3          # ratio of industrial demand [-]
  # year.sim = 2050     # simulation year [-]
  # year.ref = 2021     # reference year [-]
  # nodes.ref = nodes_data    # node data as table
  # pipes.ref = pipes_data     # pipe data as table
  # 
  
  # Prepare table for initial datasets
  nodes_sim <- nodes.ref
  pipes_sim <- pipes.ref
  
  
  # Indices of demand nodes
  dnodes <- nodes.ref$id[which(nodes.ref$type == "demand")]
  dnodes_num <- length(dnodes)
  
  # Indices of supply nodes
  snodes <- nodes.ref$id[which(nodes.ref$type == "supply")]
  snodes_num <- length(snodes)
  
  # Current year index
  yind <- year.sim - year.ref
  
  #:::::::::::::: DEMAND MODULE  :::::::::::::::::::::::::::::::::::::::::::::::
  
  # Demand adjustment factors
  edev_factor  <- (1 + dev.rate) ^ yind 
  temp_factor  <- 1 + yind * tmp.rate * tmp.elast
  price_factor <- (1 + prc.rate * prc.elast) ^ yind
  reuse_factor <- ifelse(yind < 23, 1, 0.90)
  pop.rate_pernode <- rep(pop.rate, dnodes_num) 
  pop_factor <- (1 + pop.rate_pernode) ^ yind 
  res_demand_adj <- temp_factor * price_factor * edev_factor * reuse_factor * pop_factor
  ind_demand_adj <- res_demand_adj  
  
  # Set demand per node 
  demand_per_node <- nodes.ref$discharge[dnodes] 
  demand_per_node_res <- demand_per_node * (1-ind.frac) * res_demand_adj
  demand_per_node_ind <- demand_per_node * (ind.frac) * ind_demand_adj
  node_demand_total <- round(demand_per_node_res + demand_per_node_ind,2)
  
  
  #:::::::::::::: SUPPLY MODULE  :::::::::::::::::::::::::::::::::::::::::::::::
  
  quality_factor <- (1 +  wqd.rate) ^ yind
  nodes_sim$discharge[snodes] <- nodes_sim$discharge[snodes]/quality_factor
  

  #::::::::::::: NETWORK MODULE ::::::::::::::::::::::::::::::::::::::::::::::::
  
  nodes_sim$discharge[dnodes] <- node_demand_total
  OUT <- calculate_network(nodes_sim, pipes_sim, 1.0, 1)
 
  return(
    list(nodes = nodes_sim %>% mutate(sim = as.numeric(OUT[[2]])),
         pipes = pipes_sim %>% mutate(sim = as.numeric(OUT[[3]]))
    )
  )
  
}

