

createConstraintsMatrix <- function(edges, total_flow) {
  
  # Define edges of the network
  names_edges <- edges$ID # edge names
  numberof_edges <- length(names_edges) # number of edges
  
  # Define nodes of the network 
  names_nodes <- c(edges$from, edges$to) %>% unique
  numberof_nodes <- length(names_nodes) # number of nodes
  
  # Build constraints matrix
  # Consists of multiple constraints e.g., cons1, cons2, cons3
  constraints <- list(lhs = NA, dir = NA, rhs = NA)
  
  
  #' BUILD CAPACITY CONSTRAINTS ------------------------------------------------
  #' Flow through each edge should not be larger than the edge capacity.
  
  cons1 <- constraints
  
  #' We create one constraint for each edge. All coefficients zero
  #' except the ones of the edge in question as one, with a constraint
  #' that the result is smaller than or equal to capacity of that edge.
  
  # Flow through individual edges should be smaller than or equal to than capacity
  cons1$lhs <- edges$ID %>% length %>% diag %>%
    set_colnames(edges$ID) %>% set_rownames(edges$ID)
  cons1$dir <- rep('<=', times = nrow(edges))
  cons1$rhs <- edges$capacity
  
  
  #' Build node flow constraints -----------------------------------------------
  #' For each node, find all edges that go to that node and all edges that go from that node. 
  #' The sum of all inputs and all outputs should be zero.

  cons2 <- constraints
  
  #prepare nodeflow matrix 
  #' So we set inbound edge coefficients as 1 and outbound coefficients as -1. 
  nodeflow <- matrix(0,
                     nrow = numberof_nodes,
                     ncol = numberof_edges,
                     dimnames = list(names_nodes, names_edges))
  
  for (i in names_nodes) {
    
    # input arcs
    edges_in <- edges %>%
      filter(to == i) %>%
      select(ID) %>%
      unlist
    
    # output arcs
    edges_out <- edges %>%
      filter(from == i) %>%
      select(ID) %>%
      unlist
    
    # set input coefficients to 1
    nodeflow[rownames(nodeflow) == i, colnames(nodeflow) %in% edges_in] <- 1
    
    # set output coefficients to -1
    nodeflow[rownames(nodeflow) == i, colnames(nodeflow) %in% edges_out] <- -1
    
  }
  
  # But exclude source and target edges
  # as the zero-sum flow constraint does not apply to these!
  
  # Source node is assumed to be the one with the minimum ID number
  # Sink node is assumed to be the one with the maximum ID number
  sourcenode_id <- min(edges$from)
  targetnode_id <- max(edges$to)
  
  # Keep node flow values for separate step below
  nodeflow_source <- nodeflow[rownames(nodeflow) == sourcenode_id,]
  nodeflow_target <- nodeflow[rownames(nodeflow) == targetnode_id,]
  
  # Exclude them from node flow here
  nodeflow <- nodeflow[!rownames(nodeflow) %in% c(sourcenode_id, targetnode_id),]
  
  # Add nodeflow to the constraints list
  constraints$lhs <- rbind(constraints$lhs, nodeflow)
  constraints$dir <- c(constraints$dir, rep('==', times = nrow(nodeflow)))
  constraints$rhs <- c(constraints$rhs, rep(0, times = nrow(nodeflow)))
  
  
  #' Build initialisation constraints ------------------------------------------
  #' For the source and the target node, we want all outbound nodes and
  #' all inbound nodes to be equal to the sum of flow through the network
  #' respectively
  
  # Add initialisation to the constraints list
  constraints$lhs <- rbind(constraints$lhs,
                           source = nodeflow_source,
                           target = nodeflow_target)
  constraints$dir <- c(constraints$dir, rep('==', times = 2))
  # Flow should be negative for source, and positive for target
  constraints$rhs <- c(constraints$rhs, total_flow * -1, total_flow)
  
  return(constraints)
}