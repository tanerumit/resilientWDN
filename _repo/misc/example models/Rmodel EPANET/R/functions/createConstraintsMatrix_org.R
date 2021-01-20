

createConstraintsMatrix <- function(edges, total_flow) {
  
  # Edge ids to be used as names
  names_edges <- edges$id
  
  # Number of edges
  numberof_edges <- length(names_edges)
  
  # Node ids to be used as names
  names_nodes <- c(edges$from, edges$to) %>% unique
  # Number of nodes
  numberof_nodes <- length(names_nodes)
  
  # Build constraints matrix
  constraints <- list(
    lhs = NA,
    dir = NA,
    rhs = NA)
  
  #' Build capacity constraints ------------------------------------------------
  #' Flow through each edge should not be larger than capacity.
  #' We create one constraint for each edge. All coefficients zero
  #' except the ones of the edge in question as one, with a constraint
  #' that the result is smaller than or equal to capacity of that edge.
  
  # Flow through individual edges
  constraints$lhs <- edges$id %>%
    length %>%
    diag %>%
    set_colnames(edges$id) %>%
    set_rownames(edges$id)
  # should be smaller than or equal to
  constraints$dir <- rep('<=', times = nrow(edges))
  # than capacity
  constraints$rhs <- edges$capacity
  
  
  #' Build node flow constraints -----------------------------------------------
  #' For each node, find all edges that go to that node
  #' and all edges that go from that node. The sum of all inputs
  #' and all outputs should be zero. So we set inbound edge coefficients as 1
  #' and outbound coefficients as -1. In any viable solution the result should
  #' be equal to zero.
  
  nodeflow <- matrix(0,
                     nrow = numberof_nodes,
                     ncol = numberof_edges,
                     dimnames = list(names_nodes, names_edges))
  
  for (i in names_nodes) {
    
    # input arcs
    edges_in <- edges %>%
      filter(to == i) %>%
      select(id) %>%
      unlist
    # output arcs
    edges_out <- edges %>%
      filter(from == i) %>%
      select(id) %>%
      unlist
    
    # set input coefficients to 1
    nodeflow[
      rownames(nodeflow) == i,
      colnames(nodeflow) %in% edges_in] <- 1
    
    # set output coefficients to -1
    nodeflow[
      rownames(nodeflow) == i,
      colnames(nodeflow) %in% edges_out] <- -1
  }
  
  # But exclude source and target edges
  # as the zero-sum flow constraint does not apply to these!
  # Source node is assumed to be the one with the minimum id number
  # Sink node is assumed to be the one with the maximum id number
  #sourcenode_id <- min(edges$from)
  #targetnode_id <- max(edges$to)
  # Keep node flow values for separate step below
  #nodeflow_source <- nodeflow[rownames(nodeflow) == sourcenode_id,]
  #nodeflow_target <- nodeflow[rownames(nodeflow) == targetnode_id,]
  # Exclude them from node flow here
  #nodeflow <- nodeflow[!rownames(nodeflow) %in% c(sourcenode_id, targetnode_id),]
  
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