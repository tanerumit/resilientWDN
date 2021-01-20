
source("./R/global.R")

library(visNetwork)
library(geomnet)
library(igraph)

#Read-in network data
nodelist <- read_excel(path = "./input/network2.xlsx", sheet = "nodes")
edgelist <- read_excel(path = "./input/network2.xlsx", sheet = "edges")
edgelist$label <- NA




# Plot the network (simple)
g <- graph_from_edgelist(as.matrix(edgelist[,c('from','to')]))
plot(g)
E(g)$capacity <- edgelist$capacity
E(g)$cost     <- edgelist$cost
plot(g, edge.label = E(g)$capacity)
plot(g, edge.label = E(g)$cost)

# Plot network (interactive)
visNetwork(nodes = nodelist, edges = edgelist, width = "100%") %>%
visIgraphLayout() %>%
  visNodes(shape = "dot",
           color = list(background = "#0085AF", border = "#013848", highlight = "#FF8000"),
           shadow = list(enabled = TRUE, size = 10)) %>%
  visEdges(shadow = FALSE, color = list(color = "#0085AF", highlight = "#C62F4B")) %>%
  visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T), selectedBy = "group") %>% 
  visLayout(randomSeed = 11)


################################################################################


#### Constraints matrix
createConstraintsMatrix <- function(edges, total_flow) {
  
  # Edge IDs to be used as names
  names_edges <- edges$ID
  
  # Number of edges
  numberof_edges <- length(names_edges)
  
  # Node IDs to be used as names
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
  constraints$lhs <- edges$ID %>%
    length %>%
    diag %>%
    set_colnames(edges$ID) %>%
    set_rownames(edges$ID)
  
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
      select(ID) %>%
      unlist
    # output arcs
    edges_out <- edges %>%
      filter(from == i) %>%
      select(ID) %>%
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
constraintsMatrix       <- createConstraintsMatrix(edges = edgelist, total_flow = 30)


## Optimization routine ********************************************************
library(lpSolve)


# Run lpSolve to find best solution
solution <- lp(
  direction = 'min',
  objective.in = edgelist$cost,
  const.mat = constraintsMatrix$lhs,
  const.dir = constraintsMatrix$dir,
  const.rhs = constraintsMatrix$rhs)

# Print vector of flow by edge
solution$solution

# Include solution in edge dataframe
edgelist$flow <- solution$solution