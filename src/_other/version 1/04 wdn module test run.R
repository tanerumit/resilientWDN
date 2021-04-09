
### Single scenario run & tests
source("./src/global.R")

#friesland_nodes  <- read.csv("./data/v8/nodes.csv")
#friesland_pipes  <- read.csv("./data/v8/pipes.csv")

# Read_in node and pipe schematic from csv files: 
friesland_nodes  <- read.csv("./data/v8/nodes_v8.csv")
friesland_pipes  <- read.csv("./data/v8/pipes_v8_d1.csv")

# replace NAs with zeros for reticulate
friesland_pipes[is.na(friesland_pipes)]  <- 0 
friesland_nodes[is.na(friesland_nodes)]  <- 0

### Single run 
node_df <- friesland_nodes
pipe_df <- friesland_pipes

# index numbers of demand and supply nodes in the nodes master table
dindex <- node_df$node_id[which(node_df$type == "demand")]
sindex <- node_df$node_id[which(node_df$type == "supply")]
node_df %>% group_by(type) %>% summarize(dischage = sum(discharge))

# Reduce suppy to 80% of demand
node_df[sindex,"discharge"] <- node_df[sindex,"discharge"] * 0.8
node_df %>% group_by(type) %>% summarize(dischage = sum(discharge))


### Network optimization ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#calculate_network(nodes_csv, pipes_csv, head_loss_option)

# nodes_csv: node data (required columns: node_id, type, weight, discharge, elevation, max_supply_head, disuse)
# pipes_csv: pipe data (required columns: pipe_id, start, end, length, diameter, disuse, qmax)

# penalty_order: penalty weight assigned to deficits (penalty order can be used to make sure shortages are spread out by setting it to 2.0 - only works for head_loss_option 1 or 2)
# head_loss_option: 1 = discharge only, 2 = linear head loss equation, 3 = mixed integer problem with non-linear (more accurate) head loss equation.


# Calculate network
out1 <- calculate_network(node_df, pipe_df, 2.0, 1)
out2 <- calculate_network(node_df, pipe_df, 2.0, 2)
out3 <- calculate_network(node_df, pipe_df, 1.0, 3)
out3 <- calculate_network(node_df, pipe_df, 2.0, 3)

out1[[1]] # total shortage 
out1[[2]] # Map[node_id, discharge]
out1[[3]] # Map[pipe_id, discharge])

#Sys.time() - start_time

