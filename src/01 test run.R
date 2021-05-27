


d <- 1

nodes_ref <- nodes_data[[d]]
pipes_ref <- pipes_data[[d]]

dnodes <- nodes_ref$id[which(nodes_ref$type == "demand")]
snodes <- nodes_ref$id[which(nodes_ref$type == "supply")] 

# Set node capacity to be +10% of demand
nodes_ref$discharge[snodes] <- nodes_ref$discharge[snodes] * 1.05

# Set supply node failures based on current scenario
supplyF_i <- 0
nodes_ref$disuse[which(nodes_ref$id %in% supplyF_i)] <- 1

# Set pipe failures based on current scenario
pipeF_i   <- 0
pipes_ref$disuse[which(pipes_ref$id %in% pipeF_i)] <- 1


out <- simulateWDN(
  edev.change = 0.0005,   
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
  year.sim = 2021,       
  year.ref = 2021,        
  nodes.data = nodes_ref,       
  pipes.data = pipes_ref,
  global.output = TRUE)

p <- visualizeWDN(
  nodes.data = out$nodes,
  pipes.data = out$pipes,
  node.fill.var = "rel",
  node.size.var = "discharge",
  edge.color.var = "usage",
  edge.size.var = "diameter",
  background.map = mapFriesland)

ggsave(filename = paste0("runtest.png"), height = 8, width = 12)


#ggtitle(paste0("Scenario index: ",i, " - Design: ",d))

df <- tibble(x = 0.15, y = 0.05, tb = list(out$summary))

p <- p + geom_table_npc(data = df, aes(npcx=x, npcy=y, label = tb),
                        size = 3, table.hjust = 0,
                        table.theme = ttheme_gtstripes)

ggsave(filename = paste0("runtest.png"), height = 8, width = 13)



################################################################################
################################################################################
################################################################################



##
# Run combined model 
edev.change = 0.0005   
tdev.change = 0.01
temp.change = 0.04       
price.change = 0.01        
pop.change  = 0.01 
dnetwork.change = 0.01
wqual.change = 0.005
edev.elasticity = 1.0       
price.elasticity = -0.2      
temp.elasticity = 0.03     
dom.peak.factor = 1
ind.peak.factor = 1
year.sim = 2021       
year.ref = 2021        
nodes.data = nodes_ref       
pipes.data = pipes_ref
global.output = TRUE



# Network vizualization
nodes.data = out$nodes
pipes.data = out$pipes 
node.fill.var = "rel"
node.size.var = "discharge"
edge.color.var = "usage"
edge.size.var = "diameter"
background.map = mapFriesland
edge.color.threshold = 95




