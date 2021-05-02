

######################### MODEL DATA ###########################################

# Load scripts & functions
source("./src/setup.R")
source("./src/functions/simulateWDN.R")
source("./src/functions/visualizeWDN.R")

# Directories to read-in data and save results
inPath  <- "./data/v9/"
outPath <- "./results/v9/0428/"

# Background map
load(file = paste0(inPath, "mapFriesland.rda"))

# Read_in base network from excel 
nodes_data <- list()
pipes_data <- list()
nodes_data_base <- read_excel(paste0(inPath,"nodes_d1.xlsx"))
pipes_data_base <- read_excel(paste0(inPath,"pipes_d1.xlsx"))
nodes_data[[1]] <- nodes_data_base
pipes_data[[1]] <- pipes_data_base

#:::::::::: REFERENCE RUN ::::::::::::::::::::::::::::::::::::::::::::::::::::::

nodes_ref <- nodes_data[[1]] 
pipes_ref <- pipes_data[[1]] 

dnodes <- nodes_ref$id[which(nodes_ref$type == "demand")]
snodes <- nodes_ref$id[which(nodes_ref$type == "supply")] 

# Set node capacity to be +10% of demand
nodes_ref$discharge[snodes] <- nodes_ref$discharge[snodes] * 1.05

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

# Network vizualization
nodes.data = out$nodes
pipes.data = out$pipes 
node.fill.var = "rel"
node.size.var = "discharge"
edge.color.var = "usage"
edge.size.var = "diameter"
background.map = mapFriesland


p <- visualizeWDN(
  nodes.data = out$nodes,
  pipes.data = out$pipes,
  node.fill.var = "rel",
  node.size.var = "discharge",
  edge.color.var = "usage",
  edge.size.var = "diameter",
  background.map = mapFriesland) 

ggsave(filename = paste0(outPath, "run_reference.png"), height = 8, width = 11)


################################################################################
################################################################################
################################################################################
################################################################################


