

######################### MODEL DATA ###########################################


# Load scripts & functions
source("./src/setup.R")

# Directories to read-in data and save results
inPath  <- "./data/v8_2021/"
outPath <- "./results/v8_2021/"

# Read_in node and pipe schematic from csv files: 
nodes_data <- read.csv(paste0(inPath,"nodes.csv"))
nodes_meta <- read.csv(paste0(inPath,"nodes_meta.csv"))
pipes_all <- lapply(1:designNum, function(x) read.csv(paste0(inPath,"pipes_d", x, ".csv")))

designNum <- 5
pipes_data <- pipes_all[[designNum]]



# Background map
load(file = paste0(inPath, "mapFriesland.rda"))

nodes_base <- nodes_data 
pipes_base <- pipes_data

####################### GRAPH ANALYSIS #########################################


# Run combined model 
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
  industry.ratio = 0.3,
  dom.peak.factor = 1,
  ind.peak.factor = 1,
  year.sim = 2031,       
  year.ref = 2021,        
  nodes.data = nodes_base,       
  pipes.data = pipes_base
)
  
# Performance metrics per node
nodes_out <- out$nodes %>% as_tibble() %>%
  mutate(discharge = round(discharge, 2),
         sim = ifelse(type == "supply", round(sim, 2)*-1, round(sim, 2)),
         coverage = round(100 * sim/discharge,2),
         deficit = round(discharge - sim, 2)) %>% 
  select(id, label, type, weight, discharge, sim, coverage, deficit) %>%
  left_join(select(nodes_meta,id,lat,lon), by="id")

pipes_out <- out$pipes %>% as_tibble() %>%
  mutate(coverage = round(100 * abs(sim)/q_max,2)) %>%
  mutate(dummy = as.factor(c(rep(1,50), rep(2,59))))

# Network vizualization

gc()

p <- visualizeWDN(
  nodes.data = nodes_out, 
  pipes.data = pipes_out, 
  node.color.var = "coverage",
  node.size.var = "discharge",
  edge.color.var = "dummy",
  edge.size.var = NULL,
  background.map = mapFriesland,
  scale = FALSE, 
  fill = FALSE
  )
  
p















################################################################################
################################################################################




# Network vizualization
p <- visualizeWDN(
  nodes.df = select(nodes_meta,id,lat,lon) %>% left_join(nodes_tbl, by="id"), 
  pipes.df = pipes_tbl, 
  background.map = mapFriesland,
  metric = "coverage",
  display.node.wgts = TRUE,
  display.constraints = TRUE
)

ggsave(filename = paste0(outPath, "wdnviz.png"), height = 10, width = 12)

















# Define network as undirected
# first two columns of d needs to from/to ids, first column of vertices is the ids
g <- graph_from_data_frame(d=pipes_base, vertices=nodes_base, directed = FALSE) 

dnodes <- nodes_base$id[which(nodes_base$type == "demand")]
snodes <- nodes_base$id[which(nodes_base$type == "supply")] 
snodes_labels <- nodes_base$label[snodes]

# Find connected supply nodes for each demand node  
slink <- matrix(0, nrow = length(dnodes), ncol = length(snodes))
for (d in dnodes) {
  slink[d,] <- sapply(snodes, function(s) length(paths_from_to(g,d,s)))
}
colnames(slink) <- snodes_labels
slink_df <- tibble(id = dnodes) %>% bind_cols(as.data.frame(slink))

####################### NETWORK SIMULATION #####################################

# dev_rate = 0.0005,   # economic_development [%/year]    
# tmp_rate = 0.04,     # annual increase in temperature [C/year]  
# prc_rate  = 0.01,    # price development rate [%/year]   
# pop_rate  = 0.01,    # population growth rate
# dev_elast = 1.0,     # Eco. development elasticity of demand [-]
# prc_elast = -0.2,    # Price elesticity of demand [-]
# tmp_elast = 0.03,    # Temperature elasticity of demand [C^-1]
# indf = 0.3,          # ratio of industrial demand [-]
# year_sim = 2050,     # simulation year [-]
# year_ref = 2021,     # reference year [-]  
# nodes_ref = nodes_data,    # node data as table
# pipes_ref = pipes_data     # pipe data as table

################################################################################
################################################################################
################################################################################
################ SIMPLE EXPERIMENT #############################################

dev_rate <- 0.0005         
tmp_rate = 0.04      
prc_rate  = 0.01        
pop_rate  = 0.01      
dev_elast = 1.0       
prc_elast = -0.2      
tmp_elast = 0.03     
indf = 0.3
qdecline_rate = 0.005

sample_num <- 200
year_df <- tibble(year_sim = seq(2021,2071,5))


sample_set <- tibble(id = 1:sample_num) %>%
  mutate(dev_rate = runif(sample_num, min=dev_rate*0.75, max=dev_rate*1.25)) %>%
  mutate(tmp_rate = runif(sample_num, min=tmp_rate*0.75, max=tmp_rate*1.25)) %>%
  mutate(prc_rate = runif(sample_num, min=prc_rate*0.75, max=prc_rate*1.25)) %>%
  mutate(pop_rate = runif(sample_num, min=pop_rate*0.75, max=pop_rate*1.25)) %>%
  mutate(qdecline_rate = runif(sample_num, min=qdecline_rate*0.75, 
                               max=qdecline_rate*1.25)) 

sample_df <- expandGridDf(sample_set, year_df) %>% mutate(metric = NA)
dnodes <- nodes_data$node_id[which(nodes_data$type == "demand")]
  

scn_num <- nrow(sample_df)

pb <- txtProgressBar(min = 1, max = scn_num, style = 3)
start_time <- Sys.time()


for (i in 1:scn_num) {
  
  # progress bar
  setTxtProgressBar(pb, i)

  # Run combined model 
  result <- simulateWDN(
    dev_rate = sample_df$dev_rate[i],         
    tmp_rate = sample_df$tmp_rate[i],        
    prc_rate  = sample_df$prc_rate[i],            
    pop_rate  = sample_df$pop_rate[i],          
    qdecline_rate = sample_df$qdecline_rate[i], 
    year_sim = sample_df$year_sim[i],      
    nodes_ref = nodes_data,  
    pipes_ref = pipes_data   
  )
  
  sample_df$metric[i] <- round(sum(result$final[dnodes])/sum(result$initial[dnodes])*100,0)
    
  
}
close(pb)
Sys.time() - start_time

write_csv(sample_df, "sample_df.csv")


################################################################################
#### GRAPH THEORY INSIGHTS #####################################################
################################################################################

metricNames <- c("linkdens", 
                 "avgdegree", 
                 "diameter", 
                 "clustering", 
                 "articdens", 
                 "bridgedens")

designNames <- paste0("Design ", 1:designNum)

nMetrics <- replicate(designNum, list())

edgeCon <- vector(mode = "list", length = designNum)

snodes_labels

for (i in 1:designNum) {
  
  # Generate network
  nodes_df <- nodes_data
  pipes_df <- pipes_data_all[[i]] 

  # Number of nodes & links
  nodeNum <- nrow(nodes_df)
  pipeNum <- nrow(pipes_df)
  
  dnodes <- nodes_base$id[which(nodes_base$type == "demand")]
  snodes <- nodes_base$id[which(nodes_base$type == "supply")] 
  snodes_labels <- nodes_base$label[snodes]

  
  # Define network as undirected
  G <- graph_from_data_frame(d=pipes_df, vertices=nodes_df, directed = FALSE) 
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~ Edge connectivity matrix
  foo <- matrix(NA, nrow = length(dnodes), ncol = length(snodes))
  for (d in 1:length(dnodes)) {
    foo[d,] <- sapply(snodes, function(x) edge_connectivity(G, source = dnodes[d], target = x))
  }
  edgeCon[[i]] <- tibble(id = dnodes) %>% bind_cols(as_tibble(foo)) %>% setNames(c("id", snodes_labels))

  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~ Bridges in the network 
  
  ##  Set bridge edges to red:
  num_comp <- length(decompose.graph(G))
  for (i in 1:pipeNum) {
    G_sub <- delete.edges(G, i)
    if (length(decompose.graph(G_sub)) > num_comp) pipes_df$colid[i] <- 0
  }
  
  # Calculate Graph Measures
  nMetrics[[i]]$linkdens   <- edge_density(G, loops = TRUE)
  nMetrics[[i]]$avgdegree  <- mean(degree(G))
  nMetrics[[i]]$diameter   <- diameter(G, directed = FALSE)   
  nMetrics[[i]]$clustering <- transitivity(G, type="global")
  nMetrics[[i]]$articdens  <- length(articulation_points(G))/nodeNum
  nMetrics[[i]]$bridgedens <- if(i %in% 1:2) {NA} else {
    length(which(bridge(G, directed = FALSE, normalize = TRUE)$`Bridge Strength` > 0))/pipeNum}
  
  
  # Network vizualization
  p <- visualizeWDN(
    nodes.df = select(nodes_meta,id,lat,lon) %>% left_join(nodes_df, by="id"), 
    pipes.df = pipes_df, 
    background.map = mapFriesland,
    metric = "elevation",
    display.node.wgts = FALSE,
    display.constraints = FALSE) +
    geom_edges(aes(x=x, y=y, xend=xend, yend=yend, color = colid), 
               data = pipes_df)
  

}

df <- data.frame(matrix(unlist(nMetrics), nrow=length(metricNames), byrow=F), 
                 stringsAsFactors=FALSE) 

colnames(df) <- designNames
df <- df %>% mutate_all(.funs = round, 2) %>% 
  tibble::add_column(metric = metricNames,.before = 1) 





