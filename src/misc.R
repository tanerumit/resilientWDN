
### Experiment 
design_set <- 3 #1:5
supply_set <- c(0, snodes)
scn_tbl <- expandGrid(design = design_set, supplyF = supply_set) %>%
  mutate(Rel = NA)  %>%
  arrange(design)
scn_num <- nrow(scn_tbl)

pb <- txtProgressBar(min = 1, max = scn_num, style = 3)
start_time <- Sys.time()
for (i in 1:nrow(scn_tbl)) {
  
  # progress bar
  setTxtProgressBar(pb, i)
  
  fsupply <- scn_tbl$supplyF[i]
  des <- scn_tbl$design[i]
  
  nodes_base <- nodes_data
  snodes <- nodes_base$id[which(nodes_base$type == "supply")]
  
  nodes_base$disuse[which(nodes_base$id %in% fsupply)] <- 1
  nodes_base$discharge[snodes] <- nodes_base$discharge[snodes] * 1.1  #1.50
  
  pipes_base <- pipes_all[[des]] 
  pipes_base$q_max <- pipes_base$q_max  
  
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
    dom.peak.factor = 1,
    ind.peak.factor = 1,
    year.sim = 2021,       
    year.ref = 2021,        
    nodes.data = nodes_base,       
    pipes.data = pipes_base
  )
  
  # global_results
  resultSum <- tibble(name = c("Demand", "Supply", "PipeUse", "Performance"))
  resultSum$ini <- NA
  resultSum$sim <- NA
  
  resultSum$ini[1] <- out$nodes %>% filter(type == "demand") %>% pull(discharge) %>% sum()
  resultSum$sim[1] <- out$nodes %>% filter(type == "demand") %>% pull(sim) %>% sum()
  resultSum$ini[2] <- out$nodes %>% filter(type == "supply") %>% pull(discharge) %>% sum()
  resultSum$sim[2] <- out$nodes %>% filter(type == "supply") %>% pull(sim) %>% sum() %>% abs()
  resultSum$ini[3] <- out$pipes %>% pull(q_max) %>% sum()
  resultSum$sim[3] <- out$pipes %>% pull(sim) %>% sum()
  resultSum$sim[4] <- resultSum$sim[1]/resultSum$ini[1] * 100
  
  scn_tbl$Rel[i] <- resultSum$sim[4]
  
}
close(pb)
Sys.time() - start_time

scn_tbl %>%
  left_join(select(nodes_data, id, label), by = c("supplyF" = "id")) %>%
  arrange(label)


group_by(design) %>%
  summarize(min = min(Rel), max = max(Rel), mean=mean(Rel)) %>%
  
  
  
  # Network vizualization
  
  # Performance metrics per node
  nodes_out <- out$nodes %>% as_tibble() %>%
  mutate(discharge = round(discharge, 2),
         sim = ifelse(type == "supply", round(sim, 2)*-1, round(sim, 2)),
         rel = round(100 * sim/discharge,0)) %>% 
  select(-elevation, -use_pump_curve,-max_supply_head) %>%
  left_join(select(nodes_meta,id,lat,lon), by="id")

pipes_out <- out$pipes %>% as_tibble() %>%
  select(-flowdir, -has_booster, -booster_hmax) %>%
  mutate(coverage = round(100 * abs(sim)/q_max,0)) 


p <- visualizeWDN(
  nodes.data = nodes_out, 
  pipes.data = pipes_out, 
  node.color.var = "rel",
  node.size.var = NULL,
  edge.color.var = NULL,
  edge.size.var = NULL,
  background.map = mapFriesland,
  size = FALSE,
  color = FALSE,
  fill = FALSE); p
p

ggsave(filename = paste0(outPath, "wdnviz.png"), height = 10, width = 12)



################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################



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

designNames <- paste0("Design ", 1:designNumMax)
nMetrics <- replicate(designNumMax, list())
edgeCon <- vector(mode = "list", length = designNumMax)


for (i in 1:designNumMax) {
  
  # Generate network
  nodes_df <- nodes_data
  pipes_df <- pipes_all[[i]] 
  
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
  for (x in 1:pipeNum) {
    G_sub <- delete.edges(G, x)
    if (length(decompose.graph(G_sub)) > num_comp) pipes_df$colid[x] <- 0
  }
  
  # Calculate Graph Measures
  nMetrics[[i]]$linkdens   <- edge_density(G, loops = TRUE)
  nMetrics[[i]]$avgdegree  <- mean(degree(G))
  nMetrics[[i]]$diameter   <- diameter(G, directed = FALSE)   
  nMetrics[[i]]$clustering <- transitivity(G, type="global")
  nMetrics[[i]]$articdens  <- length(articulation_points(G))/nodeNum
  nMetrics[[i]]$bridgedens <- if(i %in% 1:2) {NA} else {
    length(which(bridge(G, directed = FALSE, normalize = TRUE)$`Bridge Strength` > 0))/pipeNum}
  
  
  # # Network vizualization
  # p <- visualizeWDN(
  #   nodes.df = select(nodes_meta,id,lat,lon) %>% left_join(nodes_df, by="id"), 
  #   pipes.df = pipes_df, 
  #   background.map = mapFriesland,
  #   metric = "elevation",
  #   display.node.wgts = FALSE,
  #   display.constraints = FALSE) +
  #   geom_edges(aes(x=x, y=y, xend=xend, yend=yend, color = colid), 
  #              data = pipes_df)
  
  
}

df <- data.frame(matrix(unlist(nMetrics), nrow=length(metricNames), byrow=F), 
                 stringsAsFactors=FALSE) 

colnames(df) <- designNames
df <- df %>% mutate_all(.funs = round, 2) %>% 
  tibble::add_column(metric = metricNames,.before = 1) 




