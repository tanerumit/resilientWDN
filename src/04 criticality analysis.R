


################################################################################
#::::::::::::::::::: GENERAL SETUP :::::::::::::::::::::::::::::::::::::::::::::
################################################################################


# Load scripts & functions
source("./src/setup.R")
source("./src/functions/simulateWDN.R")
source("./src/functions/visualizeWDN.R")

# Key settings
rel_threshold <- 100

# Directories to read-in data and save results
inPath  <- "./data/v9/"
outPath <- "./results/0527/"

# Background map
load(file = paste0(inPath, "mapFriesland.rda"))

################################################################################

# DEFINE PORTFOLIOS ------------------------------------------------------------

# Read_in node and pipe table from excel. These tables need to include all 
# nodes and link (existing + new) accross all portfolios
nodes_info <- read_excel(paste0(inPath,"nodes_data.xlsx")) 
pipes_info <- read_excel(paste0(inPath,"pipes_data.xlsx")) 

# Calculate pipe lengths based on coordinates
pipes_info$length <- sapply(1:nrow(pipes_info), function(x)
  distm(c(nodes_info$lon[pipes_info$start[x]], 
          nodes_info$lat[pipes_info$start[x]]), 
        c(nodes_info$lon[pipes_info$end[x]], 
          nodes_info$lat[pipes_info$end[x]]), 
        fun = distGeo))

# Store all portfolios
nodes_data <- list()
pipes_data <- list()

# Indices of existing nodes and pipes in the base design
ex_nodes <- 1:48
ex_pipes <- 1:52

# Indices of new nodes/pipes in each design
new_nodes <- list(d1=NULL, d2=c(49), d3=c(50), d4=c(51), d5=c(52), d6=NULL)
new_pipes <- list(d1=NULL, d2=c(53,54,55), d3=c(56,57,58), d4=c(59,60,61), 
                  d5=c(62,63,64,65), d6=c(66,67,68))


# Set portfolios
nodes_data <- lapply(1:length(new_nodes), function(x) nodes_info %>% 
                       filter(id %in% c(ex_nodes, new_nodes[[x]])))

pipes_data <- lapply(1:length(new_pipes), function(x) pipes_info %>% 
                       filter(id %in% c(ex_pipes, new_pipes[[x]])))

# Set pipe labels
pipes_data <- lapply(pipes_data, function(x) mutate(x, label=paste0(start,"_",end)))

#	Pipe costs: €1M/mm/km  
pipe_unit_cost <- 1  
pipe_cost_tbl <- pipes_info %>% select(start:diameter) %>%
  mutate(cost = ifelse(id %in% ex_pipes, 0, diameter * length * pipe_unit_cost/10^6))

#	Production costs: €5 /m3
supply_unit_cost <- 5 
node_cost_tbl <- nodes_info %>% select(id:discharge) %>%
  mutate(cost = ifelse(id %in% ex_nodes, 0, supply_unit_cost * discharge*24*365/10^6)) %>%
  mutate(cost = ifelse(type == "demand", 0, cost))

cap_cost_nodes <- sapply(new_nodes, function(x) sum(node_cost_tbl$cost[x]))
cap_cost_pipes <- sapply(new_pipes, function(x) sum(pipe_cost_tbl$cost[x]))   
total_costs <-  cap_cost_nodes + cap_cost_pipes


################################################################################

# DEFINE SCENARIO SPACE --------------------------------------------------------

# Trend scenarios: demand increase  
# Shock scenarios: supply site, network failure

# ::::::::::::::::::::: STOCHASTIC FAILURE SCENARIOS :::::::::::::::::::::::::::

# Sample size for failure scenarios
smax <- 100

# Set demand and supply node indices
snodes <- nodes_data[[1]]$id[which(nodes_data[[1]]$type == "supply")]
dnodes <- nodes_data[[1]]$id[which(nodes_data[[1]]$type == "demand")]

node_num <- nrow(nodes_data[[1]])
pipe_num <- nrow(pipes_data[[1]])

## Generate supply failures based on frequencies 
node_failure_mat <- matrix(data =NA, nrow = smax, ncol = node_num)
pipe_failure_mat <- matrix(data =NA, nrow = smax, ncol = pipe_num)

# Sample node/pipe failures
# Make sure each sample has at least one link or source failure 
for (s in 1:smax) {
  
  failureT <- 0
  
  while(failureT < 1) {
    
    supply_failure <- sapply(1:node_num, function(x) 
      rbinom(n=1, size=1, prob = nodes_data[[1]]$prFail[x]))
    
    pipe_failure <- sapply(1:pipe_num, function(x) 
      rbinom(n=1, size=1, prob = pipes_data[[1]]$prFail[x]))
    
    failureT <- sum(supply_failure) + sum(pipe_failure)
    
  } 
  
  node_failure_mat[s, ] <- supply_failure
  pipe_failure_mat[s, ] <- pipe_failure
  
  s = s + 1
}

# Failure scenarios
snode_failures <- lapply(1:nrow(node_failure_mat), 
                        function(x) which(node_failure_mat[x,]==1))
pipe_failures   <- lapply(1:nrow(pipe_failure_mat), 
                        function(x) which(pipe_failure_mat[x,]==1))

scn_stochastic_mat <- tibble(stoc_id=1:smax, supplyF = snode_failures, 
                        pipeF = pipe_failures)

# ::::::::::::: TREND SCENARIOS ('NARRATIVES') :::::::::::::::::::::::::::::::::

# Read-in trend scenarios from table
scn_trends_data  <- read_excel(paste0(inPath,"ScenarioDev.xlsx"), sheet = 2) 
scn_trends_info <- read_excel(paste0(inPath,"ScenarioDev.xlsx"), sheet = 3) 
scn_trends_ini <- scn_trends_data %>% select(-id)  

scn_select <- c(1,3,5)

scn_trends <- scn_trends_ini[,scn_select]
scn_trends_labels <- scn_trends_info$label[scn_select]
trends_num  <- ncol(scn_trends)
scn_trends_mat <- tibble(trend_id = 1:trends_num, 
  demandM = lapply(1:trends_num, function(x) unlist(scn_trends[,x], use.names = F)))

######### COMBINED SCENARIOS
scn_mat <- expandGridDf(scn_stochastic_mat, scn_trends_mat) %>%
  add_column(scn=NA, .before = "stoc_id") %>%
  mutate(scn = 1:n()) 

################################################################################

#::::::::::::  RUN SCENARIOS :::::::::::::::::::::::::::::::::::::::::::::::::::

#Loop through actions

# desicision and scenario space
diter <- 1:2
siter <- scn_mat$scn

dmax <- length(diter)
smax <- length(siter)

scnOut_all <- vector("list", length = dmax)
names(scnOut_all) <- diter

pb <- txtProgressBar(min = 1, max = smax*dmax, style = 3)
start_time <- Sys.time()

node_id0  <-  nodes_data[[1]]$id
snode_id0 <- nodes_data[[1]]$id[which(nodes_data[[1]]$type == "supply")]
dnode_id0 <- nodes_data[[1]]$id[which(nodes_data[[1]]$type == "demand")]
pipe_id0  <- pipes_data[[1]]$id

for (d in 1:dmax) {
  
  # Current design
  dX <- diter[d]
    
  # Define empty list to store all results
  scnOut <- vector(mode = "list", length = smax) 
  
  # Read-in network data for the current configuration (d)
  nodes_d <- nodes_data[[dX]]
  pipes_d <- pipes_data[[dX]]
  
  # Set demand and supply node indices for the current configuration
  snode_id <- nodes_d$id[which(nodes_d$type == "supply")]
  dnode_id <- nodes_d$id[which(nodes_d$type == "demand")]
  
  # Set spare supply capacity (base system nodes only)
  ind1 <- which(nodes_d$id %in% snode_id0)
  nodes_d$discharge[ind1] <- nodes_d$discharge[ind1]  * 1.05
  
  #Loop through scenarios
  for (s in 1:smax) {
    
    # Current scenario
    sX <- siter[s]
    
    # progress bar
    setTxtProgressBar(pb, s*d)
    
    #Reset node and link data
    nodes_scn <- nodes_d
    pipes_scn <- pipes_d
    
    # Set supply/link failures based on current scenario
    supplyF_s <- unlist(scn_mat$supplyF[sX])
    nodes_scn$disuse[which(nodes_scn$id %in% supplyF_s)] <- 1
    
    pipeF_s <- unlist(scn_mat$pipeF[sX])
    pipes_scn$disuse[which(pipes_scn$id %in% pipeF_s)] <- 1
    
    # Set trends for nodes
    ind <- which(nodes_scn$id %in% node_id0)
    nodes_scn$discharge[ind] <- nodes_scn$discharge[ind] * scn_mat$demandM[[sX]]
    
    # Run simulation model
    scnOut[[s]] <- simulateWDN(
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
      nodes.data = nodes_scn,       
      pipes.data = pipes_scn
    )
  }
  
  scnOut_all[[d]] <- scnOut
  
}
Sys.time() - start_time
close(pb)

#save(scnOut_all, scn_mat, file = "./results/test_runs_0525.rdata")


################################################################################

# PERFORMANCE METRICS ----------------------------------------------------------

#load("./results/v9/scn_7500_.rdata")
trend_id_all <- 16
scn_trends_ids <- c(unique(scn_mat$trend_id), trend_id_all)
scn_trends_labels2 <-  c(scn_trends_labels, "ALL")
  
# Store results in lists
nodesOut <- list()
pipesOut <- list()



# Calculate per node statistics for each scenario
nodesOut_scenario_summary <- list()
pipesOut_scenario_summary <- list()
network_scenario_summary <- list()

# scnOut_all: for all designs and scenarios list of full node and pipe results
#nodesOut: for all scenarios, list of full node results parsed...
#pipesOut: for all scenarios, list of full node results parsed...

for (d in 1:dmax) {
  
  # :::::::::::: Results post-processing :::::::::::::::::::::::::::::::::::::::
  
  nodesOut[[d]] <- lapply(1:smax, function(x) scnOut_all[[d]][[x]]$nodes) %>%
    bind_rows(.id = "scn") %>% 
    mutate(scn = siter[as.numeric(scn)]) %>%
    select(scn, id, label, lat, lon, type, discharge, disuse, ind_ratio, sim, rel) %>%
    left_join(scn_mat, by = "scn")

  pipesOut[[d]] <- lapply(1:smax, function(x) scnOut_all[[d]][[x]]$pipes) %>%
    bind_rows(.id = "scn") %>%
    mutate(scn = siter[as.numeric(scn)]) %>%
    select(scn, start, end, id, length, diameter, disuse, q_max, sim, usage) %>%
    left_join(scn_mat, by = "scn")
  
  #::::::::: Results summarized per node/ per trend scenario :::::::::::::::::::
  
  nodesOut_scenario_summary_all <- nodesOut[[d]] %>%
    group_by(id) %>%
    summarize(mrel = round(sum(sim)/sum(discharge) * 100),
              robust = 100 * length(which(rel>=rel_threshold))/n()) %>%
    mutate(trend_id = trend_id_all) %>%
    right_join(nodes_data[[d]], by = "id")
                
  nodesOut_scenario_summary[[d]] <- nodesOut[[d]] %>%
    group_by(id, trend_id) %>%
    summarize(mrel = round(sum(sim)/sum(discharge) * 100),
              robust = 100 * length(which(rel>=rel_threshold))/n()) %>%
    right_join(nodes_data[[d]], by = "id") %>%
    bind_rows(nodesOut_scenario_summary_all) %>%
    arrange(trend_id, id)
  
  pipesOut_scenario_summary_all <- pipesOut[[d]] %>%
    group_by(id) %>%
    summarize(linkuse = round(sum(sim)/sum(q_max) * 100)) %>%
    mutate(trend_id = trend_id_all) %>%
    right_join(pipes_data[[d]], by = "id")
  
  pipesOut_scenario_summary[[d]] <- pipesOut[[d]] %>%
    group_by(id, trend_id) %>%
    summarize(linkuse = round(sum(sim)/sum(q_max) * 100)) %>%
    right_join(pipes_data[[d]], by = "id") %>%
    bind_rows(pipesOut_scenario_summary_all)  %>%
    arrange(trend_id, id)
  
  
  #::::::::: Network results per trend scenario ::::::::::::::::::::::::::::::::
  
  # Pipes
  network_scenario_summary_pipes_all <- pipesOut[[d]] %>%
    filter(disuse == 0) %>%
    summarize(linkuse = round(sum(sim)/sum(q_max) * 100)) %>%
    mutate(trend_id = trend_id_all)
    
  network_scenario_summary_pipes <- pipesOut[[d]] %>%
    filter(disuse == 0) %>%
    group_by(trend_id) %>%    
    summarize(linkuse = round(sum(sim)/sum(q_max) * 100)) %>%
    bind_rows(network_scenario_summary_pipes_all)
  
  # Demand nodes
  network_scenario_summary_dnodes_all <- nodesOut[[d]] %>%
    filter(type == "demand") %>% 
    filter(disuse == 0) %>%
    summarize(mrel = round(sum(sim)/sum(discharge) * 100),
              robust = 100 * length(which(rel>=rel_threshold))/n()) %>%
    mutate(trend_id = trend_id_all)
  
  network_scenario_summary_dnodes <-  nodesOut[[d]] %>%
    filter(type == "demand") %>% 
    filter(disuse == 0) %>%
    group_by(trend_id) %>% 
    summarize(mrel = round(sum(sim)/sum(discharge) * 100),
              robust = 100 * length(which(rel>=rel_threshold))/n()) %>%
    bind_rows(network_scenario_summary_dnodes_all)
  
  #Supply nodes
  network_scenario_summary_snodes_all <- nodesOut[[d]] %>%
    filter(type == "supply") %>% 
    filter(disuse == 0) %>%
    summarize(supuse = round(sum(sim)/sum(discharge) * 100)) %>%
    mutate(trend_id = trend_id_all)
  
  network_scenario_summary_snodes <-  nodesOut[[d]] %>%
    filter(type == "supply") %>% 
    filter(disuse == 0) %>%
    group_by(trend_id) %>% 
    summarize(supuse = round(sum(sim)/sum(discharge) * 100)) %>%
    bind_rows(network_scenario_summary_snodes_all)
  
  network_scenario_summary[[d]] <- network_scenario_summary_dnodes %>%
    left_join(network_scenario_summary_snodes, by = "trend_id") %>%
    left_join(network_scenario_summary_pipes, by = "trend_id") %>%
    gather(key = variable, value = value, -trend_id) %>%
    spread(key = trend_id, value = value)   
}


################################################################################

# RESULTS VISUALIZATION --------------------------------------------------------

xmax <- length(scn_trends_labels2)

### Visualize robustness & resilience over scenarios
for (d in 1:dmax) {

  # Loop over trend scenarios
  for (x in 1:xmax) {
    
    x_index <- scn_trends_ids[x]
    
    # Current trend scenario
    nodesOut_summary_c <- nodesOut_scenario_summary[[d]] %>% 
      filter(trend_id == x_index)
    
    nodesOut_summary_c_base <- nodesOut_scenario_summary[[1]] %>% 
      filter(trend_id == x_index) 
    
    pipesOut_summary_c <- pipesOut_scenario_summary[[d]] %>% 
      filter(trend_id == x_index)
    
    pipesOut_summary_c_base <- pipesOut_scenario_summary[[1]] %>% 
      filter(trend_id == x_index) 
    
    network_summary_c <- network_scenario_summary[[d]][,c(1,x+1)]
    
    p1 <- visualizeWDN(
      nodes.data = nodesOut_summary_c,
      pipes.data = pipesOut_summary_c,
      background.map = mapFriesland,
      node.fill.var = "robust",
      node.size.var = "discharge",
      edge.color.var = "linkuse",
      edge.size.var = "diameter",
      show.legend = FALSE,
      plot.title = scn_trends_labels2[x]) 
    
    
    tb <- network_summary_c %>% select(-trend_id) %>%
      gather(key = Metric, value = "[%]") %>%
      mutate(`[%]` = round(`[%]`,0)) %>%
      mutate(Metric = factor(Metric, 
                             levels = c("robustness", "mean_rel", "link_usage"),
                             labels = c("Robustness", "Mean Reliability","Link Cap. Usage")))
    
    df <- tibble(x = 0.001, y = 0.001, tb = list(tb))

    p1 <- p1 + geom_table_npc(data = df, aes(npcx=x, npcy=y, label = tb), 
                                             size = 3, table.hjust = 0,
                            table.theme = ttheme_gtstripes) 



    p2 <- ggplot(nodesOut_summary_c %>% filter(type == "demand"), 
                 aes(x = reorder(id, -rel_count), y = rel_count)) +
      theme_bw(base_size = 12) +
      geom_point(aes(fill = rel_count), color = "black", 
                  shape = 21, size = point_size, stroke = 1) +
      coord_flip() +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0,100,50)) +
      scale_fill_gradient2(low = "red", mid = "white", high = "white", midpoint = 99, 
                           limits = c(0, 100),
                           breaks = c(0, 20, 40, 60, 80, 99, 100),
                           labels = c(0, 20, 40, 60, 80, 99, 100)) +
      labs(y = "", x = "", color = "") +
      guides(fill = FALSE)  +
      ggtitle(label = "", subtitle = "Nodes")
  
    
    p3 <- ggplot(pipesOut_summary_c, 
                 aes(x = reorder(label, -usage_mean), y = usage_mean)) +
      theme_bw(base_size = 12) +
      geom_point(aes(fill = usage_mean), color = "gray50", 
                 alpha = 0.9, shape = 21, size = point_size, stroke = 1) +
      coord_flip() +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0,100,50)) +
      scale_fill_viridis(
        name = "Pipe\nUse [%]",
        limits = c(0,100),
        breaks = c(0,20, 40, 60, 80, 99, 100),
        labels = c(0, 20, 40, 60, 80,"", 100),
        guide = guide_colorbar(order = 3, barheight = unit(5, 'cm')), direction = -1) +
      labs(y = "", x = "", color = "") +
      guides(fill = FALSE) +
      ggtitle(label = "", subtitle = "Linkages")
    
    
    if (d > 1) {
      
      p2 <- p2 + geom_point(data = nodesOut_summary_c_base %>% filter(type == "demand"),
                            shape = 21, size = point_size, color = alpha("black", .5),
                            fill = alpha("gray95", .1))
      
      p3 <-  p3 + geom_point(data = pipesOut_summary_c_base,
                             shape = 21, size = point_size, color = alpha("black", .5),
                             fill = alpha("gray95", .1))
    }
    
    p <- cowplot::plot_grid(p1, p2, p3, nrow = 1, rel_widths = c(4,1.25,1.25), align = c("hv"))
                            #labels = c("a)", "b)","c)"))
    
    ggsave(filename = paste0(outPath, "scn",x,"_d", d,"_", scn_trends_labels2[x],".png"), 
           height = 8*0.9, width = 13*0.9)
    
  }
  
}

