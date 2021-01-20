
################################################################################
# Drinking Water System Resilience Analysis
# Scenario analysis
# 
# Date: 28/1/2020
# By: Deltares (Umit Taner)
################################################################################

# Load required packages
source("./src/R/00 global.R")

# Saving directories
data_dir <- "./data/v7/"
plot_dir <- "./graphics/2020_03/"

# Read_in node and pipe schematic from csv files: 
designNum <- 5
nodes <- read.csv(paste0(data_dir,"nodes.csv"))
pipes <- lapply(1:designNum, function(x) read.csv(paste0(data_dir,"pipes_d", x, ".csv")))

# Base pipe network to generate samples
nodes_base <- nodes
pipes_base <- pipes[[designNum]]
pipeNum    <- nrow(pipes_base)
nodeNum    <- nrow(nodes_base)   
dindex     <- nodes_base$node_id[which(nodes_base$type == "demand")]
sindex     <- nodes_base$node_id[which(nodes_base$type == "supply")]
demandNum  <- length(dindex)
supplyNum  <- length(sindex)

# Capital Expenditure of design alternatives (in million EUROS)
#€1,-/mm/m' --> E.g.: a 275 mm pipe costs 275 € per m'
CAPEX_pipes <- sapply(1:designNum, function(x) sum(pipes[[x]]$length * pipes[[x]]$diameter)/10^6)

total_pipe_lengths <- sapply(1:designNum, function(x) sum(pipes[[x]]$length)) 
                                                   
# set supply as 20% higher than demand
nodes_base[sindex,"discharge"] <- nodes_base[sindex,"discharge"] * 1.10


################################################################################
# 1. EXPLORATORY ANALYSIS ------------------------------------------------------

##### DESIGN 

# 4 Design alternatives
# 10,000 stochastic samples (supply, WQ failures)
# 5 supply levels (130%, 115%, 100%, 85%, 70%)
# 5 demand levels (130%, 115%, 100%, 85%, 70%)
# 3 seasonal variability (low, med, high)

# number of MC samples to be generated
ssize <- 100

###### Shocks (stochastic Parameters)

# parameter 1: Pipe failure
prb_pFail <- 0.05
par_pFail <- replicate(ssize, rbinom(pipeNum, size = 1, prob = prb_pFail))

# parameter 2: Supply Failure
prb_supFail <- 0.05
par_supFail <- replicate(ssize, rbinom(supplyNum, size = 1, prob = prb_supFail))

###### Deterministic Parameters

# parameter 3: Demand change (land-use change)
par_demChange <- seq(0.70, 1.30, length.out = 5)
demandVals <- sapply(par_demChange, function(x) nodes_base$discharge[dindex] * x) 

# parameter 4: Supply availability change (climate change + permits)
par_supChange <- seq(0.70, 1.30, length.out = 5)
supplyVals <- sapply(par_supChange, function(x) nodes_base$discharge[sindex] * x) 

# Scenario matrix
scn_mc   <- tibble(sample = 1: ssize)
scn_fact <- expandGrid(design = 1:designNum, demChange = 1:length(par_demChange), 
                       supChange = 1:length(par_supChange))
scn_mat  <- expandGridDf(scn_fact, scn_mc) %>% mutate(scn_id = 1:n())
scn_num  <- nrow(scn_mat)

### Run model for each scenario
out <- vector(mode = "list", length = scn_num)
outDemand     <- matrix(0, nrow = nodeNum, ncol = scn_num)
outDelivered  <- matrix(0, nrow = nodeNum, ncol = scn_num)
node_df <- nodes_base

pb <- txtProgressBar(min = 1, max = scn_num, style = 3)
start_time <- Sys.time()
for (i in 1:scn_num) {

  # progress bar
  setTxtProgressBar(pb, i)
  
  #Set current design 
  pipe_df <- pipes[[scn_mat$design[i]]] 

  # Supply failures
  node_df$disuse[sindex] <- par_supFail[, scn_mat$sample[i]]      
  
  # pipe failures
  pipe_df$disuse <- par_pFail[1:nrow(pipe_df), scn_mat$sample[i]]  
  
  # Set demand/supply trend parameters
  node_df$discharge[sindex] <- supplyVals[, scn_mat$supChange[i]]   # Set supply change
  node_df$discharge[dindex] <- demandVals[, scn_mat$demChange[i]]   # Set demand increase
  
  #### Calculate water allocation
  out[[i]] <- calculate_network(node_df, pipe_df, 1.0, 1)
  
  #### Demand and delivered water PER node
  outDemand[,i]    <- node_df$discharge 
  outDelivered[,i] <- as.numeric(out[[i]][[2]])

} 
close(pb)
Sys.time() - start_time

### Save results
#save.image("./sessions/scenarios_125000.Rdata")
#save.image("./sessions/scenarios_12500_prSup005.Rdata")

################################################################################
# 2. RESULTS ANALYSIS ----------------------------------------------------------

#load("./sessions/scenarios_125000.Rdata")
load("./sessions/scenarios_12500_prSup005.Rdata")

# Set ggplot2 parameters
ggtheme1 <- theme_light(base_size = 12) 

#### Post-process results
pFails_scn  <- apply(par_pFail,2, sum)
sFails_scn  <- apply(par_supFail,2, sum)

results_network1 <- scn_mat %>% 
  mutate(
   CAPEX    = CAPEX_pipes[as.numeric(as.character(design))],
   pFails   = pFails_scn[sample],
   sFails   = sFails_scn[sample],
   demand   = sapply(1:scn_num, function(x) round(sum(outDemand[dindex, x]),2)),
   delivery = sapply(1:scn_num, function(x) round(sum(outDelivered[dindex, x]),2)),
   rel      = round(delivery/demand* 100, 2),
   fcount   = sapply(1:scn_num, function(x) length(which(outDemand[dindex,x] - outDelivered[dindex,x] > 0.1))),
   design   = factor(design, levels = 1:designNum, labels = paste0("Design", 1:designNum)))
                    
results_network2 <- results_network1 %>% filter(sample %in% 1:100)

### Box-plots of performance metrics accross designs

# Reliability
p1 <- ggplot(results_network1, aes(x = design, y = rel, fill = design, color = design)) + 
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.data = "mean_cl_boot", size = 1, shape = 16, color = "gray20") +
  #geom_jitter(data = results_network2, size=0.8, alpha=0.5, width = 0.2, height = 0.005) +
  ggtheme1 + 
  labs(x = "", y = "Network Reliability (%)") +
  scale_color_viridis(discrete = TRUE, alpha=1) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  guides(fill = FALSE, color = FALSE); p1

ggsave(filename = paste0(plot_dir, "result_rel.png"), height = 4.5, width = 7)

# Failure count
p2 <- ggplot(results_network1, aes(x = design, y = fcount, fill = design, color = design)) + 
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.data = "mean_cl_boot", size = 1, shape = 16, color = "gray20") +
  #geom_jitter(data = results_network2, size=0.8, alpha=0.5, width = 0.2, height = 0.005) +
  ggtheme1 + 
  labs(x = "", y = "Shortage count*") +
  scale_color_viridis(discrete = TRUE, alpha=1) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  guides(fill = FALSE, color = FALSE); p2

ggsave(filename = paste0(plot_dir, "result_fcount.png"), height = 7, width = 7)

############ COST-EFFECTIVENESS PLOTS

# Per demand change
results_network_demChange <- results_network1 %>% 
  gather(key = variable, value = value, rel:fcount) %>%
  group_by(design, variable, demChange) %>% 
  summarize(value = mean(value), CAPEX = mean(CAPEX)) %>%
  mutate(demChange = factor(demChange, levels = 1:5, labels = paste0(seq(-30,+30,15), "%")))

results_network_mean <- results_network_demChange %>% 
  group_by(design, variable) %>% 
  summarize(value = mean(value), CAPEX = mean(CAPEX))

## Reliability
p3 <- ggplot(filter(results_network_mean, variable == "rel"),
             aes(x = CAPEX, y = value, fill = design)) +
  ggtheme1 + 
  scale_fill_viridis(discrete = TRUE, alpha=0.8) +
  scale_y_continuous(limits = c(70,100), breaks = seq(70,100,5)) +
  labs(x = "CAPEX (Million EUR)", y = "Network Reliability (%)", fill = "") +
  geom_line(aes(group = 1), color = "gray20") +
  geom_point(size = 5, shape = 21) +
  guides(color = FALSE) + 
  theme(
    legend.position = c(.95, .05),
    legend.justification = c("right", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(filename = paste0(plot_dir, "result_capex_rel1.png"), height = 4.5, width = 7)

p3b <- p3 +
  geom_line(aes(group = demChange), color = "gray70",
            data = filter(results_network_demChange, variable == "rel")) +
  geom_point(size = 5, shape = 21,
             data = filter(results_network_demChange, variable == "rel")) 

ggsave(filename = paste0(plot_dir, "result_capex_rel2.png"), height = 6, width = 7)


## Failure count
p4 <- ggplot(filter(results_network_mean, variable == "fcount"),
             aes(x = CAPEX, y = value, fill = design)) +
  ggtheme1 + 
  scale_fill_viridis(discrete = TRUE, alpha=0.8) +
  scale_y_continuous(limits = c(0,16), breaks = seq(0,16,2)) +
  
  labs(x = "CAPEX (M.EUR)", y = "Shortage count") +
  geom_line(aes(group = 1), color = "gray20") +
  geom_point(size = 5, shape = 21) +
  guides(color = FALSE)

ggsave(filename = paste0(plot_dir, "result_capex_fcount1.png"), height = 7, width = 8)

p4b <- p4 +
  geom_line(aes(group = demChange), color = "gray70",
            data = filter(results_network_demChange, variable == "fcount")) +
  geom_point(size = 5, shape = 21,
             data = filter(results_network_demChange, variable == "fcount")) 

ggsave(filename = paste0(plot_dir, "result_capex_fcount2.png"), height = 7, width = 8)





### Spatial analysis 

psp1 <- list(); psp2 <- list()

# Count Failures per node
results_node <- nodes %>% select(node_id, type, name, lat, lon) %>%
  #filter(type == "demand") %>% 
  expandGridDf(tibble(design = 1:designNum)) %>%
  select(design, type, node_id) %>%
  mutate(failRate = NA, rel = NA, demand = NA)

for (i in 1:nrow(results_node)) {
  
  nodeC <- results_node$node_id[i]
  scnC  <- scn_mat$scn_id[which(scn_mat$design == results_node$design[i] )]
  
  results_node$failRate[i] <- length(which(outDemand[nodeC,scnC] - outDelivered[nodeC,scnC] > 0.1)) / length(scnC) * 100
  results_node$rel[i] <- sum(outDelivered[nodeC,scnC]) / sum(outDemand[nodeC,scnC])*100
  results_node$demand[i] <- mean(outDemand[nodeC,scnC])
}

results_node$failRate[which(results_node$type == "supply")] <- NA
results_node$demand[which(results_node$type == "supply")] <- NA
results_node$rel[which(results_node$type == "supply")] <- NA

metric_range <- range(results_node$rel, na.rm = TRUE)  

for (x in 1:designNum) { 

 # Set node output table
 results_node2 <- results_node %>% filter(design == x) %>% select(-type)
 nodeDF_out <- nodesDF %>% left_join(results_node2, by = "node_id") 

 # Set pipe network
 pipesDF_out <- pipes[[x]] %>%
   left_join(y = (nodeDF_out %>% select(start = node_id, x, y))) %>%
   left_join(y = (nodeDF_out %>% select(end = node_id, xend = x, yend = y)))

# Plot current design
psp1[[x]] <- ggmap(background.map, extend = "device") + 
  theme_light() + 
  scale_fill_distiller(palette = "Reds", direction = 1, limits = c(0, 51), breaks = seq(0,50,10)) +
  scale_size(range = c(2, 12)) +
  labs(x = xlab, y = ylab, fill = "Shortage\nocurrance (%)") +
  guides(color = FALSE, size = FALSE, fill = guide_colourbar(barwidth = 1, barheight = 15)) +
  ggtitle(paste0("Design: ", x)) +
  geom_edges(aes(x=x, y=y, xend = xend, yend=yend, color = colid), data=pipesDF_out, size=1) +
  geom_nodes(aes(x=x, y=y), filter(nodeDF_out, type == "supply"), size = 5, stroke = 1.2, shape = 21, color ="gray30") +
  geom_nodes(aes(x=x, y=y, fill = failRate, size = demand), filter(nodeDF_out, type == "demand"), stroke = 1.2, shape = 21, color ="gray30") +
  geom_text(aes(x=x, y=y, label = node_id), data = nodesDF) 

ggsave(filename = paste0(plot_dir, "nw_shortages_design",x ,".png"), width = 9, height = 7)


# Plot current design
psp2 <- ggmap(background.map, extend = "device") + 
  theme_light() + 
  scale_fill_distiller(palette = "Reds", direction = -1, limits = c(50, 100), breaks = seq(50,100,10)) +
  scale_size(range = c(2, 12)) +
  labs(x = xlab, y = ylab, fill = "Reliability (%)") +
  guides(color = FALSE, size = FALSE, fill = guide_colourbar(barwidth = 1, barheight = 15)) +
  ggtitle(paste0("Design: ", x)) +
  geom_edges(aes(x=x, y=y, xend = xend, yend=yend, color = colid), data=pipesDF_out, size=1) +
  geom_nodes(aes(x=x, y=y), filter(nodeDF_out, type == "supply"), size = 5, stroke = 1.2, shape = 21, color ="gray30") +
  geom_nodes(aes(x=x, y=y, fill = rel, size = demand), filter(nodeDF_out, type == "demand"), stroke = 1.2, shape = 21, color ="gray30") +
  geom_text(aes(x=x, y=y, label = node_id), data = nodesDF) 

ggsave(filename = paste0(plot_dir, "nw_rel_design",x ,".png"), width = 9, height = 7)

}


#### Global sensitivity analysis ########################

# Demand change
# parameter 3: Demand change (land-use change)
par_demChange <- seq(0.70, 1.30, length.out = 5)
demandVals <- sapply(par_demChange, function(x) nodes_base$discharge[dindex] * x) 

# Sample analysis
sample_stats <- scn_mc %>%
  mutate(pFails = as.factor(apply(par_pFail,2, sum))) %>%
  mutate(sFails = as.factor(apply(par_supFail,2, sum)))

ggplot(data = sample_stats, aes(pFails)) + ggtheme1 + 
  geom_histogram(stat = "count", fill = "steelblue") +
  labs(x = "simultaneous pipe failures", y = "frequency")

ggsave(filename = paste0(plot_dir, "sample_pfail.png"))

ggplot(data = sample_stats, aes(sFails)) + ggtheme1 + 
  geom_histogram(stat = "count", fill = "steelblue") +
  labs(x = "simultaneous supply failures", y = "frequency")

ggsave(filename = paste0(plot_dir, "sample_sfail.png"))


