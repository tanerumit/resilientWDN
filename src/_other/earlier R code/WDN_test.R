
################################################################################
# Drinking Water System Resilience Analysis
# WDN module testing
# 
# Date: 28/1/2020
# By: Deltares (Umit Taner)
################################################################################

# Packages required
library(reticulate)
library(readr)
library(R.utils)
library(dplyr)
library(tidyr)

# Specify the virtual python environment
use_miniconda(condaenv = "myEnv", required = TRUE)

# Source python script 
source_python("./Python/wdn_module_v5.py")

# Read_in node and pipe information: 
node_info <- read.csv("./data/vitens_nodes_dummy_v5.csv")
pipe_info <- read.csv("./data/vitens_pipes_dummy_v5.csv")

node_df <- node_info
pipe_df <- pipe_info

output <- calculate_network(nodes = node_df, 
                            pipes = pipe_df, 
                            penalty_order = 1.0, 
                            head_loss_option = 3)

node_df2 <- node_df %>% select(node_id, type, discharge, disuse)
node_df2$value <- as.numeric(output[[2]])
node_df2$deficit <- node_df2$discharge - node_df2$value
  
  
# Check mass balance
supplyn_ind <- which(node_df$type == "supply")
demandn_ind <- which(node_df$type == "demand") 

supplyn_num <- length(supplyn_ind)
demandn_num <- length(demandn_ind)

for (x in 1:supplyn_num) {
  
  
  
  
}








nodesDF <- vitens_nodes %>% select(7,1,2,3:6, y= 8, x = 9) 

node_results <- nodesDF
node_results$shortage <- NA
node_results$shortage[node_df$type == "demand"] <- as.numeric(output[[3]])
node_results$display <- paste0(node_results$demand, "/", node_results$shortage)
node_results$display[node_df$type != "demand"] <- node_results$demand[node_df$type != "demand"] 

p2 +  geom_text_repel(aes(x=x, y=y, label=display), data = node_results, box.padding = 0.5, color="red")

# Attached is a script that does all three versions (q only, linear head loss, and mixed-integer). The main function you will have to call has the following signature:
#   def calculate_network(nodes_csv: str = 'nodes.csv',
#                         pipes_csv: str = 'pipes.csv',
#                         penalty_order: float = 1.0,
#                         head_loss_option: HeadLossOption = HeadLossOption.Q_ONLY):
#   
#   so nodes_csv should be a string, and by default it has a value “nodes.csv”
# The penalty order can be used to make sure shortages are spread out by setting it to 2.0. That only works for head loss option 1 and 2 (see below)
# 
# Head loss option is what method you want to use:
# 1 = discharge only
# 2 = linear head loss equation
# 3 = mixed integer problem with non-linear (more accurate) head loss equation.
# 
# Maybe I can figure out something better for option 3 in the future, but for now I think this will do.
# 
# The return value is as you want, but not raw objective value (which can change based on the penalty order). Instead I just calculated the total shortage, and return that. So it’s:
#   list(total_shortage, Map[node_id, shortage], Map[pipe_id, discharge])
