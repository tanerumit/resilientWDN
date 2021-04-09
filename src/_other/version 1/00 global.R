

############## PACKAGES ########################################################

# Network analysis  
library(igraph); library(networktools)

# Data visualization
require(ggplot2); require(cowplot); require(ggmap); 
require(ggnetwork); require(viridis)

# Data analysis
library(readr); library(lubridate); library(R.utils); library(dplyr) 
library(tidyr); library(hydrosystems)

############## PYTHON SETUP ####################################################

#Use reticulate package to run python code from R

# Specify the virtual python environment
reticulate::use_miniconda(condaenv = "resilientWDN", required = TRUE)
reticulate::source_python("./src/py/wdn_module_v8.py")



############# GGPLOT settings ##################################################

# network theme
ggtheme_nw <- theme_light() + 
  theme(legend.background = element_blank()) +
  theme(legend.key=element_blank()) +
  theme(legend.position = c(0.08, 0.92))