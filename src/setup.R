

############## PACKAGES  -------------------------------------------------------

# Network analysis  
library(igraph) 
library(networktools)

# Data visualization
require(ggplot2)
require(ggmap) 
require(ggnetwork)

# Data analysis
library(readr) 
library(lubridate) 
library(dplyr) 
library(tidyr) 

setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
setSessionTimeLimit(cpu = Inf, elapsed = Inf)

#library(hydrosystems)
#require(cowplot);
#require(viridis)

############## PYTHON SETUP ---------------------------------------------------- 

#Use reticulate package to run python code from R

# Specify the virtual python environment
reticulate::use_miniconda(condaenv = "resilientWDN", required = TRUE)
reticulate::source_python("./src/functions/calculate_network2.py")



############# Functions & Other settings ---------------------------------------

source("./src/functions/simulateWDN.R")

source("./src/functions/visualizeWDN.R")

# Set ggplot2 theme
ggtheme_nw <- theme_light() + 
  theme(legend.background = element_rect(fill = "white")) + # element_blank()) +
  theme(legend.key=element_blank()) #+
  #theme(legend.position = c(0.085, 0.15)) 



# Find paths from node index n to m using adjacency list a.
adjlist_find_paths <- function(a, n, m, path = list()) {
  path <- c(path, list(n))
  if (n == m) {
    return(list(path))
  } else {
    paths = list()
    for (child in a[[n]]) {
      if (!child %in% unlist(path)) {
        child_paths <- adjlist_find_paths(a, child, m, path)
        paths <- c(paths, child_paths)
      }
    }
    return(paths)
  }
}

# Find paths in graph from vertex source to vertex dest.
paths_from_to <- function(graph, source, dest) {
  a <- as_adj_list(graph, mode = "out")
  paths <- adjlist_find_paths(a, source, dest)
  lapply(paths, function(path) {V(graph)[unlist(path)]})
}

