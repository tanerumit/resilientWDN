

############## PACKAGES  -------------------------------------------------------

# Network analysis  
library(igraph) 
library(networktools)

# Data visualization
require(ggplot2)
require(ggmap) 
require(ggnetwork)
require(ggpmisc)
require(ggnewscale)
require(scales)
require(viridis)
require(cowplot)

# spatial analysis
require(geosphere)

# Data load/transfer
require(readr) 
require(readxl)

# Data analysis
require(lubridate) 
require(dplyr) 
require(tibble)
require(tidyr) 
require(magrittr)

setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
setSessionTimeLimit(cpu = Inf, elapsed = Inf)

#library(hydrosystems)
#require(cowplot);
#require(viridis)

############## PYTHON SETUP ---------------------------------------------------- 

#Use reticulate package to run python code from R

# Specify the virtual python environment
reticulate::use_virtualenv(virtualenv = "C:/Users/taner/OneDrive - Stichting Deltares/_WS/GITHUB/resilientWDN/venv", required = TRUE)
#reticulate::use_miniconda(condaenv = "resilientWDN", required = TRUE)
reticulate::source_python("./src/functions/calculate_network2.py")


############# Functions & Other settings ---------------------------------------

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

expandGridDf <- function (...) 
{
  Reduce(function(...) merge(..., by = NULL), list(...)) %>% 
    as_tibble()
}
