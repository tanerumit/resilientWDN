

################################################################################
# SETUP SCRIPT FOR THE STOCHASTIC WEATHER GENERATOR
# Last updated: February 18, 2019
# By: M. Umit Taner
################################################################################ 


### R PACKAGES NEEDED ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# FROM CRAN
library(tidyverse); library(magrittr);  library(dplyr);     library(tidyr); 
library(grid);      library(gridExtra); library(lubridate); library(Kendall); 
library(qqplotr);   library(ggridges);  library(viridis);   library(forecast); 
library(readxl);    library(foreign);   library(ggfortify); library(readr);
library(scales);    library(foreach);   library(doParallel); library(ggsci);
library(akima);     library(cowplot);   library(mvtnorm)
library(igraph); library(magrittr)

# FROM GITHUB
#devtools::install_github('tanerumit/hydrosystems')
library(hydrosystems)
#devtools::install_github('tanerumit/ggHydro')
library(ggHydro)

### GENERAL SETTINGS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Surpress notifications for readr
options(readr.num_columns = 0)

## ggplot settings
theme_set(theme_light())


# installing/loading the package:
#if(!require(installr)) {
#  install.packages("installr"); require(installr)} #load / install+load installr

# using the package:
#updateR() # this will start the updating process of your R installation.  It will check for newer versions, and if one is available, will guide you through the decisions you'd need to make.