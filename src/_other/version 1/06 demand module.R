
################################################################################
# VITENS Demand Module 
################################################################################

library(hydrosystems)
library(tidyverse)
library(readr)

# Calculation steps

# 1) Calculate per capita residential demand as a function of price and ET (global)
# 2) Calculate total population in each service region (per node)
# 3) Calculate total demand as population x demand (per node)
# 4) Seasonal range (per node)


# 1. Per capita water consumption ----------------------------------------------

### Elasticity of water demand
# ET elasticity: 0.1 / 0.075
# Price elasticity: -0.13 / -0.63 


df <- expandGrid(price = seq(0,30,5), ET = seq(0,60,10)) %>%
  mutate(demand =  ET * 0.075 - price * 0.63)

ggplot(df,aes(x=price, y=ET, fill=demand)) +
  theme_light(base_size = 14) +
  geom_tile(color = "gray80") +
  labs(x = "Water price change (%)", y = "Summer ET change (%)", fill = "PCD\nchange(%)") +
  scale_fill_gradient2(low = "#1b7837", high = "#762a83", mid = "white", midpoint = 0) +
  scale_x_continuous(breaks = seq(0,30,5), expand =c(0,0)) +
  scale_y_continuous(breaks = seq(0,60,10), expand =c(0,0))

ggsave("./graphics/demand/sensi_elasticities.png", height = 5, width = 6)

# Sensitivity analysis (price)
prc_e_levs <- seq(from = -0.65, to = -0.10, by = .05)
prc_c_levs <- seq(from = 0, to = 0.25, by = .05)
price_sensi_df1 <- expandGrid(prc_c = prc_c_levs, prc_e = prc_e_levs) 

price_sensi_df1$demand <- sapply(1:nrow(price_sensi_df1), 
                                 function(x) pcdModel(base = 49.5, 
                                                      prc_c = price_sensi_df1$prc_c[x], 
                                                      prc_e = price_sensi_df1$prc_e[x] , 
                                                      ET_c = 0, 
                                                      ET_e = 0))




### Demands m3 per capita per year
# NL (2009-2011) 47.5 - 48.1 
# Vitens 49.434 

### Water price (euro/m3)
# NL (2009-2011) 1.37 - 1.41
# Vitens 1.31  

base_pcd <- 49.50
base_price <- 1.31 

### Elasticity of water demand
# ET elasticity: 0.1 / 0.075
# Price elasticity: -0.13 / -0.63 

pcdModel <- function(base, prc_c, prc_e, ET_c, ET_e) {
    return(exp(1)^(log(base) + prc_c * prc_e + ET_c * ET_e))
}

proj_demand <- pcdModel(base_pcd, prc_c = 0.2, prc_e = -0.63, ET_c = 0.1, ET_e= 0.10)
proj_demand <- pcdModel(base_pcd, prc_c = 0, prc_e = -0.63, ET_c = 0, ET_e= 0.10)



#---------------------------------------------------------------------------

# Calculation steps

# 1) Calculate per capita residential demand as a function of price and ET (global)
# 2) Calculate total population in each service region (per node)
# 3) Calculate total demand as population x demand (per node)
# 4) Seasonal range (per node)


# 1. Per capita water consumption ----------------------------------------------

### Demands m3 per capita per year
# NL (2009-2011) 47.5 - 48.1 
# Vitens 49.434 

### Water price (euro/m3)
# NL (2009-2011) 1.37 - 1.41
# Vitens 1.31  

base_pcd <- 49.50
base_price <- 1.31 

### Elasticity of water demand
# ET elasticity: 0.1 / 0.075
# Price elasticity: -0.13 / -0.63 

pcdModel <- function(base, prc_c, prc_e, ET_c, ET_e) {
  return(exp(1)^(log(base) + prc_c * prc_e + ET_c * ET_e))
}

proj_demand <- pcdModel(base_pcd, prc_c = 0.2, prc_e = -0.63, ET_c = 0.1, ET_e= 0.10)
proj_demand <- pcdModel(base_pcd, prc_c = 0, prc_e = -0.63, ET_c = 0, ET_e= 0.10)

# Sensitivity analysis (price)
prc_e_levs <- seq(from = -0.65, to = -0.10, by = .05)
prc_c_levs <- seq(from = 0, to = 0.25, by = .05)
price_sensi_df1 <- expandGrid(prc_c = prc_c_levs, prc_e = prc_e_levs) 

price_sensi_df1$demand <- sapply(1:nrow(price_sensi_df1), 
                                 function(x) pcdModel(base = 49.5, 
                                                      prc_c = price_sensi_df1$prc_c[x], 
                                                      prc_e = price_sensi_df1$prc_e[x] , 
                                                      ET_c = 0, 
                                                      ET_e = 0))

# Uncertainty range
price_sensi_df2 <- price_sensi_df1 %>% 
  mutate(prc_c = prc_c * 100) %>%
  mutate(demand = demand / base_pcd * 100 - 100) %>%
  group_by(prc_c) %>%
  summarize(min = min(demand), mean = mean(demand), max=max(demand)) 

p1 <- ggplot(price_sensi_df2, aes(x= prc_c)) +
  theme_light() +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y=mean), color = "steelblue") +
  labs(x = "Price change (%)", y = "Demand change (%)")

ggsave("./graphics/demand/price_sensi.png", width = 8, height = 4)


# Sensitivity analysis (evaptranspiration)
ET_e_levs <- seq(from = 0.075, to = 0.10, by = .005)
ET_c_levs <- seq(from = 0, to = 0.25, by = .05)
evap_sensi_df1 <- expandGrid(ET_c = ET_c_levs, ET_e = ET_e_levs) 

evap_sensi_df1$demand <- sapply(1:nrow(evap_sensi_df1), 
                                function(x) pcdModel(base = 49.5, 
                                                     prc_c = 0, 
                                                     prc_e = 0, 
                                                     ET_c = evap_sensi_df1$ET_c[x], 
                                                     ET_e = evap_sensi_df1$ET_e[x]))

# Uncertainty range
evap_sensi_df2 <- evap_sensi_df1 %>% 
  mutate(demand = demand / base_pcd * 100 - 100) %>%
  mutate(ET_c = ET_c * 100) %>%
  group_by(ET_c) %>%
  summarize(min = min(demand), mean = mean(demand), max=max(demand)) 

p2 <- ggplot(evap_sensi_df2, aes(x= ET_c)) +
  theme_light() +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y=mean), color = "steelblue") +
  labs(x = "Evap change (%)", y = "Demand change (%)")

ggsave("./graphics/demand/evap_sensi.png", width = 8, height = 4)




#################################################################################
