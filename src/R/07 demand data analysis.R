
################################################################################
# VITENS Residential demand data analysis
################################################################################

library(hydrosystems)
library(tidyverse)
library(readr)
library(lubridate)
library(stringi)
library(magrittr)
library(ggcorrplot)
library(scales)

plotsDir <- "./output/graphics/demand/"

# Hourly demand data for the Friesland area
friesland_nodes <- read_csv("./data/v8/node_names.csv")
friesland_demand_ts <- read_csv("./data/raw data/watergebruik 2019_2020_serviceareas_edited.csv") %>%
  mutate(index = index + hours(2)) 

# Demand data in tidy format
friesland_demand_tidy <- friesland_demand_ts %>%
  gather(key = name, value = value, -index) %>%
  mutate(year  = year(index) %>% as.factor(),
         mon = month(index) %>% factor(level = 1:12),
         day = day(index)   %>% as.factor(),
         hour = hour(index) %>% as.factor()) %>%
  select(year, mon, day, hour, everything(), index) %>%
  left_join(friesland_nodes, by = "name")

# Remove bad data
friesland_demand_tidy %<>% 
  mutate(value = ifelse(value < 0, 0, value)) %>%
  filter(value < 1e5) %>%
  filter(name != "Franeker")

# Hourly data
friesland_demand_tidy_hr  <- friesland_demand_tidy 

# Daily data
friesland_demand_tidy_day <- friesland_demand_tidy %>% 
  group_by(name, id, year, mon, day) %>% summarize(value = mean(value, na.rm = TRUE)) 

# Monthly data
friesland_demand_tidy_mon <- friesland_demand_tidy %>% 
  group_by(name, id, year, mon) %>% summarize(value = mean(value, na.rm = TRUE)) 

# Yearly data
friesland_demand_tidy_yr  <- friesland_demand_tidy %>% 
  group_by(name, id, year) %>% summarize(value = mean(value, na.rm = TRUE)) 
  
df_hr  <- friesland_demand_tidy_hr  %>% 
  mutate(loc = ifelse(id %in% 1:6, "Island", "Mainland")) %>%
  #mutate(loc = ifelse(id %in% 1:6, "Island", ifelse(id == 30, "Munnekeburen", "Mainland"))) %>%
  unite("name", id, name, sep = ".")
df_day <- friesland_demand_tidy_day %>% 
  mutate(loc = ifelse(id %in% 1:6, "Island", "Mainland")) %>%
  #mutate(loc = ifelse(id %in% 1:6, "Island", ifelse(id == 30, "Munnekeburen", "Mainland"))) %>%
  unite("name", id, name, sep = ".")   
df_mon <- friesland_demand_tidy_mon  %>% 
  mutate(loc = ifelse(id %in% 1:6, "Island", "Mainland")) %>%
  #mutate(loc = ifelse(id %in% 1:6, "Island", ifelse(id == 30, "Munnekeburen", "Mainland"))) %>%
  unite("name", id, name, sep = ".") 
  

################################################################################

# 1) Analysis of annual means -------------------------------------------------- 


df1 <- friesland_demand_tidy_mon %>%
  group_by(name) %>% 
  summarize(value = sum(value)) %>%
  mutate(value = value/sum(value) * 100) %>%
  left_join(friesland_nodes, by = "name") %>%
  select(id, name, value) %>%
  unite(index, id:name, remove = FALSE)

p <- ggplot(df1, aes(x=reorder(index, value), y = value)) + 
  geom_bar(aes(fill = value), stat = "identity") + 
  scale_fill_distiller(palette = "Reds", direction = 1, limits = c(0, 16), breaks = seq(0,16,4)) +
  theme_light(base_size = 14) +
  guides(fill = FALSE) +
  labs(x="", y = "MCM") +
  coord_flip() 

ggsave(paste0(plotsDir, "subbalance consumption.png"), height=7, width=4) 


################################################################################

# Correlation analysis --------------------------------------------------------- 

var_name  <- "value"
var_color <- "steelblue"

# Hourly
cor_df_hr <- df_hr %>% ungroup() %>%  
  select(name, var = all_of(var_name), year, mon, day, hour) %>% 
  spread(key = name, value = var) %>%
  select(-year, -mon, -day, -hour) %>%
  cor(., use="pairwise.complete.obs")  

# Daily
cor_df_day <- df_day %>% ungroup() %>%  
  select(name, var = all_of(var_name), year, mon, day) %>% 
  spread(key = name, value = var) %>%
  select(-year, -mon, -day) %>%
  cor(., use="pairwise.complete.obs")  

# Monthly
cor_df_mon <- df_mon %>% ungroup() %>%  
  select(name, var = all_of(var_name), year, mon) %>% 
  spread(key = name, value = var) %>%
  select(-year, -mon) %>%
  cor(., use="pairwise.complete.obs")

cor_data <- cor_df_mon
p.mat <- cor_pmat(cor_data, method = "spearman")

p1 <- ggcorrplot(corr = cor_data, 
                 method = "circle",
                 outline.col = "white", 
                 colors = c("tomato2", "white", var_color),
                 ggtheme = theme_minimal(base_size = 10), 
                 type = "upper",
                 hc.order = TRUE,
                 p.mat = p.mat) +
      scale_fill_gradient2(
        limit=c(-1, 1), midpoint = 0, guide = "colourbar",
        low = muted("red"), mid = "white", high = muted("blue")) +
  theme(legend.position = "none")

ggsave(paste0(plotsDir, "demand_corrmatrix_mon.png"), height=9, width=9) 


################################################################################

# Seasonal variability ---------------------------------------------------------

df <- df_day %>%
  group_by(loc,mon,day) %>%
  summarize(value = sum(value)) %>%
  group_by(loc) %>%
  mutate(norm = value/mean(value)) %>%
  filter(norm < 10) 
  
### Per service area cluster
p1 <- ggplot(df, aes(x=mon, y = norm, group = loc)) +
  theme_light(base_size = 14) +
  geom_jitter(position=position_jitter(0.3), alpha = 0.4, size = 1.8, color = "gray20") +
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.8, color = "red") +
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), 
               geom="pointrange", color = "darkblue", size = 1) +
  facet_wrap(loc ~ ., scales = "free_y", ncol = 1) +
  labs(x = "Month", y = "Normalized consumption")
  
ggsave(paste0(plotsDir, "demand_variation_seasonal.png"), height=8, width=8) 

## Summarize norm values:
df2 <- df %>% group_by(loc, mon) %>%
  summarize(min = min(norm), q50 = quantile(norm, 0.50), q95 = quantile(norm, 0.95), max = max(norm)) %>%
  ungroup() %>%
  gather(key = variable, value = value, min:max) %>% 
  spread(key = mon, value = value)

write_csv(df2, "seasonal_ranges.csv")

################################################################################

# Within-day variations --------------------------------------------------------

df <- df_hr %>%
  select(mon, hour, day, loc, name, value) %>%
  group_by(loc, mon, day, hour) %>%
  summarize(value = sum(value)) %>%
  group_by(loc) %>%
  mutate(norm = value/mean(value)) %>%
  filter(((loc == "Island") & (norm < 50)) | ((loc == "Mainland") & (norm < 3)))


### Per service area cluster
p1 <- ggplot(df, aes(x=hour, y = norm, group = loc)) +
  theme_light(base_size = 14) +
  geom_jitter(position=position_jitter(0.3), alpha = 0.2, size = 1.5, color = "gray20") +
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.8, color = "red") +
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), 
               geom="pointrange", color = "darkblue", size = 1) +
  facet_wrap(loc ~ ., ncol = 1) +
  labs(x = "Hour", y = "Normalized consumption")

ggsave(paste0(plotsDir, "demand_variation_hourly.png"), height=8, width=8) 











#Aggregated
df2 <- demand_tidy_df %>% mutate(day = day(index)) %>%


p2 <- ggplot(df2, aes(x=month, y = value)) +
  theme_light() +
  geom_jitter(position=position_jitter(0.2), alpha = 0.5, size = 1, color = "gray50") +
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), 
               geom="pointrange", color = "darkred", size = 1) +
  labs(x = "month", y = " consumption (m3/hr)")

ggsave("./graphics/demand/demand_seasonal2.png", width = 8, height = 5)



### Variation by hour 
df3 <- demand_series_hourly_raw %>%
  gather(key = loc, value = value, -index) %>%
  group_by(index, loc) %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  mutate(month = as.factor(month(index)), hour = as.factor(hour(index))) %>%
  mutate(value = ifelse(value < 0, NA, value)) %>%
  mutate(value = ifelse(value > 20000, NA, value)) %>%
  drop_na()

p3 <- ggplot(df3, aes(x=hour, y = value/1000)) +
  theme_light() +
  geom_jitter(position=position_jitter(0.2), alpha = 0.3, size = 0.8, color = "gray50") +
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), 
               geom="pointrange", color = "darkred", size = 0.8) +
  labs(x = "hour of the day (0-23)", y = " consumption (m3/hr)") +
  facet_wrap(loc ~ ., scales = "free_y", ncol = 3) 

ggsave("./graphics/demand/demand_var_hourly1.png", width = 10, height = 14)






p5b <- ggplot(demand_agg_df2, aes(x=month, y = value/1000)) +
  theme_light() +
  geom_jitter(position=position_jitter(0.2), alpha = 0.5, size = 1.2, color = "gray50") +
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), 
               geom="pointrange", color = "darkred", size = 1.2) +
  labs(x = "month", y = " consumption (m3/hr)")

ggsave("./graphics/demand/demand_season_var.png", width = 8, height = 5)


df2 <- df %>%
  arrange(value) %>%
  mutate(n=1:n())
  
  group_by(hour) %>%
  group_by(month) %>% 
  summarize(mean = value(mean))
  
  
  summarize(value = mean(value, na.rm = TRUE))



peak_hrs <- c(9,10,11,12, 18,19,20)


unique(demand_agg_df$hour)

  
  group_by(loc) %>%
  mutate(normval = value/mean(value, na.rm = TRUE))

demand_agg_df <- demand_df %>%
  


p3 <- ggplot(demand_df, aes(x = month, y = value)) +
  facet_wrap(~ loc, scales = "free", ncol = 2) +
  geom_boxplot(aes(group = month))

ggsave("./graphics/demand/demand_seasonal_abs.png", width = 8, height = 12)

p4 <- ggplot(demand_df, aes(x = month, y = normval)) +
  facet_wrap(~ loc, scales = "free", ncol = 2) +
  geom_boxplot(aes(group = month)) + 
  stat_summary(aes(group = loc), fun=mean, colour="darkred", geom="line", size= 1.2, show.legend = FALSE) 

ggsave("./graphics/demand/demand_seasonal_norm.png", width = 8, height = 16)


p5 <- ggplot(demand_df, aes(x = month, y = normval)) +
  facet_wrap(~ loc, scales = "free", ncol = 2) +
  #geom_jitter(position=position_jitter(0.2)) + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), 
               geom="pointrange", color = "red")
  
ggsave("./graphics/demand/demand_seasonal_norm2.png", width = 8, height = 16)






################################################################################



