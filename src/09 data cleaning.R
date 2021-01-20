
source("./R/00 initialize.R")

################################################################################
# Demand/Supply value estimation  ##############################################
################################################################################

# 39 sub areas in Friesland (from Vitens)
subareas_dat  <- read_csv("./data/v2/subareas_friesland.csv")

# Water consumption data per consumer in Friesland (from Vitens)
consumers_dat <- read_csv("./data/v2/consumers_friesland.csv")

cmy2cmh <- 1/(365*24)


# Water consumption aggregated to 39 sub-areas
consumers <- consumers_dat %>%
  group_by(Gebiedsnr) %>%
  summarize(value = round(sum(Normalised), 2)) %>%
  left_join(select(subareas_dat, Gebiedsnr, Gebiedsnaa), by = "Gebiedsnr") %>%
  select(1, 2, name = 3) 
consumers[39,1] <- "FRL-05-03"
consumers[39,3] <- "Holwerd"

# Initial node table for Friesland
friesland_nodes <- read.csv("./data/v3/calculations/network3_nodes_ini2.csv") 
friesland_nodes_dat <- friesland_nodes %>%
  left_join(consumers, by = "name") 

friesland_nodes_dat[which(friesland_nodes_dat$type == "supply"),]$Gebiedsnr <- NA
friesland_nodes_dat[which(friesland_nodes_dat$type == "supply"),]$discharge <- NA

friesland_nodes_dat$value[40] <- sum(friesland_nodes_dat$value[c(6)]) 
friesland_nodes_dat$value[41] <- sum(friesland_nodes_dat$value[c(20,21, 22, 24, 23 ,25, 19, 27, 28, 29)])
friesland_nodes_dat$value[42] <- sum(friesland_nodes_dat$value[c(4)])
friesland_nodes_dat$value[43] <- sum(friesland_nodes_dat$value[c(5)])
friesland_nodes_dat$value[44] <- sum(friesland_nodes_dat$value[c(38,39)])
friesland_nodes_dat$value[45] <- sum(friesland_nodes_dat$value[c(3,2)])
friesland_nodes_dat$value[46] <- sum(friesland_nodes_dat$value[c(16, 17, 18, 15, 13, 14, 8, 9, 10, 7, 11, 12, 35, 36)])
friesland_nodes_dat$value[47] <- sum(friesland_nodes_dat$value[c(30,31,34,26,33,32,37)])
friesland_nodes_dat$value[48] <- sum(friesland_nodes_dat$value[c(1)])

#Check total
friesland_nodes_dat %<>% mutate(discharge = value) %>% select(-value)
friesland_nodes_dat %>% group_by(type) %>% summarize(discharge = sum(discharge))

friesland_nodes <- friesland_nodes_dat %>% 
  select(1, 2, 3, 4, 5, 6, 7, ind_ratio = industry_ratio, name, Gebiedsnr, lat, lon) %>%
  mutate(discharge = round(discharge * cmy2cmh,2))

write_csv(friesland_nodes, "./data/v3/network3_nodes.csv")


################################################################################
# Pipe Calculations ############################################################
################################################################################

# Merge pipe length values (in meters)

node_data  <- read.csv("./data/v3/network3_nodes.csv")
node_dist_data  <- read_csv("./data/v3/calculations/network3_node_distances.csv")

node_dist <- node_dist_data %>% select(-type, -x,-y) %>% rename(start = node_id) %>%
  gather(key = end, value = value, -start) %>% mutate(end = as.numeric(end))

pipe_data1  <- read.csv("./data/v3/network3_design1.csv") %>%
  left_join(node_dist, by = c("start", "end")) %>%
  mutate(length = round(value,0)) %>% select(-value)
write_csv(pipe_data1, "./data/v3/network3_design1.csv")

pipe_data2  <- read.csv("./data/v3/network3_design2.csv") %>%
  left_join(node_dist, by = c("start", "end")) %>%
  mutate(length = round(value,0)) %>% select(-value)
write_csv(pipe_data2, "./data/v3/network3_design2.csv")

pipe_data3  <- read.csv("./data/v3/network3_design3.csv") %>%
  left_join(node_dist, by = c("start", "end")) %>%
  mutate(length = round(value,0)) %>% select(-value)
write_csv(pipe_data3, "./data/v3/network3_design3.csv")

pipe_data4 <- read.csv("./data/v3/network3_design4.csv") %>%
  left_join(node_dist, by = c("start", "end")) %>%
  mutate(length = round(value,0)) %>% select(-value)
write_csv(pipe_data4, "./data/v3/network3_design4.csv")


################# pipe diameters

# Generate network

pipes <- read_csv("./data/v3/calculations/network3_design1_directed.csv") %>% select(from = start, to = end, length:colid)
nodes <- designs[[1]]$nodes %>% select(name = node_id, type:disuse, lat, lon)
net <- graph_from_data_frame(d=pipes, vertices=nodes, directed = T) 

pipe_dimensions <- read_csv("./data/v3/calculations/pipe_dimensioning.csv")

pipe_num <- nrow(pipes)
node_num <- nrow(nodes)

for (i in 1:node_num) {
  
  nodeC <- nodes[i,] %>% pull(name)
  nodesUp <- as.numeric(subcomponent(net, nodeC, "out"))
  qcap <- sum(nodes$discharge[nodesUp])
  
  pipes[which(pipes$to == nodeC), "qmax"] <- qcap
  
}

pipes$diameter <- sapply(pipes$qmax, function(x) pipe_dimensions[which.min(abs(x - pipe_dimensions$maxflow_cmh)),]$diameter_mm)

write_csv(pipes, "./data/v3/pipes_design1.csv")



################################################################################
# CONVERT COORDINATES ##########################################################
################################################################################

library(sp)

#Read-in vitens data
vitens_df <- read.csv("./data/v2/production_sites.csv")

#Convert to lat/long coordinates
d <- data.frame(lon=vitens_df$X, lat=vitens_df$Y)
coordinates(d) <- c("lon", "lat")
proj4string(d) <- CRS("+init=epsg:28992 +datum=WGS84") #EPSG:28992 Amersfoort / RD New
d.new <- spTransform(d, "+proj=longlat +datum=WGS84")
d.new_df <- as.data.frame(d.new)

Vitens_df2 <- vitens_df %>%
  mutate(lat = d.new_df$lat) %>%
  mutate(lon = d.new_df$lon)

write_csv(Vitens_df2, "./data/v2/production_sites_v2.csv")


#register_google(key = "AIzaSyDSykq5fNI-H89WdwRINMjDrcq5lbK0vk8")
#s <- "element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative%7Celement:geometry%7Cvisibility:off&style=feature:administrative.country%7Celement:geometry.stroke%7Ccolor:0x000000%7Cvisibility:on&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Cvisibility:off&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road%7Celement:labels.icon%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit%7Cvisibility:off&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&size=480x360"
#mapFriesland <- get_googlemap(c(5.7, 53.2), zoom = 9, style = s)
#save(mapFriesland, file="./data/mapFriesland.rda")


