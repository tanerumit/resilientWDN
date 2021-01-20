# #Calculate the driving distances between each supply and demand node
# require(gmapsdistance)
# set.api.key("AIzaSyDSykq5fNI-H89WdwRINMjDrcq5lbK0vk8")
# 
# Factories <- as.character(initial_data[!(initial_data$SupplyNodes)=="",1])
# DistributionCentres <- as.character(initial_data[!(initial_data$DemandNodes)=="",2])
# results <- gmapsdistance(origin = Factories,destination = DistributionCentres, mode="driving")
# distances <- results$Distance
# write.csv(distances,"./distances.csv")
