

library(epanetReader)
library(epanet2toolkit)
library(forecast)
library(GA)

# Simple EPANET Network 1 problem 

# Read-in the problem from the epanet file
inp <- file.path( find.package("epanet2toolkit"), "extdata","Net1.inp")
n1 <- read.inp(inp)

# Retrieve information about the problem 
summary(n1)
plot(n1)
names(n1)
n1$Junctions
n1$Reservoirs

#Simulate the network
ENepanet(inp, "Net1.rpt")

# Open an network file & change the parameter value then close
ENopen(inp, "Net1.rpt")
ENgetlinkvalue(2, "EN_LENGTH")
ENsetlinkvalue(2, "EN_LENGTH", 6789)
ENgetlinkvalue(2, "EN_LENGTH")
ENclose()







#Model calibration by univariate optimization **********************************

ENsettimeparam("EN_DURATION", 0)

optimize( calibObj,
interval = c(50, 150),
obsindex = c(4,6,8),
obs = c(112.11, 110.87, 110.32))

ENclose()


# Get node values

ENsetnodevalue(1, "EN_BASEDEMAND", 10)
ENgetnodevalue(1, "EN_BASEDEMAND")
ENclose()

#Simulate an EPANET
ENepanet(inp, "Net2.rpt")
ENopen(inp, "Net2.rpt")


ENopen("richmond-modified.inp", "rich.rpt")
myNodeIndex <- ENgetnodeindex("1302")
ENgetnodevalue(myNodeIndex, "EN_BASEDEMAND")



# Find demand of a given node
sapply(node_indices, function(x) ENgetnodevalue(x, "EN_BASEDEMAND"))




ENgetnodevalue(21, "EN_BASEDEMAND")










ENgetflowunits()
