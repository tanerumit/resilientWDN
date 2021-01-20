
source("global.R")

# Distribution Network Optimization Using R -----------------------------------#

# Optimize a distribution network with multiple supply and demand nodes.

# Information known:
# Location of Supply and Demand Nodes (network structure)
# Current demand at the demand nodes
# Available supply at the demand nodes

# Declare file names
PIPE_CAPACITY_FILE            <- "./data/pipeConstraints.csv"     # pipeCaps
SUPPLY_NODE_CONSTRAINTS_FILE  <- "./data/supplyConstraints.csv"   # supplier constraints  
DEMAND_NODE_CONSTRAINTS_FILE  <- "./data/demandConstraints.csv"   # demand constraints
CPLEX_FILENAME                <- "./data/filename1.lp"            # code to be sent to cplex
SOLUTION_FILENAME             <- "./data/solution1.txt"           # where the results are saved


# Define indices for the demand and supply nodes
supplyNodes   <- paste0("S", 1:3)   
demandNodes   <- paste0("D", 1:6)   

supplyConstraints <- read.csv(SUPPLY_NODE_CONSTRAINTS_FILE)
demandConstraints   <- read.csv(DEMAND_NODE_CONSTRAINTS_FILE)

#### Create the cost matrix
pipeCaps  <- read.csv(PIPE_CAPACITY_FILE)

# Create objects from the costmatrix
for (i in 1:length(supplyNodes)){
  for (j in 1:length(demandNodes)){
    assign(paste(supplyNodes[i], "_to_", demandNodes[j],sep=""), pipeCaps[i,j])
  }
}

# Pass variables to the optimization algorithm
source("solverPreprocess.R")




# Solve the problem
require(Rglpk)
x <- Rglpk_read_file(file=CPLEX_FILENAME, type = "CPLEX_LP")

solution <- Rglpk_solve_LP(x$objective, mat= x$constraints[[1]],dir= x$constraints[[2]], rhs= x$constraints[[3]], x$bounds, x$types, x$maximum)

# Solution Output 
require("gdata")
sink(SOLUTION_FILENAME)
vars  <-as.vector(unmatrix(varnames,byrow=T))
costs <-as.vector(unmatrix(costmatrix,byrow=T))
cat(paste("Total Cost (euros):",solution$optimum,"\n\n"))
for (i in 1:length(vars)){
  cat(paste(vars[i],"=",solution$solution[i],"cost:",solution$solution[i],"\n"))
}
sink(NULL) 


###### CLEAN SOLUTION

vars<-as.vector(unmatrix(varnames,byrow=T))
costs<-as.vector(unmatrix(costmatrix,byrow=T))

only_include<-which(solution$solution!=0)

vars<-vars[only_include]
costs<-costs[only_include]
clean_solution<-solution$solution[only_include]

cat(paste("Total Cost (euros):",solution$optimum,"\n\n"))
for (i in 1:length(vars)){
  cat(paste(vars[i],"=",clean_solution[i],"cost:",clean_solution[i]*costs[i],"\n"))
}






# cost of freight per ton per 100km in euros
cost <- 2.29

#### Create the cost matrix
pipeCaps  <- read.csv(PIPE_CAPACITY_FILE)
costmatrix <- pipeCaps[, 2:ncol(pipeCaps)]/100000*cost

# Create vectors with the correct variable names,eliminate countries (also gmapdistance inserts "Distance." to the names)
headers <-as.vector(names(costmatrix))
newheaders <-sapply(strsplit(headers, split='.', fixed=TRUE), function(x) (x[2]))
newheaders <-sapply(strsplit(newheaders, split='+', fixed=TRUE), function(x) (x[1]))
supplyNodes <- as.vector(pipeCaps[,1])
supplyNodes <- sapply(strsplit(supplyNodes, split='+', fixed=TRUE), function(x) (x[1]))
demandNodes <- as.vector(newheaders)

# renaming and writing the matrixes for our report 
names(pipeCaps)[2:ncol(pipeCaps)]<-c(newheaders)
names(costmatrix) <- c(newheaders)
costmatrixtowrite <- cbind(pipeCaps[,1],costmatrix)
names(pipeCaps)[1] <- c("to")
names(costmatrixtowrite)[1] <- c("to")

write.csv(pipeCaps,"distance_matrix_for_report.csv",row.names=FALSE)
write.csv(costmatrixtowrite,"cost_matrix_for_report.csv",row.names=FALSE)










