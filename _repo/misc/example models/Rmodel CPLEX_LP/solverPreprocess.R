

##### Create CIPLEX_LP file
##### Form the objective function

# Create variables names
varnames <- data.frame()
for (i in 1:length(supplyNodes)){
  for (j in 1:length(demandNodes)){
    varnames[i,j]<-(paste(supplyNodes[i], "_to_", demandNodes[j],sep=""))
  }
}


################ OBJECTIVE FUNCTION --------------------------------------------

total_demand  <- sum(DemandConstraints$Demand)
varnames_list <- as.vector(unmatrix(varnames,byrow = TRUE))
objFun <- paste(varnames_list, collapse = " + ")

################################################################################

################ CONSTRAINTS ---------------------------------------------------

# 1) Supply node constraints
# 2) Demand node constraints
# 3) Pipe capacity constraints

################# 1) Supply Node Constraints
# Total Deliveries to the demand nodes should be less than or equal to the 
# available amount

constraints1 <- read.csv(FACTORYCONSTRAINTS_FILE)
constraint <- as.vector(NA)
for (i in 1:length(supplyNodes)){
  for (j in 1:length(demandNodes)){
    constraint[i]<-  (paste(constraint[i]," + ",supplyNodes[i], "_to_", demandNodes[j],sep=""))
  }
}

for (i in 1:length(supplyNodes)){
  constraint[i]<- paste(constraint[i],constraints1[i,3],constraints1[i,2])
}
constraint <- sapply(strsplit(constraint, split="NA + ", fixed=TRUE), function(x) (x[2]))


### 2) Demand Node Constraints
# Actual deliveries should be less than or equal to the given amount
constraints2 <- read.csv(DEMANDNODECONSTRAINTS_FILE)
constraint2  <- as.vector(NA)
for (i in 1:length(demandNodes)){
  for (j in 1:length(supplyNodes)){
    constraint2[i]<-  (paste(constraint2[i]," + ",supplyNodes[j], "_to_", demandNodes[i],sep=""))
  }
}
for (i in 1:length(demandNodes)){
  constraint2[i]<- paste(constraint2[i],constraints2[i,3],constraints2[i,2])
}
constraint2 <- sapply(strsplit(constraint2, split="NA + ", fixed=TRUE), function(x) (x[2]))


  
### 3) Pipe Capacity Constraints
cons_data <- distances
for (i in 1:nrow(distances)) {
  for(j in 2:ncol(distances)) {
    cons_data[i, j] <- paste0(distances[i,"to"], "_to_", colnames(distances)[j], " <= ", distances[i,j])
  }
} 
constraint3 <- as.vector(unmatrix(cons_data[,-1],byrow=T))

# Combine all constraints
constraints <- c(constraint,constraint2) #, constraint3)


#Output to file
sink(CPLEX_FILENAME)
cat("Maximize\n")
cat(paste(" obj:",objFun,"\n"))
cat("Subject To \n")
c<-as.vector(0)
for (i in 1:length(constraints)){
  c[i]<-as.vector(paste("c",i,":",sep=""))
  cat(paste(" ",c[i],constraints[i],"\n"))
}
cat("End")
sink(NULL)
