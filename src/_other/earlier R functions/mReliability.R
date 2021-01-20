
#' Water Supply Reliability Metric
#'
#' Calculates water supply reliability based on a time-series of demand and supply.
#' Outputs can be provided as either time-based or volumetric. Time-based reliability considers the proportion of
#' intervals during the simulation period that the reservoir can meet the target demand. the volume of
#' water supplied to a demand centre divided by the total target demand during the entire simulation period,
#'   
#' 
#' @param demand vector specifying series of demand values
#' @param supply vector specifying series of supply values
#' @param type binary value indicating type of reliability calculation: 1 time-based, 2 volumetric
#'
#' @return numeric value displaying reliability from 0 to 100
#' @export
#'
#' @examples
mReliability  <- function(demand, supply, type = "time-based") {
  
  if (type == "time-based") {
    z = length(which(supply-demand == 0))/length(demand)
  } else {
    z <- 1 - sum(demand-supply)/sum(demand)
  }
  return(z)
  
}

mReliability2  <- function(x, type = "time-based") {
  
  if (type == "time-based") {
    z = length(which(x == 0))/length(x)
  } else {
    z <- 1 - sum(x)/sum(x)
  }
  return(z)
  
}

