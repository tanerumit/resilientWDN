
#' Function to calculate water supply resilience
#'
#' metric defining how quickly a reservoir will recover from a failure
#' 
#' @param demand vector specifying series of supply values
#' @param supply vector specifying series of supply values
#'
#' @return numeric value 0 to 1
#' @export
#'
#' @examples
mResilience   <- function(demand, supply) {
  
  # Binary failures
  z <- ifelse(demand-supply > 0, 1, 0)
  rle_z <- rle(z>0)
  
  # Length of continuous sequences of failure periods
  fs <- length(which(rle_z$value == TRUE))
  fd <- length(which(z >0))
  
  return(ifelse(fd == 0, 0, fs/fd))
  
}

mResilience2   <- function(x) {
  
  # Binary failures
  z <- ifelse(x > 0, 1, 0)
  rle_z <- rle(x>0)
  
  # Length of continuous sequences of failure periods
  fs <- length(which(rle_z$value == TRUE))
  fd <- length(which(z >0))
  
  return(ifelse(fd == 0, 0, fs/fd))
  
}

