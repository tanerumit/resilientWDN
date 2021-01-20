

#' Function to calculate water supply resilience
#'
#' metric defining how quickly a reservoir will recover from a failure
#' 
#' @param x vector specifying series of supply values
#' @param y vector specifying series of supply values
#'
#' @return numeric value 0 to 1
#' @export
#'
#' @examples
mResilience   <- function(x, y) {
  
  # Binary failures
  z <- ifelse(x-y > 0, 1, 0)
  rle_z <- rle(z>0)
  
  # Length of continuous sequences of failure periods
  fs <- length(which(rle_z$value == TRUE))
  fd <- length(which(z >0))
  
  return(ifelse(fd == 0, 0, fs/fd))

}


#dem <- c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100)
#sup <- c(100, 100, 80, 70, 50, 90, 100, 100, 60, 90, 100, 50, 100, 100, 100)

#mReliability(dem, sup, type = "time-based")
#mReliability(dem, sup, type = "volumetric")
#mVulnerability(dem, sup)
 
