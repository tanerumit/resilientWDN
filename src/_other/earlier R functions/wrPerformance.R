

wrPerformance   <- function(x, calculate.reliability = TRUE, calculate.resilience = TRUE, 
                            calculate.vulnerability = TRUE) {
  
  Reliability = NA
  Vulnerability = NA
  Resilience = NA
  
  if(calculate.reliability == TRUE) {
    # Calculate reliability 
    Reliability <- 1 - sum(x)/sum(x) #volumetric
    Reliability = length(which(x == 0))/length(x) #time-based
  }

  if(calculate.resilience == TRUE) {
    # Calculate resilience
    RLE <- rle(x>0)
    fs <- length(which(RLE$value == TRUE)) # Length of continuous sequences of failure periods
    fd <- length(which(x > 0))
    Resilience <- (ifelse(fd == 0, 0, fs/fd))
  }
  
  if(calculate.vulnerability == TRUE) {
    # Vulnerability 
    dur_lengths <- RLE$lengths[which(RLE$values == TRUE)]   # failure sequence lengths
    gap_indices <- which(x > 0)   # indices of failures
    
    index_beg <- 1
    local_max <- NULL
    
    for (i in 1:length(dur_lengths)) {
      
      if(i != 1) index_beg <- index_end + 1 
      index_end <- index_beg + dur_lengths[i]-1
      local_max <-append(local_max, max(x[gap_indices[index_beg:index_end]]))
      
    }
    Vulnerability <- mean(local_max)

  }

  return(list(Reliability = Reliability, Resilience = Resilience, Vulnerability = Vulnerability))
}