get.interval.mat <- function(midpoints, cutoffs){
  category <- matrix(NA, nrow = length(midpoints), ncol = 1)
  values <- rownames(era.cutoffs)
  for(x in 1:nrow(era.cutoffs)){
    bet <- between(midpoints, era.cutoffs[x,2], era.cutoffs[x,1])
    if(any(bet)){
      category[which(bet),1] <- values[x]
    }
  }
  interval <- cbind(midpoints, category)
  colnames(interval) <- c("times", "interval")
  return(interval)
}
