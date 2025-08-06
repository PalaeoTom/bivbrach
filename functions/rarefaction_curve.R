rarefaction_curve <- function(occs, iter = 1000){
  ## Get sample size
  N <- length(occs)
  ## Get iter samples
  sampledO <- sapply(1:iter, function(x){
    sampleX <- sample(occs, N, replace = F)
    dupes <- duplicated(sampleX)
    out <- rep(1,N)
    if(any(dupes)){
      out[which(dupes)] <- 0
    }
    rich <- cumsum(out)

  })
  ## return mean
  out <- apply(sampledO, 1, function(x) mean(x))
}
