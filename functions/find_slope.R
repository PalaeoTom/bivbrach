find_slope <- function(RCs, n = 5, min.n = 20, threshold = 0.25, n.cores = 1){
  ## get sample number vector
  samples <- seq(1,n,1)
  slopes <- mclapply(1:ncol(RCs), mc.cores = n.cores, function(x){
    ## Get curve
    curve <- RCs[which(!is.na(RCs[,x])),x]
    ## Get start
    start <- seq(1,length(curve)-(n-1),1)
    ## Get end
    end <- seq(1+(n-1),length(curve),1)
    ## define container and start point
    out <- c()
    i = 1
    while(TRUE){
      range <- curve[seq(start[i],end[i],1)]
      coeff <- as.numeric(lm(range ~ samples)$coefficients[2])
      if(coeff < threshold){
        out <- end[i]
        break
      } else {
        i <- i + 1
      }
    }
    return(out)
  })
  output <- sapply(1:length(slopes), function(y) slopes[[y]])
  ## Apply minimum
  output[which(output < min.n)] <- 20
  names(output) <- colnames(RCs)
  return(output)
}
