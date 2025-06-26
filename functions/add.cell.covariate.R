add.cell.covariate <- function(data, stage_cell, name, unknown, ref, values, threshold = 0.75){
  ## Get unique spacetime samples
  sc <- unique(data[,stage_cell])
  ## Get colnames
  cn <- colnames(data)
  ## Add empty container to data
  data$placeholder <- ""
  colnames(data) <- c(cn,name)
  ## for each bin
  for(b in sc){
    ## Get index of occurrences in this spacetime sample
    occs <- which(data[,stage_cell]==b)
    ## Get spacetime sample
    samp <- data[occs,]
    ## Get number of occurrences with assignments
    n <- nrow(samp)-length(which(samp[,ref]==unknown))
    ## If at least 1 occurrence has value associated
    if(n>0){
      ## Get proportions for each value
      v1c <- length(which(samp[,ref] == values[1]))/n
      v2c <- length(which(samp[,ref] == values[2]))/n
      ## Assign value if threshold exceeded
      if(v1c >= threshold){
        data[occs,name] <- values[1]
      } else {
        if(v2c >= threshold){
          data[occs,name] <- values[2]
        } else {
          data[occs,name] <- "mixed"
        }
      }
    } else {
      ## If 0, set as mixed (level 0)
      data[occs,name] <- "mixed"
    }
  }
  return(data)
}
