add.cell.covariate <- function(data, cell, name, unknown, ref, value){
  ## Get unique spacetime samples
  sc <- unique(data[,cell])
  ## Get colnames
  cn <- colnames(data)
  ## Add empty container to data
  data$placeholder <- ""
  colnames(data) <- c(cn,name)
  ## for each bin
  for(b in sc){
    ## Get index of occurrences in this spacetime sample
    occs <- which(data[,cell]==b)
    ## Get spacetime sample
    samp <- data[occs,]
    ## Get number of occurrences with assignments
    n <- nrow(samp)-length(which(samp[,ref]==unknown))
    ## If at least 1 occurrence has a record,
    if(n>0){
      ## Assign proportion as value
      data[occs,name] <- length(which(samp[,ref] == value))/n
    } else {
      ## If 0, set as NA. Can change later
      data[occs,name] <- NA
    }
  }
  return(data)
}
