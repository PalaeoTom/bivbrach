get.occs.per.cell <- function(data){
  output <- lapply(1:length(data), function(x){
    ## get unique cells
    cells <- unique(data[[x]][,"cell"])
    if(length(cells) > 0){
      by.cell <- sapply(cells, function(y) out <- nrow(data[[x]][which(data[[x]][,"cell"] == y),]))
    } else {
      by.cell <- 0
    }
  })
  ## create output
  matOut <- matrix(NA, nrow = length(output), ncol = 5)
  colnames(matOut) <- c("time.bin", "n.cells", "min.occs", "mean.occs", "max.occs")
  matOut[,1] <- names(data)
  for(i in 1:length(output)){
    if(length(output[[i]])>0){
      matOut[i,2] <- length(output[[i]])
      matOut[i,3] <- min(output[[i]])
      matOut[i,4] <- round(mean(output[[i]]), 0)
      matOut[i,5] <- max(output[[i]])
    }
  }
  return(matOut)
}
