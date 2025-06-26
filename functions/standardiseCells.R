standardiseCells <- function(data, stage_cell, minOccs = 10){
  ## Get spacetime samples
  sc <- unique(data[,stage_cell])
  ## For each samp
  for(b in sc){
    ## if a spacetime sample contains less than minOccs unique genera-space-time occurrences, drop
    if(length(which(data[,stage_cell]==b))<minOccs){
      data <- data[-which(data[,stage_cell]==b),]
    }
  }
  return(data)
}
