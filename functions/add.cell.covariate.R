add.cell.covariate <- function(data, name, ref, values, threshold = 0.8, n.cores = 4){
  ## Get column number for ref
  refC <- which(colnames(data[[1]]) == ref)
  ## Get names
  bin.names <- names(data)
  output <- mclapply(1:length(data), mc.cores = n.cores, function(x){
    if(nrow(data[[x]]) > 0){
      ## get grid cells
      cells <- unique(data[[x]]$cell)
      ## Set up vector
      c <- rep(NA, nrow(data[[x]]))
      ## pass over each grid cell
      for(i in cells){
        cell <- data[[x]][which(data[[x]][,"cell"] %in% i),refC]
        n <- length(cell)
        v1c <- length(which(cell == values[1]))/n
        v2c <- length(which(cell == values[2]))/n
        if(v1c >= threshold){
          c[data[[x]][,"cell"] %in% i] <- values[1]
        } else {
          if(v2c >= threshold){
            c[data[[x]][,"cell"] %in% i] <- values[2]
          } else {
            c[data[[x]][,"cell"] %in% i] <- "mix"
          }
        }
      }
      ## convert to data frame
      cDF <- data.frame(c, stringsAsFactors = T)
      colnames(cDF) <- name
      out <- cbind(data[[x]], cDF)
      return(out)
    } else {
      out <- data[[x]]
      return(out)
    }
  })
  names(output) <- bin.names
  return(output)
}
