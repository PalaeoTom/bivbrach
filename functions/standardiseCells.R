standardiseCells <- function(data, cell, type = "occs", minOccs = 10, taxVar = "combined_name"){
  cells <- data[,cell]
  sc <- unique(cells)
  if(type == "occs"){
    droppers <- c()
    for(b in sc){
      if(length(which(cells %in% b))<minOccs){
        droppers <- c(droppers, b)
      }
    }
    out <- data[-which(cells %in% droppers),]
    return(out)
  } else {
    if(type == "taxa"){
      taxa <- data[,taxVar]
      droppers <- c()
      for(b in sc){
        if(length(unique(taxa[which(cells %in% b)]))<minOccs){
          droppers <- c(droppers, b)
        }
      }
      out <- data[-which(cells %in% droppers),]
      return(out)
    } else {
      stop("argument type not 'occs' or 'taxa'")
    }
  }
}
