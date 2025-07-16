richness <- function(data, cellID, nCells, nDraws, nOccs, standardiseOccs = T, standardiseOccsWithReplacement = T, taxonName, taxaToTally, n.cores = 1){
  cells <- unique(data[,cellID])
  if(standardiseOccs){
    if(length(cells) == nCells){
      occs <- which(data[,cellID] %in% cells)
      draws <- mclapply(1:nDraws, mc.cores = n.cores, function(all){
        sampInd <- sample(occs, size = nOccs, replace = standardiseOccsWithReplacement)
        taxa <- unique(data[sampInd,taxonName])
        out <- c()
        for(t in 1:length(taxaToTally)){
          out <- c(out, length(str_subset(taxa, taxaToTally[t])))
        }
        return(out)
      })
      draws <- do.call(rbind, draws)
      output <- apply(draws, 2, function(x) median(x))
    } else {
      draws <- mclapply(1:nDraws, mc.cores = n.cores, function(all){
        gcs <- sample(cells, size = nCells, replace = F)
        occs <- which(data[,cellID] %in% gcs)
        sampInd <- sample(occs, size = nOccs, replace = standardiseOccsWithReplacement)
        taxa <- unique(data[sampInd,taxonName])
        out <- c()
        for(t in 1:length(taxaToTally)){
          out <- c(out, length(str_subset(taxa, taxaToTally[t])))
        }
        return(out)
      })
      draws <- do.call(rbind, draws)
      output <- apply(draws, 2, function(x) median(x))
    }
  } else {
    if(length(cells) == nCells){
      occs <- which(data[,cellID] %in% cells)
      taxa <- unique(data[occs,taxonName])
      out <- c()
      for(t in 1:length(taxaToTally)){
        out <- c(out, length(str_subset(taxa, taxaToTally[t])))
      }
      return(out)
    } else {
      draws <- mclapply(1:nDraws, mc.cores = n.cores, function(all){
        gcs <- sample(cells, size = nCells, replace = F)
        occs <- which(data[,cellID] %in% gcs)
        taxa <- unique(data[occs,taxonName])
        out <- c()
        for(t in 1:length(taxaToTally)){
          out <- c(out, length(str_subset(taxa, taxaToTally[t])))
        }
        return(out)
      })
      draws <- do.call(rbind, draws)
      output <- apply(draws, 2, function(x) median(x))
    }
  }
}
