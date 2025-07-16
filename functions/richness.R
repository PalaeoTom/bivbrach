richness <- function(data, cellID, nCells, nDraws, nOccs, standardiseOccs = T, taxonName, taxaToTally, n.cores = 1){
  ## Get cells
  cells <- unique(data[,cellID])
  if(standardiseOccs){
    ## If length of cells = nCells, proceed as below
    if(length(cells) == nCells){
      ## get occs
      occs <- which(data[,cellID] %in% cells)
      ## Sample
      draws <- mclapply(1:nDraws, mc.cores = n.cores, function(all){
        ## Randomly sample
        sampInd <- sample(occs, size = nOccs, replace = T)
        ## Get names
        taxa <- unique(data[sampInd,taxonName])
        ## prepare output
        out <- c()
        for(t in 1:length(taxaToTally)){
          out <- c(out, length(str_subset(taxa, taxaToTally[t])))
        }
        return(out)
      })
      ## Combine
      draws <- do.call(rbind, draws)
      ## Get median of each column
      output <- apply(draws, 2, function(x) median(x))
    } else {
      ## Need to standardise number of cells
      draws <- mclapply(1:nDraws, mc.cores = n.cores, function(all){
        ## Randomly sample nCells grid cells
        gcs <- sample(cells, size = nCells, replace = F)
        ## get occs
        occs <- which(data[,cellID] %in% gcs)
        ## Randomly sample
        sampInd <- sample(occs, size = nOccs, replace = T)
        ## Get names
        taxa <- unique(data[sampInd,taxonName])
        ## prepare output
        out <- c()
        for(t in 1:length(taxaToTally)){
          out <- c(out, length(str_subset(taxa, taxaToTally[t])))
        }
        return(out)
      })
      ## Combine
      draws <- do.call(rbind, draws)
      ## Get median of each column
      output <- apply(draws, 2, function(x) median(x))
    }
  } else {
    ## Same process but no standardised occurrences
    ## If length of cells = nCells, proceed as below
    if(length(cells) == nCells){
      ## get occs
      occs <- which(data[,cellID] %in% cells)
      ## Get names
      taxa <- unique(data[occs,taxonName])
      ## Get count
      out <- c()
      for(t in 1:length(taxaToTally)){
        out <- c(out, length(str_subset(taxa, taxaToTally[t])))
      }
      return(out)
    } else {
      ## Need to standardise number of cells
      draws <- mclapply(1:nDraws, mc.cores = n.cores, function(all){
        ## Randomly sample nCells grid cells
        gcs <- sample(cells, size = nCells, replace = F)
        ## get occs
        occs <- which(data[,cellID] %in% gcs)
        ## Get names
        taxa <- unique(data[occs,taxonName])
        ## prepare output
        out <- c()
        for(t in 1:length(taxaToTally)){
          out <- c(out, length(str_subset(taxa, taxaToTally[t])))
        }
        return(out)
      })
      ## Combine
      draws <- do.call(rbind, draws)
      ## Get median of each column
      output <- apply(draws, 2, function(x) median(x))
    }
  }
}
