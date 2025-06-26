richness <- function(data, cellID, nCells, nDraws, nOccs, taxonName, taxonomicRankToTally, taxaToTally, n.cores = 1){
  ## Get cells
  cells <- unique(data[,cellID])
  ## If length of cells = nCells, proceed as below
  if(length(cells) == nCells){
    ## Get row IDs
    occs <- seq(1,nrow(data),1)
    ## Sample
    draws <- mclapply(1:nDraws, mc.cores = n.cores, function(all){
      ## Randomly sample
      sampInd <- sample(occs, size = nOccs, replace = T)
      ## Isolate sample
      sampOccs <- data[sampInd,]
      ## Reduce to unique entries
      sampOccs <- sampOccs[!duplicated(sampOccs[,taxonName]),]
      ## prepare output
      out <- c()
      for(t in 1:length(taxaToTally)){
        out <- c(out, length(which(sampOccs[,taxonomicRankToTally] == taxaToTally[t])))
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
      ## Refine data to those contained within these grid cells
      data2 <- data[data[,cellID] %in% gcs,]
      ## Get row IDs
      occs <- seq(1,nrow(data2),1)
      ## Randomly sample
      sampInd <- sample(occs, size = nOccs, replace = T)
      ## Isolate sample
      sampOccs <- data2[sampInd,]
      ## Reduce to unique entries
      sampOccs <- sampOccs[!duplicated(sampOccs[,taxonName]),]
      ## prepare output
      out <- c()
      for(t in 1:length(taxaToTally)){
        out <- c(out, length(which(sampOccs[,taxonomicRankToTally] == taxaToTally[t])))
      }
      return(out)
    })
    ## Combine
    draws <- do.call(rbind, draws)
    ## Get median of each column
    output <- apply(draws, 2, function(x) median(x))
  }
}
