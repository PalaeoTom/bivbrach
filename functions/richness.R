richness <- function(data, cellID, nCells, nDraws, nOccs, cov.names, standardiseOccs = T, standardiseOccsWithReplacement = T, taxonName, taxaToTally, splitTaxa = F, n.cores = 1){
  ## Get cells
  cells <- unique(data[,cellID])
  ## If we're going to standardise occurrences
  if(standardiseOccs){
    ## If we're going to split taxa up
    if(splitTaxa){
      ## If we have the right number of cells
      if(length(cells) == nCells){
        ## get covariates of cells
        covs <- apply(data[match(cells, data[,cellID]),cov.names], 2, function(x) mean(x))
        ## get names
        taxa <- data[which(data[,cellID] %in% cells),taxonName]
        ## subset
        split <- lapply(1:length(taxaToTally), function(x) str_subset(taxa, taxaToTally[x]))
        ## draw
        draws <- mclapply(1:nDraws, mc.cores = n.cores, function(all){
          out <- c()
          for(t in 1:length(taxaToTally)){
            if(length(split[[t]]) > 0){
              out <- c(out, length(unique(sample(split[[t]], size = nOccs, replace = T))))
            } else {
              out <- c(out, 0)
            }
          }
          ## concatenate with other outputs
          return(out)
        })
        ## Get median of richness values
        output <- c()
        for(tt in 1:length(taxaToTally)){
          output <- c(output, median(sapply(1:length(draws), function(x) draws[[x]][tt])))
        }
        ## concatenate for output
        output <- c(output, covs)
        return(output)
      } else {
        draws <- mclapply(1:nDraws, mc.cores = n.cores, function(all){
          ## Get grid cells
          gcs <- sample(cells, size = nCells, replace = F)
          ## get covariates of cells
          covs <- apply(data[match(gcs, data[,cellID]),cov.names], 2, function(x) mean(x))
          ## Get taxa
          taxa <- data[which(data[,cellID] %in% gcs),taxonName]
          ## subset
          split <- lapply(1:length(taxaToTally), function(x) str_subset(taxa, taxaToTally[x]))
          ## draw
          out <- c()
          for(t in 1:length(taxaToTally)){
            if(length(split[[t]]) > 0){
              out <- c(out, length(unique(sample(split[[t]], size = nOccs, replace = T))))
            } else {
              out <- c(out, 0)
            }
          }
          ## combine for output
          out <- c(out, covs)
          return(out)
        })
        ## Now to do mean for covariates and median for richness
        output <- c()
        for(tt in 1:length(taxaToTally)){
          output <- c(output, median(sapply(1:length(draws), function(x) draws[[x]][tt])))
        }
        for(cc in (length(taxaToTally)+1):(length(taxaToTally)+length(cov.names))){
          output <- c(output, mean(sapply(1:length(draws), function(x) draws[[x]][cc])))
        }
        return(output)
      }
    } else {
      if(length(cells) == nCells){
        ## get covs
        covs <- apply(data[match(cells, data[,cellID]),cov.names], 2, function(x) mean(x))
        ## get taxa
        taxa <- data[which(data[,cellID] %in% cells),taxonName]
        ## draw
        draws <- mclapply(1:nDraws, mc.cores = n.cores, function(all){
          ## draw sample
          samp <- sample(taxa, size = nOccs, replace = standardiseOccsWithReplacement)
          ## split up sample
          out <- c()
          for(t in 1:length(taxaToTally)){
            out <- c(out, length(str_subset(samp, taxaToTally[t])))
          }
          return(out)
        })
        ## get median of richness values
        output <- c()
        for(tt in 1:length(taxaToTally)){
          output <- c(output, median(sapply(1:length(draws), function(x) draws[[x]][tt])))
        }
        ## concatenate for output
        output <- c(output, covs)
        return(output)
      } else {
        draws <- mclapply(1:nDraws, mc.cores = n.cores, function(all){
          ## get grid cells
          gcs <- sample(cells, size = nCells, replace = F)
          ## get covariates of cells
          covs <- apply(data[match(gcs, data[,cellID]),cov.names], 2, function(x) mean(x))
          ## get taxa in cells
          taxa <- data[which(data[,cellID] %in% gcs),taxonName]
          ## Sample
          samp <- sample(taxa, size = nOccs, replace = standardiseOccsWithReplacement)
          ## Split
          out <- c()
          for(t in 1:length(taxaToTally)){
            out <- c(out, length(str_subset(samp, taxaToTally[t])))
          }
          out <- c(out,covs)
          return(out)
        })
        ## Now to do mean for covariates and median for richness
        output <- c()
        for(tt in 1:length(taxaToTally)){
          output <- c(output, median(sapply(1:length(draws), function(x) draws[[x]][tt])))
        }
        for(cc in (length(taxaToTally)+1):(length(taxaToTally)+length(cov.names))){
          output <- c(output, mean(sapply(1:length(draws), function(x) draws[[x]][cc])))
        }
        return(output)
      }
    }
  } else {
    if(length(cells) == nCells){
      ## get covariates of cells
      covs <- apply(data[match(cells, data[,cellID]),cov.names], 2, function(x) mean(x))
      ## get names
      taxa <- unique(data[which(data[,cellID] %in% cells),taxonName])
      ## Separate and count
      out <- c()
      for(t in 1:length(taxaToTally)){
        out <- c(out, length(str_subset(taxa, taxaToTally[t])))
      }
      ## combine with covariates and return
      output <- c(out, covs)
      return(output)
    } else {
      draws <- mclapply(1:nDraws, mc.cores = n.cores, function(all){
        ## draw grid cells
        gcs <- sample(cells, size = nCells, replace = F)
        ## get covariates of cells
        covs <- apply(data[match(gcs, data[,cellID]),cov.names], 2, function(x) mean(x))
        ## Get taxa
        taxa <- unique(data[which(data[,cellID] %in% gcs),taxonName])
        ## Separate and count
        out <- c()
        for(t in 1:length(taxaToTally)){
          out <- c(out, length(str_subset(taxa, taxaToTally[t])))
        }
        ## combine with covariates and return
        out <- c(out, covs)
        return(out)
      })
      ## Now to do mean for covariates and median for richness
      output <- c()
      for(tt in 1:length(taxaToTally)){
        output <- c(output, median(sapply(1:length(draws), function(x) draws[[x]][tt])))
      }
      for(cc in (length(taxaToTally)+1):(length(taxaToTally)+length(cov.names))){
        output <- c(output, mean(sapply(1:length(draws), function(x) draws[[x]][cc])))
      }
      return(output)
    }
  }
}
