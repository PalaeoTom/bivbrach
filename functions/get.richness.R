get.richness <- function(data, SQS = T, coverage = 0.9, taxa = T, n.cores = 4, taxVar = "genus", rareVar = "collection_no", min.coll = 2, min.taxa = 2){
  if(taxa){
    taxa.labels <- names(data)
    bin.labels <- names(data[[1]])
    if(SQS){
      output <- mclapply(1:length(data), mc.cores = n.cores, function(x){
        bin <- lapply(1:length(data[[x]]), function(y){
          rep <- sapply(1:length(data[[x]][[y]]), function(z){
            if(nrow(data[[x]][[y]][[z]]) > 0){
              c <- unique(data[[x]][[y]][[z]][,rareVar])
              r <- unique(data[[x]][[y]][[z]][,taxVar])
              if(length(c) >= min.coll && length(r) >= min.taxa){
                pa <- matrix(0, nrow = length(r), ncol = length(c))
                for (i in c){
                  pa[which(r %in% data[[x]][[y]][[z]][which(data[[x]][[y]][[z]][,rareVar] == i),taxVar]),which(c %in% i)] <- 1
                }
                if(any(pa == 0)){
                  ifpa <- as.incfreq(pa)
                  att <- suppressWarnings(estimateD(ifpa, q = 0, datatype = "incidence_freq", base = "coverage", level = coverage))
                  if(att$t > (ifpa[1]*2)){
                    out <- NA
                  } else {
                    out <- att$qD
                  }
                } else {
                  out <- NA
                }
              } else {
                out <- NA
              }
            } else {
              out <- 0
            }
            return(out)
          })
        })
        names(bin) <- bin.labels
        return(bin)
      })
      names(output) <- taxa.labels
      return(output)
    } else {
      output <- mclapply(1:length(data), mc.cores = n.cores, function(x){
        bin <- lapply(1:length(data[[x]]), function(y){
          rep <- sapply(1:length(data[[x]][[y]]), function(z){
            if(nrow(data[[x]][[y]][[z]]) > 0){
              out <- length(unique(data[[x]][[y]][[z]][,taxVar]))
            } else {
              out <- 0
            }
          })
        })
        names(bin) <- bin.labels
        return(bin)
      })
      names(output) <- taxa.labels
      return(output)
    }
  } else {
    bin.labels <- names(data)
    if(SQS){
      bin <- mclapply(1:length(data), mc.cores = n.cores, function(y){
        rep <- sapply(1:length(data[[y]]), function(z){
          if(nrow(data[[y]][[z]]) > 0){
            c <- unique(data[[y]][[z]][,rareVar])
            r <- unique(data[[y]][[z]][,taxVar])
            if(length(c) >= min.coll && length(r) >= min.taxa){
              pa <- matrix(0, nrow = length(r), ncol = length(c))
              for (i in c){
                pa[which(r %in% data[[y]][[z]][which(data[[y]][[z]][,rareVar] == i),taxVar]),which(c %in% i)] <- 1
              }
              if(any(pa == 0)){
                ifpa <- as.incfreq(pa)
                att <- suppressWarnings(estimateD(ifpa, q = 0, datatype = "incidence_freq", base = "coverage", level = coverage))
                if(att$t > (ifpa[1]*2)){
                  out <- NA
                } else {
                  out <- att$qD
                }
              } else {
                out <- NA
              }
            } else {
              out <- NA
            }
          } else {
            out <- 0
          }
          return(out)
        })
      })
      names(bin) <- bin.labels
      return(bin)
    } else {
      bin <- mclapply(1:length(data), mc.cores = n.cores, function(y){
        rep <- sapply(1:length(data[[y]]), function(z){
          if(nrow(data[[y]][[z]]) > 0){
            out <- length(unique(data[[y]][[z]][,taxVar]))
          } else {
            out <- 0
          }
        })
      })
      names(bin) <- bin.labels
      return(bin)
    }
  }
}
