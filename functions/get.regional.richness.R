get.regional.richness <- function(input.dir, input.pre, output.dir, output.pre, vars, times,
                                  SQS = T, coverage = 0.9, taxa = T, n.cores = 1, taxVar = "genus", rareVar = "collection_no", min.rareVar = 2, min.taxVar = 2,
                                  alt.value = "none"){
  combin <- expand.grid(vars)
  varStrings <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
  input.dirs <- paste0(input.dir, "/", input.pre, "_", varStrings, ".Rds")
  output.dirs <- paste0(output.dir, "/", output.pre, "_", varStrings, ".Rds")
  if(taxa){
    ## for each input.dirs
    for(d in 1:length(input.dirs)){
      ## read in data
      data <- readRDS(input.dirs[d])
      ## if no viable time bins, skip to next generation
      if(length(data[[1]]) < 1){
        next
      } else {
        taxa.labels <- names(data)
        time.labels <- names(data[[1]])
        if(SQS){
          output <- mclapply(1:length(data[[1]]), mc.cores = n.cores, function(t){
            per.taxon <- sapply(1:length(data), function(b){
              all.reps <- sapply(1:length(data[[b]][[t]]), function(r){
                ## if sample has more than 0 occurrences
                if(nrow(data[[b]][[t]][[r]]) > 0){
                  ## get number of unique taxa and collections
                  uc <- unique(data[[b]][[t]][[r]][,rareVar])
                  ut <- unique(data[[b]][[t]][[r]][,taxVar])
                  ## if both c and r exceed threshold
                  if(length(uc) >= min.rareVar && length(ut) >= min.taxVar){
                    ## create presence-absence matrix
                    pa <- matrix(0, nrow = length(ut), ncol = length(uc))
                    ## fill out presence-absence matrix
                    for (i in uc){
                      pa[which(ut %in% data[[b]][[t]][[r]][which(data[[b]][[t]][[r]][,rareVar] == i),taxVar]),which(uc %in% i)] <- 1
                    }
                    ## need at least one 0 for function to work
                    if(any(pa == 0)){
                      ifpa <- as.incfreq(pa)
                      att <- suppressWarnings(estimateD(ifpa, q = 0, datatype = "incidence_freq", base = "coverage", level = coverage))
                      ## if extrapolation beyond twice number of collections, defer to alt.value
                      if(att$t > (ifpa[1]*2)){
                        if(alt.value == "none"){
                          out <- NA
                        } else {
                          if(alt.value == "raw"){
                            out <- length(ut)
                          } else {
                            stop("argument alt.value needs to be 'none' or 'raw'")
                          }
                        }
                      } else {
                        out <- att$qD
                      }
                    } else {
                      if(alt.value == "none"){
                        out <- NA
                      } else {
                        if(alt.value == "raw"){
                          out <- length(ut)
                        } else {
                          stop("argument alt.value needs to be 'none' or 'raw'")
                        }
                      }
                    }
                  } else {
                    if(alt.value == "none"){
                      out <- NA
                    } else {
                      if(alt.value == "raw"){
                        out <- length(ut)
                      } else {
                        stop("argument alt.value needs to be 'none' or 'raw'")
                      }
                    }
                  }
                } else {
                  out <- 0
                }
                return(out)
              })
            })
            colnames(per.taxon) <- taxa.labels
            return(per.taxon)
          })
          names(output) <- time.labels
          ## the below calculates the raw richness
        } else {
          output <- mclapply(1:length(data[[1]]), mc.cores = n.cores, function(t){
            per.taxon <- sapply(1:length(data), function(b){
              all.reps <- sapply(1:length(data[[b]][[t]]), function(r){
                ## if sample has more than 0 occurrences
                if(nrow(data[[b]][[t]][[r]]) > 0){
                  ## get number of unique taxa and collections
                  ut <- unique(data[[b]][[t]][[r]][,taxVar])
                } else {
                  out <- 0
                }
                return(out)
              })
            })
            colnames(per.taxon) <- taxa.labels
            return(per.taxon)
          })
          names(output) <- time.labels
        }
      }
      ## Save output here
      saveRDS(output, output.dirs[d])
    }
  } else {
    stop("Tom has been lazy and hasn't written the version of this function that doesn't work over outputs split by Taxa. Bug him to do this.")
  }
}
