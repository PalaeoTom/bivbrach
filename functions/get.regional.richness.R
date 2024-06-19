get.regional.richness <- function(input.dir, input.pre, output.dir, output.pre, vars, mode = "SQS", SQS.coverage = 0.9,
                                  SR.rep = 100, SR.nSite = 3, taxa = T, n.cores = 1, taxVar = "genus",
                                  SQS.rareVar = "collection_no", SQS.min.rareVar = 2, SQS.min.taxVar = 2, SQS.min.taxRareVar.alt = "none", SQS.noAbsence.alt = "none", SQS.exceedExtrap.alt = "none",
                                  omit.NAs = T){
  combin <- expand.grid(vars)
  varStrings <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
  input.dirs <- paste0(input.dir, "/", input.pre, "_", varStrings, ".Rds")
  output.dirs <- paste0(output.dir, "/", output.pre, "_", varStrings, ".csv")
  if(mode == "SR"){
    if(length(SR.nSite) == 1){
      nSites <- rep(SR.nSite, length(input.dirs))
    } else {
      if(length(SR.nSite) == length(input.dirs) && is.numeric(SR.nSite)){
        nSites <- SR.nSite
      } else {
        stop("Argument SR.nSite needs to be a single numeric value or a vector of numeric values specifying the number of sites to be sampled for each combination of variables. See expand.grid(vars) for order and number of values needed.")
      }
    }
  }
  if(taxa){
    ## for each input.dirs
    for(d in 1:length(input.dirs)){
      ## read in data
      data <- suppressWarnings(tryCatch(readRDS(input.dirs[d]), error = function(e){}))
      ## if data is NULL, skip to next
      if(is.null(data)){
        next
      } else {
        taxa.labels <- names(data)
        time.labels <- names(data[[1]])
        if(mode == "SQS"){
          output <- mclapply(1:length(data[[1]]), mc.cores = n.cores, function(t){
            per.taxon <- sapply(1:length(data), function(b){
              all.reps <- sapply(1:length(data[[b]][[t]]), function(r){
                ## if sample has more than 0 occurrences
                if(nrow(data[[b]][[t]][[r]]) > 0){
                  ## get number of unique taxa and collections
                  uc <- unique(data[[b]][[t]][[r]][,SQS.rareVar])
                  ut <- unique(data[[b]][[t]][[r]][,taxVar])
                  ## if both c and r exceed threshold
                  if(length(uc) >= SQS.min.rareVar && length(ut) >= SQS.min.taxVar){
                    ## create presence-absence matrix
                    pa <- matrix(0, nrow = length(ut), ncol = length(uc))
                    ## fill out presence-absence matrix
                    for (i in uc){
                      pa[which(ut %in% data[[b]][[t]][[r]][which(data[[b]][[t]][[r]][,SQS.rareVar] == i),taxVar]),which(uc %in% i)] <- 1
                    }
                    ## need at least one 0 for function to work
                    if(any(pa == 0)){
                      ifpa <- as.incfreq(pa)
                      att <- suppressWarnings(estimateD(ifpa, q = 0, datatype = "incidence_freq", base = "coverage", level = SQS.coverage))
                      ## if extrapolation beyond twice number of collections, defer to exceedExtrap.alt
                      if(att$t > (ifpa[1]*2)){
                        if(SQS.exceedExtrap.alt == "none"){
                          out <- NA
                        } else {
                          if(SQS.exceedExtrap.alt == "raw"){
                            out <- length(ut)
                          } else {
                            stop("argument SQS.exceedExtrap.alt needs to be 'none' or 'raw'")
                          }
                        }
                      } else {
                        out <- att$qD
                      }
                    } else {
                      if(SQS.noAbsence.alt == "none"){
                        out <- NA
                      } else {
                        if(SQS.noAbsence.alt == "raw"){
                          out <- length(ut)
                        } else {
                          stop("argument SQS.noAbsence.alt needs to be 'none' or 'raw'")
                        }
                      }
                    }
                  } else {
                    if(SQS.min.taxRareVar.alt == "none"){
                      out <- NA
                    } else {
                      if(SQS.min.taxRareVar.alt == "raw"){
                        out <- length(ut)
                      } else {
                        stop("argument SQS.min.taxRareVar.alt needs to be 'none' or 'raw'")
                      }
                    }
                  }
                } else {
                  out <- 0
                }
                return(out)
              })
            })
            names(per.taxon) <- taxa.labels
            return(per.taxon)
          })
          names(output) <- time.labels
          ## the below calculates the raw richness
        } else {
          if(mode == "raw"){
          output <- mclapply(1:length(data[[1]]), mc.cores = n.cores, function(t){
            per.taxon <- sapply(1:length(data), function(b){
              all.reps <- sapply(1:length(data[[b]][[t]]), function(r){
                ## if sample has more than 0 occurrences
                if(nrow(data[[b]][[t]][[r]]) > 0){
                  ## get number of unique taxa and collections
                  out <- length(unique(data[[b]][[t]][[r]][,taxVar]))
                } else {
                  out <- 0
                }
                return(out)
              })
            })
            names(per.taxon) <- taxa.labels
            return(per.taxon)
          })
          names(output) <- time.labels
          } else {
            if(mode == "SR"){
              #### Now to add individual spatial subsample site rarefaction
              output <- mclapply(1:length(data[[1]]), mc.cores = n.cores, function(t){
                per.taxon <- sapply(1:length(data), function(b){
                  all.reps <- sapply(1:length(data[[b]][[t]]), function(r){
                    if(nrow(data[[b]][[t]][[r]]) > 0){
                      ## Get list of unique cells
                      cells <- unique(data[[b]][[t]][[r]][,"cell"])
                      ## Get SR.rep samples
                      subsamples <- sapply(1:SR.rep, function(all){
                        ## Get sample of SR.nSite
                        samp <- unique(sample(cells, size = nSites[d], replace = T))
                        ## Get count of taxa
                        count <- length(unique(data[[b]][[t]][[r]][data[[b]][[t]][[r]][,"cell"] %in% samp,taxVar]))
                      })
                      ## get median
                      out <- median(subsamples)
                    } else {
                      out <- 0
                    }
                  })
                })
                names(per.taxon) <- taxa.labels
                return(per.taxon)
              })
              names(output) <- time.labels
            } else {
              stop("mode needs to be set 'SQS', 'raw', or 'SR'")
            }
          }
        }
      }
      ## get times column
      times.out <- c()
      for(i in 1:length(time.labels)){
        if(length(output[[i]]) > 2){
          times.out <- c(times.out,rep(time.labels[i],nrow(output[[i]])))
        } else {
          times.out <- c(times.out, time.labels[i])
        }
      }
      ## reformat outputs
      output.mat <- cbind(as.numeric(times.out), do.call(rbind, output))
      colnames(output.mat) <- c("times", taxa.labels)
      rownames(output.mat) <- NULL
      ## if na.omit true, drop rows with NAs
      if(omit.NAs){
        output.mat <- output.mat[apply(output.mat, 1, function(x) !any(is.na(x))),]
        if(length(output.mat)>=3){
          write.csv(output.mat, output.dirs[d])
        }
      } else {
      ## Save output here
      write.csv(output.mat, output.dirs[d])
      }
    }
  } else {
    stop("Tom has been lazy and hasn't written the version of this function that doesn't work over outputs split by Taxa. Bug him to do this.")
  }
}
