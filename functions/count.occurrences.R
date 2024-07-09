count.occurrences <- function(input.dir, output.dir, input.prefix, output.prefix, method, vars, n.cores, taxa = F){
  combin <- expand.grid(vars)
  varStrings <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
  input.dirs <- paste0(input.dir, "/", input.prefix, "_", varStrings, ".Rds")
  output.dir <- paste0(output.dir, "/", output.prefix, ".csv")
  outMatrix <- cbind(combin,matrix(NA, nrow = length(input.dirs), ncol = 7))
  colnames(outMatrix) <- c(names(vars), "minimum", "q0.25", "median", "q0.75", "maximum", "mean", "standardDeviation")
  if(method == "sites" || method == "occs" || method == "sitesThenOccs"){
    if(taxa){
      for(d in 1:length(input.dirs)){
        data <- suppressWarnings(tryCatch(readRDS(input.dirs[d]), error = function(e){}))
        ## if data is NULL, skip to next
        if(is.null(data)){
          next
        } else {
          output <- mclapply(1:length(data), mc.cores = n.cores, function(ta){
            timeBin <- sapply(1:length(data[[ta]]), function(ti){
              spaReg <- unlist(sapply(1:length(data[[ta]][[ti]]), function(sp){
                subsample <- sapply(1:length(data[[ta]][[ti]][[sp]]), function(su){
                  n.occs <- nrow(data[[ta]][[ti]][[sp]][[su]])
                })
              }))
            })
          })
          ## re-order
          final <- lapply(1:length(output[[1]]), function(ti){
            taxa <- lapply(1:length(output), function(ta) output[[ta]][[ti]])
          })
          final <- unlist(lapply(1:length(final), function(x) out <- Reduce("+", final[[x]])))
        }
        outMatrix[d,3] <- min(final)
        outMatrix[d,4] <- quantile(final, probs = 0.25)
        outMatrix[d,5] <- median(final)
        outMatrix[d,6] <- quantile(final, probs = 0.75)
        outMatrix[d,7] <- max(final)
        outMatrix[d,8] <- round(mean(final), 2)
        outMatrix[d,9] <- round(sd(final), 2)
      }
    } else {
      for(d in 1:length(input.dirs)){
        data <- suppressWarnings(tryCatch(readRDS(input.dirs[d]), error = function(e){}))
        ## if data is NULL, skip to next
        if(is.null(data)){
          next
        } else {
          final <- unlist(mclapply(1:length(data), mc.cores = n.cores, function(ti){
            spaReg <- unlist(sapply(1:length(data[[ti]]), function(sp){
              subsample <- sapply(1:length(data[[ti]][[sp]]), function(su){
                n.occs <- nrow(data[[ti]][[sp]][[su]])
              })
            }))
          }))
        }
        outMatrix[d,3] <- min(final)
        outMatrix[d,4] <- quantile(final, probs = 0.25)
        outMatrix[d,5] <- median(final)
        outMatrix[d,6] <- quantile(final, probs = 0.75)
        outMatrix[d,7] <- max(final)
        outMatrix[d,8] <- round(mean(final), 2)
        outMatrix[d,9] <- round(sd(final), 2)
      }
    }
  } else {
    if(method == "divvySites" || method == "weightedDivvySites"){
      if(taxa){
        for(d in 1:length(input.dirs)){
          data <- suppressWarnings(tryCatch(readRDS(input.dirs[d]), error = function(e){}))
          ## if data is NULL, skip to next
          if(is.null(data)){
            next
          } else {
            output <- mclapply(1:length(data), mc.cores = n.cores, function(ta){
              timeBin <- sapply(1:length(data[[ta]]), function(ti){
                subsample <- unlist(sapply(1:length(data[[ta]][[ti]]), function(su){
                  n.occs <- nrow(data[[ta]][[ti]][[su]])
                }))
              })
            })
            ## re-order
            final <- lapply(1:length(output[[1]]), function(ti){
              taxa <- lapply(1:length(output), function(ta) output[[ta]][[ti]])
            })
            final <- unlist(lapply(1:length(final), function(x) out <- Reduce("+", final[[x]])))
          }
          outMatrix[d,3] <- min(final)
          outMatrix[d,4] <- quantile(final, probs = 0.25)
          outMatrix[d,5] <- median(final)
          outMatrix[d,6] <- quantile(final, probs = 0.75)
          outMatrix[d,7] <- max(final)
          outMatrix[d,8] <- round(mean(final), 2)
          outMatrix[d,9] <- round(sd(final), 2)
        }
      } else {
        for(d in 1:length(input.dirs)){
          data <- suppressWarnings(tryCatch(readRDS(input.dirs[d]), error = function(e){}))
          ## if data is NULL, skip to next
          if(is.null(data)){
            next
          } else {
            final <- unlist(mclapply(1:length(data), mc.cores = n.cores, function(ti){
              subsample <- unlist(sapply(1:length(data[[ti]][[sp]]), function(su){
                n.occs <- nrow(data[[ti]][[sp]][[su]])
              }))
            }))
          }
          outMatrix[d,3] <- min(final)
          outMatrix[d,4] <- quantile(final, probs = 0.25)
          outMatrix[d,5] <- median(final)
          outMatrix[d,6] <- quantile(final, probs = 0.75)
          outMatrix[d,7] <- max(final)
          outMatrix[d,8] <- round(mean(final), 2)
          outMatrix[d,9] <- round(sd(final), 2)
        }
      }
    } else {
      stop("this function is for getting number of occurrences for each subsample produced via cookies2() after time binning. Check method argument is one of the specified subsampling methods")
    }
  }
  write.csv(outMatrix, file = output.dir)
}
