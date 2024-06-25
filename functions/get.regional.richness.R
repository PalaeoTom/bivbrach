get.regional.richness <- function(input.dir, input.pre, output.dir, output.pre, vars, taxa = T, n.cores = 1, taxVar = "genus"){
  ## set up inputs
  combin <- expand.grid(vars)
  varStrings <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
  input.dirs <- paste0(input.dir, "/", input.pre, "_", varStrings, ".Rds")
  output.dirs <- paste0(output.dir, "/", output.pre, "_", varStrings, ".csv")
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
        cookie.counts <- lapply(1:length(data[[1]]), function(t) length(data[[1]][[t]]))
        ## get counts of taxa
        output <- mclapply(1:length(data[[1]]), mc.cores = n.cores, function(t){
          per.taxon <- sapply(1:length(data), function(b){
            per.cookie <- sapply(1:length(data[[b]][[t]]), function(c){
              per.rep <- sapply(1:length(data[[b]][[t]][[c]]), function(r){
                if(nrow(data[[b]][[t]][[c]][[r]]) > 0){
                  out <- length(unique(data[[b]][[t]][[c]][[r]][,taxVar]))
                } else {
                  out <- 0
                }
                return(out)
              })
            })
          })
      colnames(per.taxon) <- taxa.labels
      return(per.taxon)
      })
      names(output) <- time.labels
      ## get times column
      times.out <- c()
      for(i in 1:length(time.labels)){
        if(length(output[[i]]) > 2){
          times.out <- c(times.out,rep(time.labels[i],nrow(output[[i]])))
        } else {
          times.out <- c(times.out, time.labels[i])
        }
      }
      ## get cookie.id column
      cookies.out <- c()
      for(t in 1:length(time.labels)){
        if(length(output[[t]]) > 2){
          cookie.labels <- paste0(time.labels[t], ".", seq(1,cookie.counts[[t]],1))
          for(c in 1:length(cookie.labels)){
            cookies.out <- c(cookies.out, rep(cookie.labels[c], nrow(output[[t]])/cookie.counts[[t]]))
          }
        } else {
          cookies.out <- c(cookies.out, paste0(time.labels[t], ".", seq(1,cookie.counts[[t]],1)))
        }
      }
      ## reformat outputs
      output.mat <- cbind(as.numeric(times.out), cookies.out, do.call(rbind, output))
      colnames(output.mat) <- c("times", "source.subregion.ID", taxa.labels)
      rownames(output.mat) <- NULL
      ## Save output here
      write.csv(output.mat, output.dirs[d])
      }
    }
  } else {
    ## for each input.dirs
    for(d in 1:length(input.dirs)){
      ## read in data
      data <- suppressWarnings(tryCatch(readRDS(input.dirs[d]), error = function(e){}))
      ## if data is NULL, skip to next
      if(is.null(data)){
        next
      } else {
        time.labels <- names(data)
        cookie.counts <- lapply(1:length(data), function(t) length(data[[t]]))
        ## get counts of taxa
        output <- mclapply(1:length(data), mc.cores = n.cores, function(t){
            per.cookie <- as.vector(sapply(1:length(data[[t]]), function(c){
              per.rep <- sapply(1:length(data[[t]][[c]]), function(r){
                if(nrow(data[[t]][[c]][[r]]) > 0){
                  out <- length(unique(data[[t]][[c]][[r]][,taxVar]))
                } else {
                  out <- 0
                }
                return(out)
              })
          }))
        })
        names(output) <- time.labels
        ## get times column
        times.out <- c()
        for(i in 1:length(time.labels)){
          if(length(output[[i]]) > 1){
            times.out <- c(times.out,rep(time.labels[i],length(output[[i]])))
          } else {
            times.out <- c(times.out, time.labels[i])
          }
        }
        ## get cookie.id column
        cookies.out <- c()
        for(t in 1:length(time.labels)){
          if(length(output[[t]]) > 1){
            cookie.labels <- paste0(time.labels[t], ".", seq(1,cookie.counts[[t]],1))
            for(c in 1:length(cookie.labels)){
              cookies.out <- c(cookies.out, rep(cookie.labels[c], length(output[[t]])/cookie.counts[[t]]))
            }
          } else {
            cookies.out <- c(cookies.out, paste0(time.labels[t], ".", seq(1,cookie.counts[[t]],1)))
          }
        }
        ## reformat outputs
        output.mat <- cbind(as.numeric(times.out), cookies.out, unlist(output))
        colnames(output.mat) <- c("times", "source.subregion.ID", paste0(taxVar, ".count"))
        rownames(output.mat) <- NULL
        ## Save output here
        write.csv(output.mat, output.dirs[d])
      }
    }
  }
}
