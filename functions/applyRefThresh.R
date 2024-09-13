applyRefThresh <- function(input.dir, input.pre, output.dir, output.pre, vars, categories, taxa = T, all.var.comb = F, threshold = 3, n.cores = 4){
  if(all.var.comb){
    combin <- expand.grid(vars)
  } else {
    combin <- data.frame(do.call(cbind, vars))
  }
  varStrings <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
  input.dirs <- paste0(input.dir, "/", input.pre, "_", varStrings, ".Rds")
  output.dirs <- paste0(output.dir, "/", output.pre, "_", varStrings, ".Rds")
  for(i in 1:length(input.dirs)){
    ## Load data
    data <- suppressWarnings(tryCatch(readRDS(input.dirs[i]), error = function(e){}))
    if(is.null(data)){
      next
    } else {
    ## Prune out missing bits
    if(taxa){
      taxon.names <- names(data)
      if(!all(taxon.names %in% names(categories))){
        stop("names of elements in 'categories' do not match names of taxa in data")
      }
      ## Re-order categories if necessary
      order <- c()
      for(o in 1:length(categories)) order <- c(order,which(taxon.names %in% names(categories)[o]))
      categories <- lapply(order, function(x) out <- categories[[x]])
      names(categories) <- taxon.names
      ## Identify which cookies need to be dropped
      per.taxa <- mclapply(1:length(data), mc.cores = n.cores, function(x){
        per.time.bin <- lapply(1:length(data[[x]]), function(y){
          per.cookie <- lapply(1:length(data[[x]][[y]]), function(z){
            ## get composition
            comp <- distinct(data[[x]][[y]][[z]][,c("reference_no","reference_comp")])
            ## get count of categories
            count <- 0
            for(c in categories[[x]]) count <- count + length(which(comp[,"reference_comp"] %in% c))
            if(count>=threshold){
              out <- data[[x]][[y]][[z]]
            } else {
              out <- NA
            }
          })
        })
        names(per.time.bin) <- names(data[[1]])
        return(per.time.bin)
      })
      ## Start with copy of data
      output <- data
      ## initialise vector to record time bins to be dropped
      timeBin.droppers <- c()
      for(ti in 1:length(per.taxa[[1]])){
        ## initialise vector to record cookies within time bins to be dropped
        droppers <- c()
        ## identify cookies with an NA in one of two categories
        for(ta in 1:length(per.taxa)){
          droppers <- c(droppers, which(is.na(per.taxa[[ta]][[ti]])))
        }
        ## refine to unique cookie IDs
        droppers <- unique(droppers)
        ## if one or more cookie IDs added to droppers
        if(length(droppers) > 0){
          ## if the number of cookies to be dropped equals number of cookies in time bins
          if(length(droppers) == length(output[[1]][[ti]])){
            timeBin.droppers <- c(timeBin.droppers,ti)
          } else {
            ## otherwise, delete cookies from each side of the object
           for(tax in 1:length(per.taxa)){
             output[[tax]][[ti]][droppers] <- NULL
           }
          }
        }
      }
      ## finally, drop time bins with nothing
      for(tax in 1:length(output)){
        output[[tax]][timeBin.droppers] <- NULL
      }
      ## Export output
      saveRDS(output, output.dirs[i])
    } else {
      ## Identify which cookies need to be dropped
        per.time.bin <- lapply(1:length(data), function(y){
          per.cookie <- lapply(1:length(data[[y]]), function(z){
            ## get composition
            comp <- distinct(data[[y]][[z]][,c("reference_no","reference_comp")])
            ## get count of categories
            count <- 0
            for(c in categories) count <- count + length(which(comp[,"reference_comp"] %in% c))
            if(count>=threshold){
              out <- data[[y]][[z]]
            } else {
              out <- NA
            }
          })
        })
        names(per.time.bin) <- names(data)
      ## Start with copy of data
      output <- data
      ## initialise vector to record time bins to be dropped
      timeBin.droppers <- c()
      for(ti in 1:length(per.time.bin)){
        ## identify cookies with an NA
        droppers <- which(is.na(per.time.bin[[ti]]))
        ## if one or more cookie IDs added to droppers
        if(length(droppers) > 0){
          ## if the number of cookies to be dropped equals number of cookies in time bins
          if(length(droppers) == length(output[[ti]])){
            timeBin.droppers <- c(timeBin.droppers,ti)
          } else {
            ## otherwise, delete cookies from each side of the object
            output[[ti]][droppers] <- NULL
          }
        }
      }
      ## finally, drop time bins with nothing
      output[timeBin.droppers] <- NULL
      ## Export output
      saveRDS(output, output.dirs[i])
    }
    }
  }
}
