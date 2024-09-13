subsample.references <- function(input.dir, input.pre, output.dir, output.pre, vars, nRefs = 1, reps = 100, taxa = T, n.cores = 4, all.var.comb = F){
  if(all.var.comb){
    combin <- expand.grid(vars)
  } else {
    combin <- data.frame(do.call(cbind, vars))
  }
  varStrings <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
  input.dirs <- paste0(input.dir, "/", input.pre, "_", varStrings, ".Rds")
  output.dirs <- paste0(output.dir, "/", output.pre, "_", varStrings, ".Rds")
  ## for each input.dirs
  for(d in 1:length(input.dirs)){
    ## read in data
    data <- suppressWarnings(tryCatch(readRDS(input.dirs[d]), error = function(e){}))
    ## if data is NULL, skip to next
    if(is.null(data)){
      next
    } else {
      if(taxa){
        taxa.labels <- names(data)
        time.labels <- names(data[[1]])
        ## Pass over
        output <- mclapply(1:length(data), mc.cores = n.cores, function(x){
          per.bin <- lapply(1:length(data[[x]]), function(y){
            per.RCR <- lapply(1:length(data[[x]][[y]]), function(z){
              samples <- lapply(1:reps, function(all){
                ## get references
                ref.IDs <- unique(data[[x]][[y]][[z]][,"reference_no"])
                ## draw sample
                out <- data[[x]][[y]][[z]][which(data[[x]][[y]][[z]][,"reference_no"] %in% sample(ref.IDs, nRefs, replace = F)),]
              })
            })
          })
          names(per.bin) <- time.labels
          return(per.bin)
        })
        names(output) <- taxa.labels
      } else {
        time.labels <- names(data[[1]])
        ## Pass over
          output <- mclapply(1:length(data), mc.cores = n.cores, function(y){
            per.RCR <- lapply(1:length(data[[y]]), function(z){
              samples <- lapply(1:reps, function(all){
                ## get references
                ref.IDs <- unique(data[[y]][[z]][,"reference_no"])
                ## draw sample
                out <- data[[y]][[z]][which(data[[y]][[z]][,"reference_no"] %in% sample(ref.IDs, nRefs, replace = F)),]
              })
            })
          })
          names(output) <- time.labels
      }
      saveRDS(output, output.dirs[d])
    }
  }
}
