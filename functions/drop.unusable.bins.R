drop.unusable.bins <- function(input.dir, input.pre, output.dir, output.pre, vars, sD, threshold = 2, taxa = F){
  combin <- expand.grid(vars)
  times <- names(sD)
  varStrings <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
  input.dirs <- paste0(input.dir, "/", input.pre, "_", varStrings, ".Rds")
  output.dirs <- paste0(output.dir, "/", output.pre, "_", varStrings, ".Rds")
  for(x in 1:length(input.dirs)){
    ## Load data
    data <- readRDS(input.dirs[x])
    ## Prune out missing bits
    if(taxa){
      ## if all time bins NA for both taxa, skip
      check <- sapply(1:length(data), function(x){
        perTaxon <- sapply(1:length(data[[x]]), function(y) all(is.na(data[[x]][[y]])))
      })
      if(all(check)){
        next
      } else {
      ## initialise drop vector
      drop.list <- list()
      ## for each element in data, identify NAs and then drop all
      for(i in 1:length(data)){
        names(data[[i]]) <- times
        drop.vector <- c()
        for(d in 1:length(data[[i]])){
          if(all(is.na(data[[i]][[d]]))){
            ## if na, add
            drop.vector <- c(drop.vector,d)
          } else {
            ## if fewer viable spatial subsamples than threshold (2 minimum)
            if(length(data[[i]][[d]]) < threshold){
              drop.vector <- c(drop.vector,d)
            }
          }
        }
        drop.list[[i]] <- drop.vector
      }
      ## get intersect of two lists
      TBD <- do.call(intersect, drop.list)
      ## if drop vector length exceeds 0, reduce to unique entries, then drop
      if(length(TBD) > 0){
        for(n in 1:length(data)){
          data[[n]] <- data[[n]][-TBD]
        }
      }
      saveRDS(data, output.dirs[x])
      }
    } else {
      check <- sapply(1:length(data), function(y) all(is.na(data[[y]])))
      if(all(check)){
        next
      } else {
      names(data) <- times
      TBD <- c()
      for(d in 1:length(data)){
        ## if fewer viable spatial subsamples than threshold
        if(all(is.na(data[[d]]))){
          TBD <- c(TBD,d)
        } else {
          if(length(data[[d]]) < threshold){
            TBD <- c(TBD,d)
          }
        }
      }
      ## if drop vector length exceeds 0, reduce to unique entries, then drop
      if(length(TBD) > 0){
        data <- data[-TBD]
      }
      saveRDS(data, output.dirs[x])
      }
    }
  }
}
