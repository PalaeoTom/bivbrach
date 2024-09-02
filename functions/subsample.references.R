subsample.references <- function(input.dir, input.pre, output.dir, output.pre, vars, categories, nRefs = 1, taxa = T, n.cores = 4){
  combin <- expand.grid(vars)
  varStrings <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
  input.dirs <- paste0(input.dir, "/", input.pre, "_", varStrings, ".Rds")
  output.dirs <- paste0(output.dir, "/", output.pre, "_", varStrings, ".Rds")
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
        ## Pass over
        output <- mclapply(1:length(data), mc.cores = n.cores, function(x){
          per.bin <- lapply(1:length(data[[x]]), function(y){
            per.RCR <- lapply(1:length(data[[x]][[y]]), function(z){
              per.samples <- lapply(1:length(data[[x]][[y]][[z]]), function(s){
                sampled.refs <- sample(unique(data[[x]][[y]][[z]][[s]][,c("reference_no")]), nRefs, replace = F)
                out <- data[[x]][[y]][[z]][[s]][data[[x]][[y]][[z]][[s]][,"reference_no"] %in% unique(sampled.refs),]
              })
            })
          })
          names(per.bin) <- time.labels
          return(per.bin)
        })
        names(output) <- taxa.labels
        saveRDS(output, output.dirs[d])
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
        ## Pass over
        output <- mclapply(1:length(data), function(y){
          per.RCR <- lapply(1:length(data[[y]]), function(z){
            per.samples <- lapply(1:length(data[[y]][[z]]), function(s){
              references <- distinct(data[[y]][[z]][[s]][,c("reference_no","reference_comp")])
              sampled.refs <- c()
              for(i in 1:length(categories)){
                sampled.refs <- c(sampled.refs,sample(references[which(references[,2] %in% categories[[i]]),1], nRefs, replace=F))
              }
              out <- data[[y]][[z]][[s]][data[[y]][[z]][[s]][,"reference_no"] %in% unique(sampled.refs),]
            })
          })
        })
        names(output) <- time.labels
        saveRDS(output, output.dirs[d])
      }
    }
  }
}
