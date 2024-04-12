count.viable.samples <- function(dir, pre, vars, sD, n.cores, output.dir, output.name, taxa = F){
  home <- getwd()
  ## get all combinations of labels
  combin <- expand.grid(vars)
  ## convert vars into a list of strings
  cLabs <- names(sD)
  rLabs <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
  dirs <- sapply(1:length(rLabs), function(x) paste0(dir, "/", pre, "_", rLabs[x], ".Rds"))
  ## create output object - ncol = number of time bins
  #output <- matrix(0, ncol = length(cLabs), nrow = length(rLabs))
  ## populate output object
  output <- do.call(rbind,mclapply(1:length(rLabs), mc.cores = n.cores, function(r){
    data <- readRDS(dirs[r])
    if(taxa) data <- data[[1]]
    oneCombin <- lapply(1:length(cLabs), function(c){
      if(all(is.na(data[[c]]))){
        out <- 0
      } else {
        out <- length(data[[c]])
      }
    })
  }))
  rownames(output) <- rLabs
  colnames(output) <- cLabs
  setwd(output.dir)
  write.csv(output, file = paste0(output.name, "_viable_subsamples.csv"))
  setwd(home)
}
