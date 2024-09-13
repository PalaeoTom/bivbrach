count.viable.samples <- function(dir, pre, vars, sD, n.cores, output.dir, output.name, taxa = F, all.var.comb = F){
  home <- getwd()
  ## get all combinations of labels
  if(all.var.comb){
    combin <- expand.grid(vars)
  } else {
    combin <- data.frame(do.call(cbind, vars))
  }
  ## convert vars into a list of strings
  cLabs <- names(sD)
  rLabs <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
  dirs <- sapply(1:length(rLabs), function(x) paste0(dir, "/", pre, "_", rLabs[x], ".Rds"))
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
