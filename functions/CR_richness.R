CR_richness <- function(data, n = 20, reps = 1000, cell = "stage_cell", taxon_name = "combined_name", count = c("Mollusca", "Brachiopoda"), summarise = "median", n.cores = 1){
  ## Get cells
  cells <- unique(data[,cell])
  ## If length of n is 1, repeat length(cells) times
  if(length(n)==1){
    n <- rep(n,length(cells))
  } else {
    ## Check that length is equal
    if(!length(n) == length(cells)){
      stop("n does not include values for every cell in data")
    } else {
      ## Rearrange order of n to match order of cells
      n <- n[match(cells,names(n))]
    }
  }
  ## mclapply time
  output <- mclapply(1:length(cells), mc.cores = n.cores, function(x){
    ## Get combined names to be rarefied
    taxa <- data[which(data[,cell] %in% cells[x]),taxon_name]
    ## take rep sample
    draws <- t(sapply(1:reps, function(y){
      ## draw sample
      samp <- unique(sample(taxa, n[x], replace = F))
      ## Create container
      out <- c()
      ## Subset vector by patterns in count argument
      for(c in count){
        out <- c(out,length(str_subset(samp, c)))
      }
      return(out)
    }))
    ## Include switch for median and mean
    if(summarise == "median"){
      out <- apply(draws,2,function(z) median(z))
    } else {
      if(summarise == "mean"){
        out <- apply(draws,2,function(z) mean(z))
      } else {
        stop("summarise is not median or mean")
      }
    }
  })
  final <- do.call(rbind, output)
  final <- cbind(cells,final)
  colnames(final) <- c(cell,count)
  return(final)
}
