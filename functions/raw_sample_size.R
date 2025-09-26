raw_sample_size <- function(data, cell = "stage_cell", n.cores = 8){
  ## Get cells
  cells <- unique(data[,cell])
  ## mclapply time
  output <- mclapply(1:length(cells), mc.cores = n.cores, function(x){
    ## Get number of taxa in cell
    n <- length(which(data[,cell] %in% cells[x]))
  })
  final <- do.call(rbind, output)
  final <- cbind(cells,as.numeric(final))
  colnames(final) <- c(cell,"n")
  return(final)
}
