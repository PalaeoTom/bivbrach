raw_richness <- function(data, cell = "stage_cell", taxon_name = "combined_name", count = c("Mollusca", "Brachiopoda"), n.cores = 8){
  ## Get cells
  cells <- unique(data[,cell])
  ## mclapply time
  output <- mclapply(1:length(cells), mc.cores = n.cores, function(x){
    ## Get unique combined names values to be counted
    taxa <- unique(data[which(data[,cell] %in% cells[x]),taxon_name])
    ## Create container
    out <- c()
    ## Subset vector by patterns in count argument
    for(c in count){
      out <- c(out,length(str_subset(taxa, c)))
    }
    return(out)
  })
  final <- do.call(rbind, output)
  final <- cbind(cells,final)
  colnames(final) <- c(cell,count)
  return(final)
}
