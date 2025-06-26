label.cell.reference.combo.comps <- function(data, n.cores){
  times <- names(data)
  output <- mclapply(1:length(data), mc.cores = n.cores, function(x){
    if(nrow(data[[x]]) > 0){
      ## get grid cells
      GCs <- unique(data[[x]][,"cell"])
      ## loop over each grid cell
      out <- lapply(1:length(GCs), function(y){
        gc <- data[[x]][which(data[[x]][,"cell"] %in% GCs[y]),]
        gc <- add.reference.IDs(gc)
      })
      final <- do.call(rbind, out)
    } else {
      final <- data[[x]]
    }
    return(final)
  })
  names(output) <- times
  return(output)
}
