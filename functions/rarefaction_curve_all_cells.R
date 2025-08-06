rarefaction_curve_all_cells <- function(data, cell, taxVar, iter){
  ## Isolate occs
  occs <- data[,taxVar]
  ## Isolate cells
  cells <- data[,cell]
  ## Get unique cells
  cells_u <- unique(cells)
  ## Loop for each cell
  allCells <- sapply(1:length(cells_u), function(c){
    samp <- occs[which(cells %in% cells_u[c])]
    curve <- rarefaction_curve(samp, iter)
  })
  ## Homogenise lengths
  max_n <- max(sapply(1:length(allCells), function(z) length(allCells[[z]])))
  allCells_s <- sapply(1:length(allCells), function(z){
    out <- allCells[[z]]
    length(out) <- max_n
    return(out)
  })
  sampleN <- 1:nrow(allCells_s)
  output <- data.frame(cbind("sampled" = sampleN, allCells_s))
  colnames(output) <- c("sampled",cells_u)
  return(output)
}
