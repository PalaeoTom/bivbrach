transfer.cell.covariate <- function(source.data, export.data, cov.cols, cell.col, n.cores){
  ## Get bin names
  bin.names <- names(source.data)
  ## Pass over export data
  output <- mclapply(1:length(export.data), mc.cores = n.cores, function(x){
    if(nrow(export.data[[x]]) > 0){
      ## get cell data
      cell.covs <- distinct(source.data[[x]][,which(colnames(source.data[[x]]) %in% c(cell.col,cov.cols))])
      ## get cells present
      cells <- export.data[[x]][,cell.col]
      ## get cell covariate data
      c <- cell.covs[match(cells, cell.covs[,1]),-1]
      ## attach and export
      out <- cbind(export.data[[x]], c)
      return(out)
    } else {
      out <- export.data[[x]]
      return(out)
    }
  })
  names(output) <- bin.names
  return(output)
}
